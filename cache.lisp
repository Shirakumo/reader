#|
 This file is a part of Reader
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:reader)

(defvar *cache* (merge-pathnames "cache/" (mconfig-pathname #.*package*)))
(defvar *article-contents* (make-hash-table :test 'eql))
(defparameter *app* 25)

(defun parse (text)
  (ecase (config :markup)
    (:plain
     (let ((root (plump:make-root)))
       (plump:make-text-node root text)
       root))
    (:html
     (plump:parse text))
    (:markdown
     (let ((3bmd:*smart-quotes* T)
           (3bmd-code-blocks:*code-blocks* T))
       (plump:parse
        (with-output-to-string (string)
          (3bmd:parse-string-and-print-to-stream text string)))))
    (:markless
     (cl-markless:output
      text
      :target (plump-dom:make-root)
      :format 'cl-markless-plump:plump))))

(defun article-content (article)
  (let ((article (ensure-article article)))
    (or (gethash (dm:id article) *article-contents*)
        (setf (gethash (dm:id article) *article-contents*)
              (parse (dm:field article "text"))))))

(defun article-excerpt (article)
  (let* ((article (ensure-article article))
         (lquery:*lquery-master-document* (article-content article)))
    (lquery:$1 "p")))

(defun article-image (article)
  (let* ((article (ensure-article article))
         (lquery:*lquery-master-document* (article-content article)))
    (lquery:$1 "img" (attr :src))))

(defun article-description (article)
  (let* ((article (ensure-article article))
         (lquery:*lquery-master-document* (article-content article)))
    (lquery:$1 "p" (text))))

(defun sanitize-tag (tag)
  (string-trim " " (cl-ppcre:regex-replace-all "[\\[\\]\\(\\)\\{\\}\\$\\^\\\\\\|\\*,/]" tag "")))

(defun query-tag (tag)
  (format NIL "(^|,)\\s*~a\\s*(,|$)"
          (cl-ppcre:regex-replace-all "([.+*])" tag "\\\\\\1")))

(defun article-tags (article)
  (let ((article (ensure-article article)))
    (mapcar #'sanitize-tag (cl-ppcre:split "," (dm:field article "tags")))))

(defun cache-file (type thing &optional page)
  (merge-pathnames
   (format NIL "~a/~a~@[/~a~].html" type thing page)
   *cache*))

(defun show-cache (type thing &optional page)
  (let ((file (cache-file type thing page)))
    (unless (probe-file file)
      (case type
        (:index (recache-index))
        (:article (recache-article thing))
        (:tag (recache-tag thing))
        (:atom (recache-atom thing))))
    (serve-file file "application/xhtml+xml; charset=utf-8"))
  NIL)

(defun partition (set n)
  (let ((partitions ()))
    (loop with partition = ()
          for item in set
          for i from 1
          do (push item partition)
             (when (= (mod i n) 0)
               (push (nreverse partition) partitions)
               (setf partition ()))
          finally (when partition
                    (push (nreverse partition) partitions)))
    (nreverse partitions)))

(defun call-with-template-to-cache (file template function)
  (let ((temp (merge-pathnames (make-pathname :type "tmp") file)))
    (ensure-directories-exist file)
    (restart-case
        (progn
          (with-open-file (stream temp :direction :output :if-exists :supersede)
            (let ((r-clip:*document* (plump:parse (@template template))))
              (funcall function)
              (let ((plump:*tag-dispatchers* plump:*xml-tags*))
                (plump:serialize r-clip:*document* stream))))
          (uiop:rename-file-overwriting-target temp file))
      (stub ()
        :report "Create a stub file instead."
        (with-open-file (stream file :direction :output :if-exists :supersede)
          (format stream "<div class=\"error\">Error generating cache!<br />~a</div>" file)))
      (bail ()
        :report "Bail out, potentially leaving no cache file behind (dangerous)."))))

(defmacro with-template-to-cache ((file template) &body body)
  `(call-with-template-to-cache ,file ,template (lambda () ,@body)))

(defun starts-with (string subs)
  (and (<= (length subs) (length string))
       (string-equal subs string :end2 (length subs))))

(defun find-series (tag cid)
  (let ((first (dm:get-one 'articles (db:query (:matches 'tags (query-tag tag))) :sort '(("_id" :ASC))))
        (next (dm:get-one 'articles (db:query (:and (:matches 'tags (query-tag tag)) (:> '_id cid))) :sort '(("_id" :ASC))))
        (prev (dm:get-one 'articles (db:query (:and (:matches 'tags (query-tag tag)) (:< '_id cid))) :sort '(("_id" :DESC)))))
    (list :title (subseq tag 2)
          :first first
          :next next
          :prev prev)))

(defun series (tags cid)
  (loop for tag in tags
        when (starts-with tag "s:")
        collect (find-series tag cid)))

(defun recache-index ()
  (let* ((articles (dm:get 'articles (db:query :all) :sort '((time :DESC))))
         (pages (partition articles *app*)))
    (loop for page in (or pages '(()))
          for index from 0
          do (with-template-to-cache ((cache-file :index index) "index.ctml")
               (r-clip:process
                T
                :articles page
                :page (1+ index)
                :has-more (< index (1- (length pages)))
                :title (config :title)
                :description (config :description))))
    articles))

(defun recache-tag (tag)
  (let* ((articles (dm:get 'articles (db:query (:matches 'tags (query-tag tag))) :sort '((time :DESC))))
         (pages (partition articles *app*)))
    (loop for page in pages
          for index from 0
          do (with-template-to-cache ((cache-file :tag tag index) "tagged.ctml")
               (r-clip:process
                T
                'radiance::tag tag
                :articles page
                :page (1+ index)
                :has-more (< index (1- (length pages)))
                :title (config :title)
                :description (config :description))))
    articles))

(defun recache-article (article &optional recache-content)
  (let* ((article (ensure-article article))
         (next (dm:get-one 'articles (db:query (:> '_id (dm:id article))) :sort '(("_id" :ASC))))
         (prev (dm:get-one 'articles (db:query (:< '_id (dm:id article))) :sort '(("_id" :DESC)))))
    (when recache-content (setf (gethash (dm:id article) *article-contents*) NIL))
    (with-template-to-cache ((cache-file :article (dm:id article)) "article.ctml")
      (r-clip:process
       T
       :article article
       :next next
       :prev prev
       :links (dm:get 'links (db:query :all))
       :title (config :title)
       :description (config :description)))
    article))

(defun recache-all ()
  (trigger 'recache-all (dm:get 'articles (db:query :all) :sort '((time :DESC)))))

(define-trigger (recache-all 'reader-cache) (articles)
  (let ((tags ()))
    (format T "~&Recaching articles (~d)~%" (length articles))
    (loop for article in articles
          for i from 1
          do (when (= 0 (mod i 50))
               (format T "~&..~d~%" i))
             (recache-article article T)
             (dolist (tag (article-tags article))
               (pushnew tag tags :test #'string-equal)))
    (format T "~&Recaching tags (~d)~%" (length tags))
    (loop for tag in tags
          for i from 1
          do (when (= 0 (mod i 50))
               (format T "~&..~d~%" i))
             (recache-tag tag))
    (format T "~&Recaching index~%")
    (recache-index)))

(define-trigger (article-updated 'reader-cache) (article)
  (recache-article article T)
  (recache-index)
  (dolist (tag (article-tags article))
    ;; Recache tagged pages to refresh the prev/next links.
    (dolist (article (recache-tag tag))
      (recache-article article))))

(define-trigger (article-deleted 'reader-cache) (article)
  (uiop:delete-file-if-exists (cache-file :article (dm:id article)))
  (recache-index)
  (dolist (tag (article-tags article))
    (recache-tag tag)))
