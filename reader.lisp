#|
 This file is a part of Reader
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module reader
  (:use #:cl #:radiance)
  (:shadow #:write))
(in-package #:reader)

;;; DB-SETUP
(define-trigger db:connected ()
  (db:create 'reader-articles '((title (:varchar 64)) (text :text) (author (:varchar 32)) (time (:integer 5)) (tags :text)))
  (db:create 'reader-links '((title (:varchar 32)) (url (:varchar 128)))))

;;; HELPER FUNCTIONS
(defun starts-with (string subs)
  (and (<= (length subs) (length string))
       (string-equal subs string :end2 (length subs))))

(defun find-series (tag cid)
  (let ((first (dm:get-one 'reader-articles (db:query (:matches 'tags (query-tag tag))) :sort '(("_id" :ASC))))
        (next (dm:get-one 'reader-articles (db:query (:and (:matches 'tags (query-tag tag)) (:> '_id cid))) :sort '(("_id" :ASC))))
        (prev (dm:get-one 'reader-articles (db:query (:and (:matches 'tags (query-tag tag)) (:< '_id cid))) :sort '(("_id" :DESC)))))
    (list :title (subseq tag 2)
          :first first
          :next next
          :prev prev)))

(defun series (tags cid)
  (loop for tag in tags
        when (starts-with tag "s:")
          collect (find-series tag cid)))

(defun article-url (id)
  (external-pattern "reader/article/{0}" id))

(defun tag-url (tag)
  (external-pattern "reader/tagged/{0}" tag))

(defparameter *time-format* '((:year 4) #\. (:month 2) #\. (:day 2)))
(defun format-time (time)
  (local-time:format-timestring NIL (local-time:universal-to-timestamp time) :format *time-format*))

(defun parse (text)
  (let ((3bmd:*smart-quotes* T)
        (3bmd-code-blocks:*code-blocks* T))
    (with-output-to-string (string)
      (3bmd:parse-string-and-print-to-stream text string))))

(defun excerpt (text)
  (let ((lquery:*lquery-master-document*))
    (format NIL "~{~a~}" (coerce (lquery:$ (initialize (parse text))
                                   "p:first-only" (serialize)) 'list))))

(defun sanitize-tag (tag)
  (string-trim " " (cl-ppcre:regex-replace-all "[\\[\\]\\(\\)\\{\\}\\$\\^\\\\\\|\\*\\.,/]" tag "")))

(defun query-tag (tag)
  (format NIL "(^|,)\\s*~a\\s*(,|$)" tag))

;;; PAGE
(defparameter *app* 25)
(defvar *cache-validated* ())

(defmacro cache-wrapper ((cache-format-string &rest format-args) &body body)
  (let ((cache-var (gensym "CACHE-NAME")))
    `(let ((,cache-var (intern (format NIL ,cache-format-string ,@format-args) "READER")))
       (cache:with-cache ,cache-var
           (not (member ,cache-var *cache-validated*))
         (push ,cache-var *cache-validated*)
         ,@body))))

(defmacro lquery-wrapper ((template) &body body)
  `(let ((lquery:*lquery-master-document* (lquery:load-page (template ,template))))
     ,@body
     (setf (content-type *response*) "application/xhtml+xml")
     (lquery:$ (serialize) (node))))

(define-page index #@"reader/" ()
  (cache-wrapper ("READER-INDEX")
    (lquery-wrapper ("index.ctml")
      (let* ((articles (dm:get 'reader-articles (db:query :all) :sort '(("time" :DESC)) :amount *app*))
             (count (db:count 'reader-articles (db:query :all))))
        (r-clip:process (lquery:$ (node))
                        :articles articles
                        :page 1
                        :has-more (< *app* count)
                        :title (config-tree :reader :title)
                        :description (config-tree :reader :description))))))

(define-page page #@"reader/page/([0-9]*)" (:uri-groups (page))
  (let ((page (1- (or (parse-integer (or (get-var "page") page) :junk-allowed T) 1))))
    (cache-wrapper ("PAGE-~a" page)
      (lquery-wrapper ("index.ctml")
        (let* ((articles (dm:get 'reader-articles (db:query :all) :sort '(("time" :DESC)) :amount *app* :skip (* *app* page)))
               (count (db:count 'reader-articles (db:query :all))))
          (r-clip:process (lquery:$ (node))
                          :articles articles
                          :page (1+ page)
                          :has-more (< (* *app* page) count)
                          :title (config-tree :reader :title)
                          :description (config-tree :reader :description)))))))

(define-page article #@"reader/article/(([0-9]+)(-.*)?)?" (:uri-groups (NIL id))
  (let ((id (or (parse-integer (or (get-var "id") id "") :junk-allowed T) -1)))
    (cache-wrapper ("ARTICLE-~a" id)
      (lquery-wrapper ("article.ctml")
        (let* ((article (dm:get-one 'reader-articles (db:query (:= '_id id))))
               (next (dm:get-one 'reader-articles (db:query (:> '_id id)) :sort '(("_id" :ASC))))
               (prev (dm:get-one 'reader-articles (db:query (:< '_id id)) :sort '(("_id" :DESC)))))
          (unless article
            (error 'request-not-found :message "No such article found."))
          (setf (dm:field article "tags") (cl-ppcre:split "\\s*,\\s*" (dm:field article "tags")))

          (r-clip:process (lquery:$ (node))
                          :article article
                          :next next
                          :prev prev
                          :links (dm:get 'reader-links (db:query :all))
                          :title (config-tree :reader :title)
                          :description (config-tree :reader :description)))))))

(define-page tag #@"reader/tagged/([^/]*)(/([0-9]+))?" (:uri-groups (tag NIL page))
  (let ((tag (sanitize-tag tag))
        (page (1- (or (parse-integer (or page "") :junk-allowed T) 1))))
    (cache-wrapper ("TAG-~a-~a" tag page)
      (lquery-wrapper ("tagged.ctml")
        (let ((articles (dm:get 'reader-articles (db:query (:matches 'tags (query-tag tag))) :sort '(("time" :DESC)) :amount *app* :skip (* *app* page)))
              (count (db:count 'reader-articles (db:query (:matches 'tags (query-tag tag))))))
          (r-clip:process (lquery:$ (node))
                          'radiance::tag tag
                          :articles articles
                          :page (1+ page)
                          :has-more (< (* *app* page) count)
                          :title (config-tree :reader :title)
                          :description (config-tree :reader :description)))))))

(define-page write #@"reader/write/([0-9]*)" (:uri-groups (id) :lquery (template "write.ctml") :access (perm reader write))
  (let* ((id (or (parse-integer (or (post/get "id") id) :junk-allowed T) -1))
         (article (or (dm:get-one 'reader-articles (db:query (:= '_id id))) (dm:hull 'reader-articles)))
         (action (or (post-var "action") "noop"))
         (message))
    (cond
      ((string-equal action "save")
       (setf (dm:field article "text") (post-var "text")
             (dm:field article "title") (post-var "title")
             (dm:field article "tags") (format NIL "~{~a~^, ~}" (mapcar #'sanitize-tag (cl-ppcre:split "," (post-var "tags")))))
       (cond
         ((dm:hull-p article)
          (setf (dm:field article "time") (get-universal-time)
                (dm:field article "author") (user:username (auth:current)))
          (dm:insert article)
          (setf *cache-validated* NIL)
          (setf message (format NIL "Article <a href=\"~a\">created</a>!" (article-url (dm:id article)))))
         (t
          (dm:save article)
          (setf *cache-validated* NIL)
          (setf message (format NIL "Article <a href=\"~a\">updated</a>!" (article-url (dm:id article)))))))
      
      ((string-equal action "delete")
       (when (dm:hull-p article)
         (error 'radiance-error :message (format NIL "No such article to delete.")))
       (dm:delete article)
       (setf article (dm:hull 'reader-articles))
       (setf *cache-validated* NIL)
       (setf message "Article deleted."))
      
      ((string-equal action "noop"))
      
      (T (error 'radiance-error :message (format NIL "Unknown action ~a." action))))
    (if message
        (lquery:$ "#message" (html message))
        (lquery:$ "#message" (remove)))
    (r-clip:process (lquery:$ (node))
                    :article article
                    :message message
                    :title (config-tree :reader :title)
                    :description (config-tree :reader :description))))

(define-page web-fonts (#@"/static/reader/wf/(.+)" 1001) (:uri-groups (path))
  (setf (header "Cache-Control") "public, max-age=31536000")
  (setf (header "Access-Control-Allow-Origin") (string-right-trim "/" (uri-to-url #@"reader/" :representation :external)))
  (serve-file (static-file (format NIL "wf/~a" path))))
