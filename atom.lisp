#|
 This file is a part of Reader
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:reader)

(defvar *entry-count* 10)

(defun recache-atom (&optional tag)
  (let ((articles (mapc
                   (lambda (article)
                     (let ((author (user:get (dm:field article "author"))))
                       (setf (dm:field article "homepage") (user:field "homepage" author)
                             (dm:field article "email") (user:field "email" author))))
                   (dm:get 'articles
                           (if tag
                               (db:query (:matches 'tags (query-tag tag)))
                               (db:query :all)) :amount *entry-count* :sort '((time :DESC))))))
    (with-template-to-cache ((cache-file :atom tag) "atom.ctml")
      (r-clip:process
       T
       :updated (if articles (dm:field (first articles) "time") -1)
       :articles articles
       :tag (when tag (urlencode:urlencode tag))
       :domain (if (boundp '*request*) (domain *request*) (first (mconfig :radiance-core :domains)))
       :title (config :title)
       :description (config :description)))))

(define-trigger (article-updated 'reader-atom) (article)
  (recache-atom)
  (dolist (tag (article-tags article))
    (recache-atom tag)))

(define-trigger (article-deleted 'reader-atom) (article)
  (recache-atom)
  (dolist (tag (article-tags article))
    (recache-atom tag)))

(define-trigger (recache-all 'reader-atom) (articles)
  (let ((tags ()))
    (dolist (article articles)
      (dolist (tag (article-tags article))
        (pushnew tag tags :test #'string-equal)))
    (format T "~&Recaching atom feeds (~d)~%" (length tags))
    (dolist (tag tags)
      (recache-atom tag))
    (recache-atom)))

(define-api reader/atom (&optional tag) ()
  (let ((tag (and tag (sanitize-tag tag))))
    (show-cache :atom tag)
    (setf (content-type *response*) "application/atom+xml; charset=utf-8")
    NIL))
