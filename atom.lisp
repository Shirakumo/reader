#|
 This file is a part of Reader
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:reader)

(defvar *entry-count* 10)

(defun atom-time (time)
  (local-time:format-timestring
   NIL (local-time:universal-to-timestamp time)))

(defun atom-url (tag)
  (external-pattern "/api/reader/atom?tag={0}" tag))

(define-api reader/atom (&optional tag) ()
  (let ((tag (and tag (sanitize-tag tag))))
    (prog1
        (cache-wrapper ("READER-ATOM-~a" tag)
          (format
           NIL "<?xml version=\"1.0\" encoding=\"utf-8\"?>~%~a"
           (lquery-wrapper ("atom.ctml")
             (let ((articles (mapc
                              #'(lambda (article)
                                  (let ((author (user:get (dm:field article "author"))))
                                    (setf (dm:field article "homepage") (user:field author "homepage")
                                          (dm:field article "email") (user:field author "email"))))
                              (dm:get 'reader-articles
                                      (if tag
                                          (db:query (:matches 'tags (query-tag tag)))
                                          (db:query :all)) :amount *entry-count* :sort '((time :DESC))))))
               (r-clip:process (lquery:$ (node))
                               :updated (if articles (dm:field (first articles) "time") -1)
                               :articles articles
                               :tag (when tag (urlencode:urlencode tag))
                               :domain (domain *request*)
                               :title (config-tree :reader :title)
                               :description (config-tree :reader :description))))))
      (setf (content-type *response*) "application/atom+xml"))))
