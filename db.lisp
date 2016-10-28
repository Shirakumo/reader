#|
 This file is a part of Reader
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:reader)

(define-trigger db:connected ()
  (db:create 'reader-articles '((title (:varchar 64)) (text :text) (author (:varchar 32)) (time (:integer 5)) (tags :text)))
  (db:create 'reader-links '((title (:varchar 32)) (url (:varchar 128)))))

(defgeneric ensure-article (article)
  (:method ((id integer))
    (or (dm:get-one 'reader-articles (db:query (:= '_id id)))
        (error "No post with ID ~d found." id)))
  (:method ((id string))
    (ensure-article (parse-integer id)))
  (:method ((article dm:data-model))
    article))

(define-hook article-updated (article))
(define-hook article-deleted (article))
(define-hook recache-all (articles))

(define-trigger startup-done ()
  (defaulted-config "Untitled" :title)
  (defaulted-config "An unconfigured blog." :description))
