#|
 This file is a part of Reader
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:reader)

(define-trigger db:connected ()
  (db:create 'articles '((title (:varchar 64)) (text :text) (author (:varchar 32)) (time (:integer 5)) (tags :text) (format (:integer 1))))
  (db:create 'links '((title (:varchar 32)) (url (:varchar 128)))))

(defun ensure-article (article)
  (typecase article
    (dm:data-model article)
    (db:id (or (dm:get-one 'articles (db:query (:= '_id article)))
               (error "No post with ID ~d found." article)))
    (T (ensure-article (db:ensure-id article)))))

(define-hook article-updated (article))
(define-hook article-deleted (article))
(define-hook recache-all (articles))

(define-trigger startup-done ()
  (defaulted-config "Untitled" :title)
  (defaulted-config "An unconfigured blog." :description)
  (defaulted-config :markdown :markup))
