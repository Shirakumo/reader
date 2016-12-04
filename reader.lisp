#|
 This file is a part of Reader
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:reader)

(define-page index "reader/" ()
  (unless (probe-file (cache-file :index 0))
    (recache-index))
  (show-cache :index 0))

(define-page page "reader/page/([0-9]*)" (:uri-groups (page))
  (let ((page (1- (or (parse-integer (or (get-var "page") page) :junk-allowed T) 1))))
    (show-cache :index page)))

(define-page article "reader/article/(([0-9]+)(-.*)?)?" (:uri-groups (NIL id))
  (let ((id (or (parse-integer (or (get-var "id") id "") :junk-allowed T) -1)))
    (show-cache :article id)))

(define-page tag "reader/tagged/([^/]*)(/([0-9]+))?" (:uri-groups (tag NIL page))
  (let ((tag (sanitize-tag tag))
        (page (1- (or (parse-integer (or page "") :junk-allowed T) 1))))
    (show-cache :tag tag page)))

(define-page write "reader/write/([0-9]*)" (:uri-groups (id) :lquery (@template "write.ctml") :access (perm reader write))
  (let* ((id (or (parse-integer (or (post/get "id") id) :junk-allowed T) -1))
         (article (or (dm:get-one 'reader-articles (db:query (:= '_id id))) (dm:hull 'reader-articles)))
         (action (or (post-var "action") "noop"))
         (message))
    (unless (or (= id -1)
                (string= (dm:field article "author") (user:username (auth:current)))
                (user:check (auth:current) (perm reader admin)))
      (error 'request-denied :message "You do not have the permission to edit this post."))
    (cond
      ((string-equal action "save")
       (let ((tags (mapcar #'sanitize-tag (cl-ppcre:split "," (post-var "tags")))))
         (setf (dm:field article "text") (post-var "text")
               (dm:field article "title") (post-var "title")
               (dm:field article "tags") (format NIL "~{~a~^, ~}" tags))
         (cond
           ((dm:hull-p article)
            (setf (dm:field article "time") (get-universal-time)
                  (dm:field article "author") (user:username (auth:current)))
            (dm:insert article)
            (setf message (format NIL "Article <a href=\"~a\">created</a>!"
                                  (uri-to-url (format NIL "reader/article/~a" (dm:id article)) :representation :external))))
           (t
            (dm:save article)
            (setf message (format NIL "Article <a href=\"~a\">updated</a>!"
                                  (uri-to-url (format NIL "reader/article/~a" (dm:id article)) :representation :external)))))
         ;; Recache necessary parts
         (trigger 'article-updated article)))
      
      ((string-equal action "delete")
       (when (dm:hull-p article)
         (error 'radiance-error :message (format NIL "No such article to delete.")))
       (dm:delete article)
       (trigger 'article-deleted article)
       (setf article (dm:hull 'reader-articles))
       (setf message "Article deleted."))
      
      ((string-equal action "noop"))
      
      (T (error 'radiance-error :message (format NIL "Unknown action ~a." action))))
    (if message
        (lquery:$ "#message" (html message))
        (lquery:$ "#message" (remove)))
    (r-clip:process (lquery:$ (node))
                    :article article
                    :message message
                    :title (config :title)
                    :description (config :description))))

(define-page web-fonts ("/static/reader/wf/(.+)" 1001) (:uri-groups (path))
  (setf (header "Cache-Control") "public, max-age=31536000")
  (setf (header "Access-Control-Allow-Origin") (string-right-trim "/" (uri-to-url "reader/" :representation :external)))
  (serve-file (@static (format NIL "wf/~a" path))))
