#|
 This file is a part of Reader
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem #:reader
  :defsystem-depends-on (:radiance)
  :class "radiance:module"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :version "1.1.2"
  :license "Artistic"
  :description "A simple blogging platform for Radiance."
  :homepage "https://github.com/Shinmera/reader"
  :components ((:file "reader")
               (:file "atom"))
  :depends-on ((:interface :database)
               (:interface :data-model)
               (:interface :auth)
               (:interface :cache)
               (:interface :profile)
               :local-time
               :r-clip
               :3bmd
               :3bmd-ext-code-blocks
               :cl-ppcre
               :do-urlencode))
