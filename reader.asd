#|
 This file is a part of Reader
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem #:reader
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :version "1.1.2"
  :license "Artistic"
  :description "A simple blogging platform for Radiance."
  :homepage "https://Shinmera.github.io/reader/"
  :bug-tracker "https://github.com/Shinmera/reader/issues"
  :source-control (:git "https://github.com/Shinmera/reader.git")
  :serial T
  :components ((:file "module")
               (:file "db")
               (:file "cache")
               (:file "reader")
               (:file "atom"))
  :depends-on ((:interface :database)
               (:interface :auth)
               (:interface :profile)
               :r-data-model
               :local-time
               :r-clip
               :3bmd
               :3bmd-ext-code-blocks
               :cl-ppcre
               :do-urlencode))
