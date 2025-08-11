(asdf:defsystem #:reader
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "1.2.0"
  :license "zlib"
  :description "A simple blogging platform for Radiance."
  :homepage "https://shinmera.com/docs/reader/"
  :bug-tracker "https://shinmera.com/project/reader/issues"
  :source-control (:git "https://shinmera.com/project/reader.git")
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
               :cl-markless-plump
               :cl-ppcre
               :do-urlencode))
