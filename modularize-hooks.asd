(asdf:defsystem modularize-hooks
  :name "Modularize-Hooks"
  :version "1.0.2"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Generic hooks and triggers extension for modularize."
  :homepage "https://shinmera.com/docs/modularize-hooks/"
  :bug-tracker "https://shinmera.com/project/modularize-hooks/issues"
  :source-control (:git "https://shinmera.com/project/modularize-hooks.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "hooks"))
  :depends-on (:modularize
               :closer-mop
               :trivial-arguments
               :lambda-fiddle))
