(asdf:defsystem modularize-hooks
  :name "Modularize-Hooks"
  :version "1.0.2"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Generic hooks and triggers extension for modularize."
  :homepage "https://Shinmera.github.io/modularize-hooks/"
  :bug-tracker "https://github.com/Shinmera/modularize-hooks/issues"
  :source-control (:git "https://github.com/Shinmera/modularize-hooks.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "hooks"))
  :depends-on (:modularize
               :closer-mop
               :trivial-arguments
               :lambda-fiddle))
