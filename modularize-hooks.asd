#|
 This file is a part of Modularize-Interfaces
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem modularize-hooks
  :name "Modularize-Hooks"
  :version "1.0.2"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Generic hooks and triggers extension for modularize."
  :homepage "https://github.com/Shinmera/modularize-hooks"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "hooks"))
  :depends-on (:modularize
               :closer-mop
               :trivial-arguments
               :lambda-fiddle))
