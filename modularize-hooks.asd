#|
 This file is a part of Modularize-Interfaces
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.lib.modularize.hooks.asdf
  (:use #:cl #:asdf))
(in-package :org.tymoonnext.radiance.lib.modularize.hooks.asdf)

(defsystem modularize-hooks
  :name "Modularize-Hooks"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Generic hooks and triggers extension for modularize."
  :long-description ""
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "hooks"))
  :depends-on (:modularize
               :closer-mop))
