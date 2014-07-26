#|
 This file is a part of Modularize-Hooks
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.lib.modularize.hooks
  (:use #:cl #:modularize)
  (:nicknames #:modularize-hooks #:hooks)
  ;; hooks.lisp
  (:export
   #:hookify
   #:dehookify
   #:define-hook
   #:remove-hook
   #:trigger
   #:define-trigger
   #:remove-trigger))
