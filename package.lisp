#|
 This file is a part of Modularize-Hooks
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:modularize-hooks
  (:use #:cl #:modularize #:lambda-fiddle)
  (:nicknames #:org.shirakumo.radiance.lib.modularize.hooks #:hooks)
  ;; hooks.lisp
  (:export
   #:hookify
   #:dehookify
   #:hook
   #:name
   #:arglist
   #:docstring
   #:list-hooks
   #:define-hook
   #:remove-hook
   #:trigger
   #:define-trigger
   #:remove-trigger
   #:sticky-hook
   #:define-hook-switch))
