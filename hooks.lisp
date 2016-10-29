#|
 This file is a part of Modularize-Hooks
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.lib.modularize.hooks)

(defun enlist (a &rest args)
  (if (listp a) a (list* a args)))

(defun hookify (&optional (module *package*))
  "Turns the module into one capable of hooks.

In essence this merely defines a new package with a matching name."
  (unless (module-storage module 'hooks)
    (setf (module-storage module 'hooks)
          (make-hash-table :test 'eql))))

(defun dehookify (&optional (module *package*))
  "Returns the module to one that is not capable of hooks.

In essence this merely removes all functions from the hooks package
and deletes it."
  (setf (module-storage module 'hooks) NIL))

(define-modularize-hook (module)
  (hookify module))

(define-delete-hook (module)
  (dehookify module))

(defclass hook ()
  ((name :initarg :name :accessor name)
   (arglist :initarg :arglist :accessor arglist)
   (triggers :initform (make-hash-table :test 'eql) :accessor triggers)
   (docstring :initarg :docstring :accessor docstring))
  (:default-initargs
   :name (error "NAME required.")
   :arglist ()
   :docstring NIL))

(defmethod print-object ((hook hook) stream)
  (print-unreadable-object (hook stream :type T)
    (format stream "~a" (name hook))))

(defun list-hooks (&optional (module *package*))
  (loop for k being the hash-keys of (module-storage module 'hooks)
        collect k))

(defun hook (name &optional (module *package*) error)
  (or (gethash name (module-storage module 'hooks))
      (and error (error "No such hook ~s found in ~a." name module))))

(defun (setf hook) (hook name &optional (module *package*))
  (setf (gethash name (module-storage module 'hooks)) hook))

(defun remove-hook (name &optional (module *package*))
  "Removes the hook as named."
  (remhash name (module-storage module 'hooks)))

(defmacro define-hook (name args &optional documentation)
  "Defines a new hook on which triggers can be defined.
The name should be a symbol from the module that the hook should belong to."
  (destructuring-bind (name &optional (module (symbol-package name)) (class 'hook)) (enlist name)
    (let ((hookg (gensym "HOOK"))
          (initargs `(:arglist ',args
                      :docstring ,documentation)))
      ;; Make sure it's available at macro-expansion time to allow
      ;; defining triggers at the same time as hooks.
      (unless (hook name module)
        (setf (hook name module) (make-instance class :name name
                                                      :arglist args
                                                      :docstring documentation)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (let ((,hookg (hook ',name ,module)))
           (cond ((typep ,hookg ',class)
                  (change-class ,hookg ',class ,@initargs))
                 (,hookg
                  (reinitialize-instance ,hookg ,@initargs))
                 (T
                  (setf (hook ',name ,module)
                        (make-instance ',class :name ',name ,@initargs)))))
         ',name))))

(defmethod run-triggers ((hook hook) args)
  (with-simple-restart (abort "Abort triggering ~s." (name hook))
    (loop for name being the hash-keys of (triggers hook)
          for function being the hash-values of (triggers hook)
          do (with-simple-restart (continue "Skip triggering ~s." name)
               (apply function args)))))

(defmethod find-trigger (name (hook hook))
  (gethash name (triggers hook)))

(defmethod (setf find-trigger) ((function function) name (hook hook))
  (setf (gethash name (triggers hook)) function))

(defmethod (setf find-trigger) ((null null) name (hook hook))
  (remhash name (triggers hook)))

(defun remove-trigger (hook &optional (ident *package*) (module (symbol-package hook)))
  "Attempts to remove the trigger from the hook."
  (setf (find-trigger ident (hook hook module T)) NIL))

(defmacro define-trigger (hook args &body body)
  "Defines a new trigger on the hook.
A trigger can either accept no arguments or it has to match the hook in its arguments list.
The name of the trigger defaults to the *PACKAGE*. If you want to have multiple triggers for
the same hook in the same package, use a list of the following structure as the HOOK argument:
 (hook trigger-name hook-module)"
  (destructuring-bind (name &optional (ident *package*) (module (symbol-package name))) (enlist hook)
    (let ((realargs (arglist (hook name module T))))
      (when (and args (not (function-lambda-matches realargs args)))
        (error "Lambda-list ~a does not match required list ~a."
               args realargs))
      `(progn
         (setf (find-trigger ',ident (hook ',name ,module))
               (lambda ,(or args realargs)
                 ,@(unless args `((declare (ignore ,@(extract-lambda-vars realargs)))))
                 ,@body))
         '(,name ,ident)))))

(defun trigger (hook &rest args)
  "Calls all triggers registered on the hook with the given arguments."
  (let ((hook (if (listp hook)
                  (hook (first hook) (second hook) T)
                  (hook hook (symbol-package hook) T))))
    (run-triggers hook args)))

;; Sticky

(defclass sticky-hook (hook)
  ((stuck-args :accessor stuck-args)))

(defmethod run-triggers :after ((hook sticky-hook) args)
  (setf (stuck-args hook) args))

(defmethod (setf find-trigger) :after ((function function) name (hook sticky-hook))
  (when (slot-boundp hook 'stuck-args)
    (apply function (stuck-args hook))))

(defmacro define-hook-switch (on off args)
  (destructuring-bind (on &optional (on-mod (symbol-package on))) (enlist on)
    (destructuring-bind (off &optional (off-mod (symbol-package off))) (enlist off)
      `(progn
         (define-hook (,on ,on-mod sticky-hook) ,args)
         (define-hook (,off ,off-mod hook) ,args)
         (define-trigger (,off unstick-hook) ()
           (slot-makunbound (hook ',on ,on-mod) 'stuck-args))))))
