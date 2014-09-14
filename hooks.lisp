#|
 This file is a part of Modularize-Hooks
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.modularize.hooks)

(defun make-hook-package-name (module)
  "Returns a fitting hooks package name for the given module."
  (format NIL "~a.HOOKS" (module-identifier module)))

(defun hook-package (module)
  "Returns the hooks package of the module or signals an error if none can be found."
  (or (find-package (make-hook-package-name module))
      (error "Module is not set up for hooks.")))

(defun transform-symbol (module-symbol)
  "Transforms the symbol into one from the module's hooks package."
  (let ((package (hook-package (symbol-package module-symbol))))
    (or (find-symbol (symbol-name module-symbol) package)
        (intern (symbol-name module-symbol) package))))

(defun hookify (&optional (module *package*))
  "Turns the module into one capable of hooks.

In essence this merely defines a new package with a matching name."
  (let ((name (make-hook-package-name module)))
    (unless (find-package name)
      (make-package name :use ()))))

(defun dehookify (&optional (module *package*))
  "Returns the module to one that is not capable of hooks.

In essence this merely removes all functions from the hooks package
and deletes it."
  (let ((package (hook-package module)))
    (do-symbols (symbol package)
      (when (fboundp symbol)
        (fmakunbound symbol)))
    (delete-package package)))

(define-modularize-hook (module)
  (hookify module))

(define-delete-hook (module)
  (dehookify module))

(defun call-all-methods (gf &rest args)
  "Calls all methods of the generic function with the given arguments."
  (loop for method in (c2mop:generic-function-methods gf)
        unless (method-qualifiers method)
          do (let ((ident (first (c2mop:method-specializers method))))
               (when (typep ident 'c2mop:eql-specializer)
                 (apply gf (c2mop:eql-specializer-object ident) args)))))

(defmacro define-hook (name args &optional documentation)
  "Defines a new hook on which triggers can be defined.
The name should be a symbol from the module that the hook should belong to."
  (assert (symbolp name))
  (let ((name (transform-symbol name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defgeneric ,name (ident ,@args)
         ,@(when documentation
             `((:documentation ,documentation))))
       (defmethod ,name ((ident null) ,@args)
         (call-all-methods #',name ,@(extract-lambda-vars args)))
       ',name)))

(defun remove-hook (name)
  "Removes the hook as named."
  (fmakunbound (transform-symbol name)))

(defun %trigger (hook &rest args)
  (apply (symbol-function (transform-symbol hook)) NIL args))

(defun trigger (hook &rest args)
  "Calls all triggers registered on the hook with the given arguments."
  (apply #'%trigger hook args))

(define-compiler-macro trigger (hook &rest args)
  (if (symbolp hook)
      `(,(transform-symbol hook) ,@args)
      `(%trigger ,hook ,@args)))

(defmacro define-trigger (hook args &body body)
  "Defines a new trigger on the hook.
A trigger can either accept no arguments or it has to match the hook in its arguments list.
The name of the trigger defaults to the *PACKAGE*. If you want to have multiple triggers for
the same hook in the same package, use a list of the following structure as the HOOK argument:
 (hook trigger-name)"
  (destructuring-bind (name ident) (if (listp hook) hook (list hook *package*))
    (let* ((name (transform-symbol name))
           (realargs (cdr (function-arguments (symbol-function name)))))
      (assert (or (null args) (function-lambda-matches (symbol-function name) (cons NIL args))))
      `(progn
         (defmethod ,name ((,(gensym "IDENT") (eql ,ident)) ,@(or args realargs))
           ,@(unless args `((declare (ignore ,@(extract-lambda-vars realargs)))))
           ,@body)
         ',name))))

(defmacro define-trigger-method (hook &rest args)
  `(defmethod ,(transform-symbol hook) ,@args))

(defun remove-trigger (hook &optional ident specializers)
  "Attempts to remove the trigger from the hook."
  (let* ((name (transform-symbol hook))
         (func (symbol-function name))
         (ident (or ident *package*))
         (spec (or specializers (required-lambda-vars (cdr (function-arguments func))))))
    (remove-method func (find-method func '() `((eql ,ident) ,@(mapcar (constantly T) spec))))))
