#|
 This file is a part of Modularize-Hooks
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.modularize.hooks)

(defun make-hook-package-name (module)
  (format NIL "~a.HOOKS" (module-identifier module)))

(defun hook-package (module)
  (or (find-package (make-hook-package-name module))
      (error "Module is not set up for hooks.")))

(defun transform-symbol (module-symbol)
  (let ((package (hook-package (symbol-package module-symbol))))
    (or (find-symbol (symbol-name module-symbol) package)
        (intern (symbol-name module-symbol) package))))

(defun hookify (&optional (module *package*))
  (make-package (make-hook-package-name module) :use ()))

(defun dehookify (&optional (module *package*))
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
  (loop for method in (c2mop:generic-function-methods gf)
        unless (method-qualifiers method)
          do (let ((ident (first (c2mop:method-specializers method))))
               (when (typep ident 'c2mop:eql-specializer)
                 (apply gf (c2mop:eql-specializer-object ident) args)))))

(defmacro define-hook (name args &optional documentation)
  (assert (symbolp name))
  (let ((name (transform-symbol name)))
    `(progn
       (defgeneric ,name (ident ,@args)
         (:documentation ,documentation))
       (defmethod ,name ((ident null) ,@args)
         (call-all-methods #',name ,@(extract-lambda-vars args))))))

(defun remove-hook (name)
  (fmakunbound (transform-symbol name)))

(defun %trigger (hook &rest args)
  (apply (symbol-function (transform-symbol hook)) NIL args))

(defun trigger (hook &rest args)
  (apply #'%trigger hook args))

(define-compiler-macro trigger (hook &rest args)
  (if (symbolp hook)
      `(,(transform-symbol hook) ,@args)
      `(%trigger ,hook ,@args)))

(defmacro define-trigger (hook args &body body)
  (destructuring-bind (name ident) (if (listp hook) hook (list hook *package*))
    (let* ((name (transform-symbol name))
           (realargs (cdr (function-arguments (symbol-function name)))))
      (assert (or (null args) (function-lambda-matches (symbol-function name) (cons NIL args))))
      `(defmethod ,name ((,(gensym "IDENT") (eql ,ident)) ,@(or args realargs))
         ,@(unless args `((declare (ignore ,@(extract-lambda-vars realargs)))))
         ,@body))))

(defmacro define-trigger-method (hook &rest args)
  `(defmethod ,(transform-symbol hook) ,@args))

(defun remove-trigger (hook &optional ident specializers)
  (let* ((name (transform-symbol hook))
         (func (symbol-function name))
         (ident (or ident *package*))
         (spec (or specializers (required-lambda-vars (cdr (function-arguments func))))))
    (remove-method func (find-method func '() `((eql ,ident) ,@(mapcar (constantly T) spec))))))
