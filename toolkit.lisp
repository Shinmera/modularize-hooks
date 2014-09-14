#|
 This file is a part of Modularize-Hooks
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.modularize.hooks)

(defun lambda-keyword-p (symbol)
  "Returns the symbol if it is a lambda-keyword symbol (the &-options)."
  (find symbol '(&allow-other-keys &aux &body &environment &key &optional &rest &whole)))

(defun remove-aux-part (lambda-list)
  "Removes the &aux part of the lambda-list."
  (let ((position (position '&aux lambda-list)))
    (if position
        (subseq lambda-list 0 position)
        lambda-list)))

(defun flatten-lambda-list (lambda-list)
  "Flattens the lambda-list by replacing all lists within it with their respective first symbol."
  (mapcar #'(lambda (a) (if (listp a) (car a) a)) lambda-list))

(defun extract-lambda-vars (lambda-list)
  "Extracts the symbols that name the variables in the lambda-list."
  (remove-if #'lambda-keyword-p (flatten-lambda-list (remove-aux-part lambda-list))))

(defun function-arguments (function)
  "Returns the lambda-list of the function if possible.
This is only implemented with: SBCL, SWANK, working FUNCTION-LAMBDA-EXPRESSION."
  (assert (or (symbolp function)
              (listp function))
          () "Function must be the symbol or list naming the function.")
  #+sbcl (sb-introspect:function-lambda-list (fdefinition function))
  #+(and swank (not sbcl)) (swank-backend:arglist function)
  #-(or sbcl swank) (third (function-lambda-expression (fdefinition function))))

(defun required-lambda-vars (lambda-list)
  "Returns the list of required variables in the lambda-list."
  (loop for i in (if (eql '&whole (first lambda-list))
                     (cddr lambda-list)
                     lambda-list)
        until (lambda-keyword-p i)
        collect i))

(defun function-lambda-matches (function lambda-list)
  "Returns T if the function matches the lambda-list in arguments.
As a secondary value it returns a reason as to why it may have failed the test."
  (let ((lambda-cur (remove-aux-part (function-arguments function)))
        (lambda-act (remove-aux-part lambda-list)))
    (cond
      ((/= (length lambda-cur) (length lambda-act))
       (values NIL "Lambda-lists do not match in length."))
      ((loop for cur in lambda-cur
             for act in lambda-act
             thereis (or (and (lambda-keyword-p cur)
                              (not (lambda-keyword-p act)))
                         (and (lambda-keyword-p act)
                              (not (eql act cur)))))
       (values NIL "Lambda-lists do not match in structure."))
      (T (values T "Test passed.")))))
