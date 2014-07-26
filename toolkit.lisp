#|
 This file is a part of Modularize-Hooks
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.modularize.hooks)

(defun lambda-keyword-p (symbol)
  (find symbol '(&allow-other-keys &aux &body &environment &key &optional &rest &whole)))

(defun remove-aux-part (lambda-list)
  (let ((position (position '&aux lambda-list)))
    (if position
        (subseq lambda-list 0 position)
        lambda-list)))

(defun flatten-lambda-list (lambda-list)
  (mapcar #'(lambda (a) (if (listp a) (car a) a)) lambda-list))

(defun extract-lambda-vars (lambda-list)
  (remove-if #'lambda-keyword-p (flatten-lambda-list (remove-aux-part lambda-list))))

(defun function-arguments (function)
  "Returns the lambda-list of the function if possible.
This is only implemented with: SBCL, SWANK, working FUNCTION-LAMBDA-EXPRESSION."
  #+sbcl (sb-introspect:function-lambda-list function)
  #+(and swank (not sbcl)) (swank-backend:arglist function)
  #-(and sbcl swank) (second (nth-value 2 (function-lambda-expression function))))

(defun required-lambda-vars (lambda-list)
  (loop for i in (if (eql '&whole (first lambda-list))
                     (cddr lambda-list)
                     lambda-list)
        until (lambda-keyword-p i)
        collect i))

(defun function-lambda-matches (function lambda-list)
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
