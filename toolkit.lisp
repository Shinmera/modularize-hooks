#|
 This file is a part of Modularize-Hooks
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.modularize.hooks)

(defun function-lambda-matches (function lambda-list)
  "Returns T if the function matches the lambda-list in arguments.
As a secondary value it returns a reason as to why it may have failed the test."
  (let ((lambda-cur (remove-aux-part (arglist function)))
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
