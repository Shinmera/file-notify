#|
 This file is a part of file-notify
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.file-notify)

(defmacro define-implementable (name args)
  `(setf (fdefinition ',name)
         (lambda ,args
           (declare (ignore ,@(loop for arg in args
                                    unless (find arg lambda-list-keywords)
                                    collect arg)))
           (error "Not implemented"))))

(defmacro define-implementation (name args &body body)
  `(progn
     (fmakunbound ',name)
     (defun ,name ,args ,@body)))

(define-condition failure (error)
  ())

(define-implementable init (&key))

(define-implementable shutdown ())

(define-implementable watch (file/s &key events))

(define-implementable list-watched ())

(define-implementable unwatch (file/s))

(define-implementable process-events (function &key timeout))

(defmacro with-events ((file change-type &rest args) &body body)
  `(process-events (lambda (,file ,change-type) ,@body) ,@args))
