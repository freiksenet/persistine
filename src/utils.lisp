(cl:defpackage #:persistine.utils
  (:use #:cl #:alexandria)
  (:export
   #:make-32-array
   #:rsh))
(cl:in-package #:persistine.utils)

(defun make-32-array ()
  (make-array '(32)
              :initial-element nil
              :fill-pointer 0))

(defun rsh (integer count)
  (declare (fixnum integer count))
  (ash integer (- count)))
