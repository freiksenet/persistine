(cl:defpackage #:persistine.utils
  (:use #:cl #:alexandria)
  (:export
   #:make-32-array
   #:rsh
   #:range))
(cl:in-package #:persistine.utils)

(declaim (inline make-32-array))
(defun make-32-array ()
  (make-array '(32)
              :initial-element nil
              :fill-pointer 0))

(declaim (inline rsh))
(defun rsh (integer count)
  (declare (fixnum integer count))
  (ash integer (- count)))

(declaim (inline range))
(defun range (from to &optional (step 1))
  (let ((step (if (> from to)
                  (- step)
                  step))
        (test (if (> from to)
                  (lambda (i to) (<= i to))
                  (lambda (i to) (>= i to)))))
    (do ((i from (+ i step))
         (result (list) (push i result)))
        ((funcall test i to) (nreverse result)))))
