(cl:defpackage #:persistine.tests.vector
  (:use #:cl
        #:alexandria
        #:fiveam
        #:persistine.vector
        #:persistine.tests))
(cl:in-package #:persistine.tests.vector)

(def-suite vector :in persistine.tests::structures)

(in-suite vector)

(test length
  "Length of the vector should be consistent with its real length."
  (let (length)
    (for-all ((vector
               (gen-pvector :length
                            (lambda ()
                              (setf length
                                    (funcall (gen-integer :min 500 :max 1000)))))))
      (is (= (pvector-length vector)
             length)))))

(test get
  "Accessing the vector should return appropriate item at that index."
  (let ((length (funcall (gen-integer :min 50 :max 500))))
    (with-test-vector (vector :length length)
      (is (every (lambda (i) (= (pvector-get vector i) i))
                 (iota length))))))

(test extend
  "Extending the vector should add element to the end of the resulting vector, maintain persistense and keep length correct."
  (let (length)
    (for-all ((vector
               (gen-pvector :length
                            (lambda ()
                              (setf length
                                    (funcall (gen-integer :min 10 :max 300))))))
              (clone-vector (gen-pvector :length (lambda () length)))
              (i (gen-integer :min -10 :max 10)))
      (let ((new-vector (pvector-extend vector i)))
        (is (= (pvector-length new-vector)
               (1+ length)))
        (is (= (pvector-get new-vector length)
               i))
        (is (equalp vector clone-vector))))))

(test update
  "Updating the vector should replace the item at index with a new value, maintain persistence and keep length correct."
  (let (length)
    (for-all ((vector
               (gen-pvector :length
                            (lambda ()
                              (setf length
                                    (funcall (gen-integer :min 10 :max 300))))))
              (clone-vector (gen-pvector :length (lambda () length)))
              (i (gen-integer :min -10 :max 10)))
      (let* ((ix (random length))
             (new-vector (pvector-update vector ix i)))
        (is (= (pvector-length new-vector)
               length))
        (is (= (pvector-get new-vector ix)
               i))
        (is (equalp vector clone-vector))))))

(test pop
  "Popping the vector should return a new vector without last element, maintain persistence and update length."
  (let (length)
    (for-all ((vector
               (gen-pvector :length
                            (lambda ()
                              (setf length
                                    (funcall (gen-integer :min 10 :max 300))))))
              (clone-vector (gen-pvector :length (lambda () length))))
      (let ((new-vector (pvector-pop vector)))
        (is (= (pvector-length new-vector)
               (1- length)))
        (is (equalp vector clone-vector))))))

;; Utilities

(defun make-persistent-vector-of (list)
  (reduce (lambda (acc next)
            (pvector-extend acc next))
          list
          :initial-value (make-persistent-vector)))

(defun gen-pvector (&key (length (gen-integer :min 0 :max 100)))
  (lambda ()
    (make-persistent-vector-of (iota (funcall length)))))

(defmacro with-test-vector ((vector &key (length 100)) &body body)
  `(let ((,vector (make-persistent-vector-of (iota ,length))))
     ,@body))
