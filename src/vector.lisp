(cl:defpackage #:persistine.vector
  (:use #:cl #:alexandria #:optima #:persistine.utils))
(cl:in-package #:persistine.vector)

(declaim (optimize (speed 3)))

(defstruct (persistent-vector (:constructor %make-persistent-vector)
                              (:conc-name :pv-)
                              (:predicate pv-p))
  root
  tail
  length
  height)

(defun make-persistent-vector (&key root tail (length 0) (height 1))
  (let ((root (or root (make-node)))
        (tail (or tail (make-node))))
    (%make-persistent-vector :root root
                             :tail tail
                             :length length
                             :height height)))

(defun make-node (&rest items)
  (let ((array (make-32-array)))
    (dolist (item items array)
      (vector-push item array))))

(defun copy-node (node)
  (copy-array node))

(defun tail-offset (vector)
  (match vector
    ((pv- length)
     (declare (fixnum length))
     (if (< length 32)
         0
         (ash (rsh (1- length) 5) 5)))))

(defun descent (vector index)
  "Returns the node of the tree at whose level the given index is found"
  (declare (fixnum index))
  (match vector
    ((pv- root tail height)
     (if (>= index (tail-offset vector))
         tail
         (do* ((shift (* height 5)
                      (- shift 5))
               (node root
                     (aref node (logand (rsh index shift)
                                        31))))
              ((<= shift 0) node))))))

(defun pvector-ref (vector index)
  (aref (descent vector index)
        (logand index 31)))

(defun pvector-cons (vector item)
  (match vector
    ((pv- root tail length height)
     (cond
       ((< (- length (tail-offset vector)) 32)
        (let ((new-tail (copy-node tail)))
          (vector-push item new-tail)
          (make-persistent-vector :root root
                                  :tail new-tail
                                  :length (1+ length)
                                  :height height)))
       ((> (rsh length 5) (ash 1 (* height 5)))
        (let ((new-root (make-node)))
          (vector-push root new-root)
          (vector-push (expand-to-height tail height) new-root)
          (make-persistent-vector :root new-root
                                  :tail (make-node item)
                                  :length (1+ length)
                                  :height (1+ height))))
       (t
        (make-persistent-vector :root (insert-tail root tail length height)
                                :tail (make-node item)
                                :length (1+ length)
                                :height height))))))

(defun expand-to-height (node height)
  (if (= height 0)
      node
      (make-node (expand-to-height node (- height 1)))))

(defun insert-tail (node item length height)
  (let* ((new-node (copy-node node))
         (next-index (logand (rsh (1- length) (* 5 height)) 31))
         (child (aref new-node next-index)))
    (if child
        (setf (aref new-node next-index)
              (insert-tail child item length (1- height)))
        (vector-push (expand-to-height item (1- height))
                     new-node))
    new-node))
