(cl:defpackage #:persistine.vector
  (:use #:cl #:alexandria #:persistine.utils)
  (:export
   #:make-persistent-vector
   #:pvector-p
   #:pvector-length
   #:pvector-get
   #:pvector-extend
   #:pvector-update
   #:pvector-pop))
(cl:in-package #:persistine.vector)

(declaim (optimize (speed 3)))

(defstruct (persistent-vector (:constructor %make-persistent-vector)
                              (:conc-name :pv-)
                              (:predicate :pvector-p))
  root
  tail
  (length 0 :type fixnum)
  (height 1 :type fixnum))

(defun make-persistent-vector (&key root tail (length 0) (height 1))
  (let ((root (or root (make-node)))
        (tail (or tail (make-node))))
    (%make-persistent-vector :root root
                             :tail tail
                             :length length
                             :height height)))

(defun pvector-length (vector)
  (pv-length vector))

(defun pvector-get (vector index)
  (node-get (get-node vector index)
            (index-at-height index 0)))

(defun pvector-extend (vector item)
  (let ((root (pv-root vector))
        (tail (pv-tail vector))
        (length (pv-length vector))
        (height (pv-height vector))
        (tail-offset (tail-offset vector)))
    (cond
      ((< (- length tail-offset) 32) ; Space in tail
       (let ((new-tail (node-push tail item)))
         (make-persistent-vector :root root
                                 :tail new-tail
                                 :length (1+ length)
                                 :height height)))
      ((> length (capacity height)) ; No space left in root
       (let ((new-root (make-node root (node-grow tail height))))
         (make-persistent-vector :root new-root
                                 :tail (make-node item)
                                 :length (1+ length)
                                 :height (1+ height))))
      (t
       (make-persistent-vector :root (node-push-deep root tail length height)
                               :tail (make-node item)
                               :length (1+ length)
                               :height height)))))

(defun pvector-update (vector index item)
  (declare (fixnum index))
  (let ((root (pv-root vector))
        (tail (pv-tail vector))
        (length (pv-length vector))
        (height (pv-height vector))
        (tail-offset (tail-offset vector)))
    (if (>= index tail-offset)
        (make-persistent-vector :root root
                                :tail (node-update
                                       tail
                                       (index-at-height index 0)
                                       item)
                                :length length
                                :height height)
        (make-persistent-vector :root (node-update-deep
                                       root index item height)
                                :tail tail
                                :length length
                                :height height))))

(defun pvector-pop (vector)
  (let ((root (pv-root vector))
        (tail (pv-tail vector))
        (length (pv-length vector))
        (height (pv-height vector))
        (tail-offset (tail-offset vector)))
    (if (> (- length tail-offset) 0)
        (make-persistent-vector :root root
                                :tail (node-pop tail)
                                :length (1- length)
                                :height height)
        (make-persistent-vector :root (node-pop-deep root length height)
                                :tail (node-get vector (- length 2))
                                :length (1- length)
                                :height (if (> height 1)
                                            (1- height)
                                            height)))))

;; Helper functions related to shifting magic

(defun index-at-height (index height)
  (declare (fixnum index height))
  (logand (rsh index (* height 5)) 31))

(defun capacity (height)
  (declare (fixnum height))
  (ash 1 (* (1+ height) 5)))

(defun tail-offset (vector)
  (let ((length (pv-length vector)))
    (if (< length 32)
        0
        (ash (rsh (1- length) 5) 5))))

;; Helper functions related to nodes of the tree

(defun get-node (vector index)
  "Returns node that holds item of given index"
  (declare (fixnum index))
  (let ((height (pv-height vector))
        (tail-offset (tail-offset vector)))
    (if (>= index tail-offset)
        (pv-tail vector)
        (reduce (lambda (node height)
                  (node-get node
                            (index-at-height index height)))
                (iota height :start height :step -1)
                :initial-value (pv-root vector)))))

(defun make-node (&rest items)
  (let ((array (make-32-array)))
    (dolist (item items array)
      (vector-push item array))))

(defun copy-node (node)
  (copy-array node))

(defun node-size (node)
  (fill-pointer node))

(defun node-get (node index)
  (declare (fixnum index))
  (aref node index))

(defun node-update (node index value)
  (declare (fixnum index))
  (let ((new-node (copy-node node)))
    (setf (aref new-node index)
          value)
    new-node))

(defun node-push (node value)
  (let ((new-node (copy-node node)))
    (vector-push value new-node)
    new-node))

(defun node-pop (node)
  (let ((new-node (copy-node node)))
    (vector-pop new-node)
    new-node))

(defun node-grow (node height)
  (declare (fixnum height))
  (if (= height 0)
      node
      (make-node (node-grow node (1- height)))))

(defun node-update-deep (node index item height)
  (declare (fixnum index height))
  (if (= height 0)
      (node-update node (index-at-height index 0) item)
      (let ((next-index (index-at-height index height)))
        (node-update node next-index
                     (node-update-deep (node-get node next-index)
                                       index
                                       item
                                       (1- height))))))

(defun node-push-deep (node item length height)
  (declare (fixnum length height))
  (if (= height 1)
      (node-push node item)
      (let ((next-index (index-at-height (1- length) height)))
        (if-let (child (node-get node next-index))
          (node-update node next-index
                    (node-push-deep child item length (1- height)))
          (node-push node (node-grow item (1- height)))))))

(defun node-pop-deep (node length height)
  (declare (fixnum length height))
  (let ((next-index (index-at-height (- length 2) height)))
    (if (> height 1)
        (let ((child (node-pop-deep node length (1- height))))
          (if (> (node-size child) 0)
              (node-update node next-index child)
              (node-pop node)))
        (node-pop node))))
