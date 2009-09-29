;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Josh Marchan, Adlai Chandrasekhar
;;;;
;;;; Simple Queues
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The functions in this file are dangerous. Good compilers will generate code that will
;;;   do VERY funky shit when called incorrectly. Calls to these functions should be hidden
;;;   behind safe code that never passes arguments of incorrect types.

;;; Unlike the standard queue implementation which you find in CL code (push to the tail of
;;;   a list, pop from the head), these queues do not cons one bit. You do, however, need to
;;;   "declare" a queue's size (at runtime) when you make one.

;;; This is the API required by src/channels.lisp:
;;;   These don't need to be super-optimized:
;;;     (queue-count queue) -- How many elements are present in a queue
;;;     (queue-max-size queue) -- The maximum size of a queue
;;;   This one matters for performance, but not THAT much:
;;;     (make-queue size) -- Create and return a queue of given maximum size.
;;;   These do matter THAT much:
;;;     (queue-full-p queue) -- Is this queue full?
;;;     (queue-empty-p queue) -- Is this queue empty?
;;;     (enqueue object queue) -- Enqueue an object in the queue, return object.
;;;     (dequeue queue) -- Dequeue queue, return dequeued object.
;;; All other functions in here should be marked as unsafe and apocalyptic.

(in-package :chanl)

(eval-when (:compile-toplevel)
  (defvar queue-sentinel (make-symbol "EMPTY")))

(declaim (ftype (function (fixnum) simple-vector)))
(define-speedy-function %make-queue (length)
  (declare (fixnum length))
  "Creates a new queue of maximum size LENGTH"
  (let ((queue (make-array (the fixnum (+ 2 length)))))
    (setf (svref queue 2) '#.queue-sentinel ; Sentinel value for an empty queue
          (svref queue 1) 2        ; Tail pointer set to first element
          (svref queue 0) 2)       ; Head pointer set to first element
    queue))

;;; Do we need a compiler macro for the above when LENGTH is constant so that we
;;;   don't add 2 at runtime? That's not very high on the priority list, although
;;;   it'll probably take less time to write than this comment did. -- Adlai

(define-speedy-function %queue-max-size (queue)
  "Returns QUEUE's maximum length"
  (the fixnum (- (length (the simple-vector queue)) 2)))

(define-speedy-function queuep (x)
  "If this returns NIL, X is not a queue"
  (when (simple-vector-p x)
    (let ((length (length x))
          (head (svref x 0))
          (tail (svref x 1)))
      (and (typep head 'fixnum)
           (typep tail 'fixnum)
           (< 1 head length)
           (< 1 tail length)))))

(define-speedy-function %queue-head (queue)
  "QUEUE's head pointer"
  (the fixnum (svref queue 0)))

(define-speedy-function %queue-tail (queue)
  "QUEUE's tail pointer"
  (the fixnum (svref queue 1)))

;;; This function needs to be eliminated
(define-speedy-function %queue-peek (queue)
  "Dereference QUEUE's head pointer"
  (svref queue (%queue-head queue)))

;;; As does this one
(define-speedy-function %queue-zero-p (queue)
  "Checks whether QUEUE's theoretical length is zero"
  (= (the fixnum (%queue-head queue))
     (the fixnum (%queue-tail queue))))

(define-speedy-function %queue-empty-p (queue)
  "Checks whether QUEUE is effectively empty"
  ;; We keep the head reference around because we do two checks
  (let ((head (%queue-head queue)))
    (declare (fixnum head))
    ;; Are the head and tail pointers the same?
    (when (= head (the fixnum (%queue-tail queue)))
      ;; Is the value at the head pointer EQ to the sentinel?
      (eq (svref queue head) '#.queue-sentinel))))

(define-speedy-function %queue-full-p (queue)
  "Checks whether QUEUE is effectively full"
  ;; We keep the head reference around because we do two checks
  (let ((head (%queue-head queue)))
    (declare (fixnum head))
    ;; Are the head and tail pointers the same?
    (when (= head (the fixnum (%queue-tail queue)))
      ;; Is there a real value at the head pointer?
      (not (eq (svref queue head) '#.queue-sentinel)))))

(define-speedy-function %queue-count (queue)
  "Returns QUEUE's effective length"
  ;; We start with the 'raw' length -- the difference between the pointers
  (let ((length (- (%queue-tail queue) (%queue-head queue))))
    (declare (fixnum length))
    (cond ((plusp length) length)                 ; Raw length is OK
          ((or (minusp length)                    ; Tail pointer is before head pointer,
               (not (eq (%queue-peek queue)       ;   or the queue is full if the pointers
                        '#.queue-sentinel)))      ;   don't point to the sentinel value, so
           (the fixnum
             (+ length (%queue-max-size queue)))) ; Add the effective length
          (t 0))))                                ; Queue is empty -- return zero

(define-speedy-function %next-index (current-index queue-real-length)
  (declare (fixnum current-index queue-real-length))
  (let ((new-index (1+ current-index)))                 ; Simply increment the index
    (declare (fixnum new-index))
    (if (= new-index queue-real-length) 2 new-index)))  ; Overflow to 2 if necessary

(define-speedy-function %enqueue (object queue)
  "Sets QUEUE's head to OBJECT and increments QUEUE's head pointer"
  (prog1 (setf (svref queue (%queue-tail queue)) object)
    (setf (svref queue 1) (%next-index (%queue-tail queue) (length queue)))))

(define-speedy-function %dequeue (queue &aux (head (%queue-head queue)))
  "Sets QUEUE's tail to QUEUE, increments QUEUE's tail pointer, and returns the previous tail ref"
  (prog1 (svref queue head)
    (setf (svref queue 0) (%next-index head (length queue)))
    (when (= (the fixnum (%queue-tail queue)) (the fixnum head))
      (setf (svref queue head) '#.queue-sentinel))))

;;; Now that all the backend functions are defined, we can define the API:

(defun make-queue (size)
  "Makes a queue of maximum size SIZE"
  (%make-queue size))

(defun queue-count (queue)
  "Returns the current size of QUEUE"
  (%queue-count queue))

(defun queue-max-size (queue)
  "Returns the maximum size of QUEUE"
  (%queue-max-size queue))

(defun queue-full-p (queue)
  "Tests whether QUEUE is full"
  (%queue-full-p queue))

(defun queue-empty-p (queue)
  "Tests whether QUEUE is empty"
  (%queue-empty-p queue))

(defun enqueue (object queue)
  "Enqueues OBJECT in QUEUE"
  (%enqueue object queue))

(defun dequeue (queue)
  "Dequeues QUEUE"
  (%dequeue queue))
