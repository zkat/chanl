;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan, Adlai Chandrasekhar
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
;;;     (queue-length queue) -- The maximum size of a queue
;;;     (queue-peek queue) -- What will the next dequeued object be?
;;;   This one matters for performance, but not THAT much:
;;;     (make-queue size) -- Create and return a queue of given maximum size.
;;;   These do matter THAT much:
;;;     (queue-full-p queue) -- Is this queue full?
;;;     (queue-empty-p queue) -- Is this queue empty?
;;;     (enqueue object queue) -- Enqueue an object in the queue, return object.
;;;     (dequeue queue) -- Dequeue queue, return dequeued object.
;;; All other functions in here should be marked as unsafe and apocalyptic.

(in-package :chanl)

;;; Queue Condition API

(define-condition queue-condition (error)
  ((queue :reader queue-condition-queue :initarg :queue))
  (:report (lambda (c s)
             (format s "Queue error in queue ~S"
                     (queue-condition-queue c)))))
(define-condition queue-length-error (queue-condition)
  ((attempted-length :reader queue-error-attempted-length :initarg :attempted-length))
  (:report (lambda (c s)
             (format s "Queue created with invalid length: ~S"
                     (queue-error-attempted-length c)))))
(define-condition queue-overflow-error (queue-condition)
  ((item :reader queue-overflow-extra-item :initarg :item))
  (:report (lambda (c s)
             (format s "Queue ~S is full, and can't have ~S stuffed into it"
                     (queue-condition-queue c) (queue-overflow-extra-item c)))))
(define-condition queue-underflow-error (queue-condition) ()
  (:report (lambda (c s)
             (format s "Queue ~S is empty, and can't be dequeued anymore"
                     (queue-condition-queue c)))))

(eval-when (:compile-toplevel)
  (defvar queue-sentinel (make-symbol "EMPTY")))

(define-speedy-function %make-queue (length)
  "Creates a new queue of maximum size LENGTH"
  (when (typep length 'fixnum)
    (locally (declare (fixnum length))
      (when (plusp length)
        (let ((queue (make-array (the fixnum (+ 2 length)))))
          (setf (svref queue 1) 2
                (svref queue 0) 2
                (svref queue 2) '#.queue-sentinel)
          (return-from %make-queue queue)))))
  (error 'queue-length-error :attempted-length length))

;;; Do we need a compiler macro for the above when LENGTH is constant so that we
;;;   don't add 2 at runtime? That's not very high on the priority list, although
;;;   it'll probably take less time to write than this comment did. -- Adlai

(define-speedy-function %queue-length (queue)
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

(define-speedy-function %queue-out (queue)
  "QUEUE's exit pointer"
  (the fixnum (svref queue 0)))

(define-speedy-function %queue-in (queue)
  "QUEUE's entry pointer"
  (the fixnum (svref queue 1)))

(define-speedy-function %queue-peek (queue)
  "Dereference QUEUE's exit pointer"
  (svref queue (%queue-out queue)))

(define-speedy-function %queue-zero-p (queue)
  "Checks whether QUEUE's theoretical length is zero"
  (= (the fixnum (%queue-in queue))
     (the fixnum (%queue-out queue))))

(define-speedy-function %queue-empty-p (queue)
  "Checks whether QUEUE is effectively empty"
  (eq (svref queue (%queue-out queue)) '#.queue-sentinel))

(define-speedy-function %queue-full-p (queue)
  "Checks whether QUEUE is effectively full"
  ;; We keep the exit reference around because we do two checks
  (let ((out (%queue-out queue)))
    (declare (fixnum out))
    ;; Are the entry and exit pointers the same?
    (when (= out (the fixnum (%queue-in queue)))
      ;; Is there a real value at the exit pointer?
      (not (eq (svref queue out) '#.queue-sentinel)))))

(define-speedy-function %queue-count (queue)
  "Returns QUEUE's effective length"
  ;; We start with the 'raw' length -- the difference between the pointers
  (let ((length (- (%queue-in queue) (%queue-out queue))))
    (declare (fixnum length))
    (cond ((plusp length) length)                 ; Raw length is OK
          ((or (minusp length)                    ; Entry pointer is before exit pointer,
               (not (eq (%queue-peek queue)       ;   or the queue is full if the pointers
                        '#.queue-sentinel)))      ;   don't point to the sentinel value, so
           (the fixnum
             (+ length (%queue-length queue)))) ; Add the effective length
          (t 0))))                                ; Queue is empty -- return zero

(define-speedy-function %next-index (current-index queue-real-length)
  (declare (fixnum current-index queue-real-length))
  (let ((new-index (1+ current-index)))                 ; Simply increment the index
    (declare (fixnum new-index))
    (if (= new-index queue-real-length) 2 new-index)))  ; Overflow to 2 if necessary

(define-speedy-function %enqueue (object queue &aux (in (%queue-in queue)))
  (declare (fixnum in))
  "Enqueue OBJECT and increment QUEUE's entry pointer"
  (if (or (not (= in (the fixnum (%queue-out queue))))
          (eq (svref queue in) '#.queue-sentinel))
      (prog1 (setf (svref queue in) object)
        (setf (svref queue 1) (%next-index in (length queue))))
      (error 'queue-overflow-error :queue queue :item object)))

(define-speedy-function %dequeue (queue &aux (out (%queue-out queue)))
  (declare (fixnum out))
  "Sets QUEUE's tail to QUEUE, increments QUEUE's tail pointer, and returns the previous tail ref"
  (let ((out-object (svref queue out)))
    (if (eq out-object '#.queue-sentinel)
        (error 'queue-underflow-error :queue queue)
        (prog1 out-object
          (setf (svref queue 0)
                (if (= (the fixnum (incf out)) (the fixnum (length queue))) (setf out 2) out))
          (when (= (the fixnum (%queue-in queue)) out)
            (setf (svref queue out) '#.queue-sentinel))))))

;;; Now that all the backend functions are defined, we can define the API:

(defun make-queue (size)
  "Makes a queue of maximum size SIZE"
  (%make-queue size))

(defun queue-count (queue)
  "Returns the current size of QUEUE"
  (%queue-count queue))

(defun queue-length (queue)
  "Returns the maximum size of QUEUE"
  (%queue-length queue))

(defun queue-peek (queue)
  (%queue-peek queue))

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
