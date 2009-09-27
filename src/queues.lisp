;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan
;;;;
;;;; Simple Queues
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :chanl)

(eval-when (:compile-toplevel)
  (defvar queue-sentinel (make-symbol "EMPTY")))

(declaim (ftype (function (fixnum) simple-vector)))
(define-speedy-function make-queue (length)
  (declare (type fixnum length))
  "Creates a new queue of maximum size LENGTH"
  (let ((queue (make-array (the fixnum (+ 2 length)))))
    (setf (svref queue 2) '#.queue-sentinel ; Sentinel value for an empty queue
          (svref queue 1) 2        ; Tail pointer set to first element
          (svref queue 0) 2)       ; Head pointer set to first element
    queue))

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

(define-speedy-function queue-head (queue)
  "QUEUE's head pointer"
  (the fixnum (svref queue 0)))

(define-speedy-function queue-tail (queue)
  "QUEUE's tail pointer"
  (the fixnum (svref queue 1)))

;;; This function needs to be eliminated
(define-speedy-function queue-peek (queue)
  "Dereference QUEUE's head pointer"
  (svref queue (queue-head queue)))

;;; As does this one
(define-speedy-function queue-zero-p (queue)
  "Checks whether QUEUE's theoretical length is zero"
  (= (the fixnum (queue-head queue))
     (the fixnum (queue-tail queue))))

(define-speedy-function queue-empty-p (queue)
  "Checks whether QUEUE is effectively empty"
  ;; We keep the head reference around because we do two checks
  (let ((head (queue-head queue)))
    (declare (type fixnum head))
    ;; Are the head and tail pointers the same?
    (when (= head (the fixnum (queue-tail queue)))
      ;; Is the value at the head pointer EQ to the sentinel?
      (eq (svref queue head) '#.queue-sentinel))))

(define-speedy-function queue-full-p (queue)
  "Checks whether QUEUE is effectively full"
  ;; We keep the head reference around because we do two checks
  (let ((head (queue-head queue)))
    (declare (type fixnum head))
    ;; Are the head and tail pointers the same?
    (when (= head (the fixnum (queue-tail queue)))
      ;; Is there a real value at the head pointer?
      (not (eq (svref queue head) '#.queue-sentinel)))))

(define-speedy-function queue-count (queue)
  "Returns QUEUE's effective length"
  (let ((length (the fixnum
                  (mod (- (the fixnum (svref queue 1))
                          (the fixnum (svref queue 0)))
                       (- (length queue) 2)))))
    (if (zerop length)
        (if (eq (queue-peek queue) '#.queue-sentinel) 0
            (- (length queue) 2))
        length)))

(define-speedy-function queue-max-size (queue)
  "Returns QUEUE's maximum length"
  (the fixnum (- (length (the simple-vector queue)) 2)))

(define-speedy-function enqueue (object queue)
  "Sets QUEUE's head to OBJECT and increments QUEUE's head pointer"
  (setf (svref queue (the (integer 2 #.(1- array-total-size-limit))
                       (svref queue 1)))
        object
        (svref queue 1)
        (the fixnum (+ 2 (mod (1- (the fixnum (svref queue 1)))
                              (- (length queue) 2)))))
  object)

(define-speedy-function dequeue (queue)
  "Sets QUEUE's tail to QUEUE, increments QUEUE's tail pointer, and returns the previous tail ref"
  (prog1 (svref queue (svref queue 0))
    (setf (svref queue 0)
          (the fixnum (+ 2 (mod (1- (the fixnum (svref queue 0)))
                                (- (length queue) 2)))))
    (when (queue-zero-p queue) (setf (svref queue (svref queue 0)) '#.queue-sentinel))))
