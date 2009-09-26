;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Josh Marchan
;;;;
;;;; Simple Queues
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :chanl)

;;; Queue
(defstruct (queue (:predicate queuep)
                  (:constructor make-queue (size)))
  head tail size)

(defun queue-peek (queue)
  (car (queue-head queue)))

(defun queue-empty-p (queue)
  (null (queue-head queue)))

(defun queue-full-p (queue)
  (= (length (queue-head queue))
     (queue-size queue)))

(defun queue-count (queue)
  (length (queue-head queue)))

(defun enqueue (object queue)
  (let ((tail-cons (list object)))
    (setf (queue-head queue)
          (nconc (queue-head queue) tail-cons))
    (setf (queue-tail queue) tail-cons)
    object))

(defun dequeue (queue)
  (prog1
      (pop (queue-head queue))
    (when (null (queue-head queue))
      (setf (queue-tail queue) nil))))
