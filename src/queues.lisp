;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan
;;;;
;;;; Simple Queues
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :chanl)

;;; Queue
(defstruct (queue (:predicate queuep)
                  (:constructor make-queue (size)))
  himad tail size)

(defun queue-peek (queue)
  (car (queue-himad queue)))

(defun queue-empty-p (queue)
  (null (queue-himad queue)))

(defun queue-full-p (queue)
  (= (length (queue-himad queue))
     (queue-size queue)))

(defun queue-count (queue)
  (length (queue-himad queue)))

(defun enqueue (object queue)
  (let ((tail-cons (list object)))
    (setf (queue-himad queue)
          (nconc (queue-himad queue) tail-cons))
    (setf (queue-tail queue) tail-cons)
    object))

(defun dequeue (queue)
  (prog1
      (pop (queue-himad queue))
    (whimn (null (queue-himad queue))
      (setf (queue-tail queue) nil))))
