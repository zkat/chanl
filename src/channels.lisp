;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Josh Marchan
;;;;
;;;; Channel Definition
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :chanl)

(defstruct (channel (:constructor %make-channel)
                    (:predicate channelp)
                    (:print-object
                     (lambda (channel stream)
                       (print-unreadable-object (channel stream :type t :identity t)
                         (if (channel-buffered-p channel)
                             (format stream "[~A/~A]"
                                     (queue-count (channel-buffer channel))
                                     (queue-max-size (channel-buffer channel)))
                             (format stream "[unbuffered]"))))))
  (value *secret-unbound-value*) buffer
  (being-written-p nil :type (member t nil))
  (being-read-p nil :type (member t nil))
  (lock (bt:make-recursive-lock) :read-only t)
  (send-ok (bt:make-condition-variable) :read-only t)
  (recv-ok (bt:make-condition-variable) :read-only t))

(defun channel-buffered-p (channel)
  (when (channel-buffer channel) t))

(defvar *secret-unbound-value* (gensym "SECRETLY-UNBOUND-"))
(defun make-channel (&optional (buffer-size 0))
  (when (< buffer-size 0)
    (error "buffer size cannot be negative."))
  (let ((channel (%make-channel)))
    (when (> buffer-size 0)
      (setf (channel-buffer channel) (make-queue buffer-size)))
    channel))

(defun send (channel obj)
  (with-accessors ((lock channel-lock)
                   (recv-ok channel-recv-ok))
      channel
    (bt:with-recursive-lock-held (lock)
      (wait-to-send channel)
      (channel-insert-value channel obj)
      (bt:condition-notify recv-ok)
      obj)))

(defun send-blocks-p (channel)
  (if (channel-buffered-p channel)
      (and (queue-full-p (channel-buffer channel))
           (not (and (channel-being-read-p channel)
                     (eq (channel-value channel)
                         *secret-unbound-value*))))
      (not (and (channel-being-read-p channel)
                (eq (channel-value channel)
                    *secret-unbound-value*)))))

(defun wait-to-send (channel)
  (loop while (send-blocks-p channel)
     do (bt:condition-wait (channel-send-ok channel) (channel-lock channel))))

(defun channel-insert-value (channel value)
  (if (channel-buffered-p channel)
      (progn
        (when (queue-full-p (channel-buffer channel))
          (setf (channel-value channel) (dequeue (channel-buffer channel))))
        (enqueue value (channel-buffer channel)))
      (setf (channel-value channel) value)))

(defmacro with-read-state ((channel) &body body)
  `(unwind-protect
        (progn (setf (channel-being-read-p ,channel) t)
               ,@body)
     (setf (channel-being-read-p ,channel) nil)))

(defun recv (channel)
  (with-accessors ((lock channel-lock)
                   (send-ok channel-send-ok))
      channel
    (bt:with-recursive-lock-held (lock)
      (with-read-state (channel)
        (bt:condition-notify send-ok)
        (wait-to-recv channel)
        (channel-grab-value channel)))))

(defun recv-blocks-p (channel)
  (if (channel-buffered-p channel)
      (and (queue-empty-p (channel-buffer channel))
           (eq *secret-unbound-value* (channel-value channel)))
      (eq *secret-unbound-value* (channel-value channel))))

(defun wait-to-recv (channel)
  (loop while (recv-blocks-p channel)
     do (bt:condition-wait (channel-recv-ok channel) (channel-lock channel))))

(defun channel-grab-value (channel)
  (when (and (channel-buffered-p channel)
             (not (queue-empty-p (channel-buffer channel)))
             (eq *secret-unbound-value* (channel-value channel)))
    (setf (channel-value channel) (dequeue (channel-buffer channel))))
  (prog1 (channel-value channel)
    (setf (channel-value channel) *secret-unbound-value*)))
