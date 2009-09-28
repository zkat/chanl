;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Josh Marchan, Adlai Chandrasekhar
;;;;
;;;; Channel Definition
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :chanl)

(defvar *secret-unbound-value* (gensym "SECRETLY-UNBOUND-")
  "This value is used as a sentinel in channels.")

;;;
;;; Channel objects
;;;
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
  (readers 0) ; number of readers currently trying to read
  (writers 0)
  (lock (bt:make-recursive-lock) :read-only t)
  (send-ok (bt:make-condition-variable) :read-only t)
  (recv-ok (bt:make-condition-variable) :read-only t))

(defun channel-buffered-p (channel)
  (when (channel-buffer channel) t))

(defun make-channel (&optional (buffer-size 0))
  (when (< buffer-size 0)
    (error "buffer size cannot be negative."))
  (let ((channel (%make-channel)))
    (when (> buffer-size 0)
      (setf (channel-buffer channel) (make-queue buffer-size)))
    channel))

;;;
;;; Messaging
;;;

;;; Sending
(defmacro with-write-state ((channel) &body body)
  `(unwind-protect
        (progn (incf (channel-writers ,channel))
               ,@body)
     (decf (channel-writers ,channel))))

(defun send (channel obj &optional (blockp t))
  (with-accessors ((lock channel-lock)
                   (recv-ok channel-recv-ok))
      channel
    (bt:with-recursive-lock-held (lock)
      (with-write-state (channel)
        (loop while (send-blocks-p channel)
           if blockp
           do (bt:condition-wait (channel-send-ok channel) lock)
           else do (return-from send (values nil nil))))
      (bt:condition-notify recv-ok)
      (channel-insert-value channel obj) ; wake up a sleeping reader
      (values obj t))))

(defun send-blocks-p (channel)
  ;; This is a bit of a logical mess. The points to note are:
  ;; 1. We must make special accomodations for buffered channels, since
  ;;    they don't block if there's still space in the buffer.
  ;; 2. We block unless *both* of these conditions are filled:
  ;;    a. at least one reader somewhere currently trying to read from the channel.
  ;;    b. the sentinel value is present (meaning we're in the middle of a send already,
  ;;       for some reason. Is this even possible?)
  (and (not (and (plusp (channel-readers channel))
                 (eq (channel-value channel)
                     *secret-unbound-value*)))
       (if (channel-buffered-p channel)
           (queue-full-p (channel-buffer channel))
           t))) ; this basically means we ignore this IF when the channel is unbuffered.

(defun channel-insert-value (channel value)
  ;; We have to do some sleight-of hand here when buffered channels are involved.
  ;; otherwise, we really just set the value to the actual value we want to send.
  ;; This'll temporarily clobber the sentinel (recv should restore it once it
  ;; grabs the value).
  (if (channel-buffered-p channel)
      (progn
        (when (queue-full-p (channel-buffer channel))
          (setf (channel-value channel) (dequeue (channel-buffer channel))))
        (enqueue value (channel-buffer channel)))
      (setf (channel-value channel) value)))

;;; Sending
(defmacro with-read-state ((channel) &body body)
  ;; Basically, a poor man's semaphore implementation in macro form!
  ;; The idea behind the 'read state' is to have a *second* sentinel, of sorts,
  ;; that allows us to catch a corner case that I discussed with pkhuong before,
  ;; but completely forgot the details about. Figure it out. If you do, be sure
  ;; to update this :P -- sykopomp
  `(unwind-protect
        (progn (incf (channel-readers ,channel))
               ,@body)
     (decf (channel-readers ,channel))))

(defun recv (channel &optional (blockp t))
  (with-accessors ((lock channel-lock)
                   (send-ok channel-send-ok))
      channel
    (bt:with-recursive-lock-held (lock)
      (with-read-state (channel)
        ;; we're ready to grab something! Notify the others that we want some lovin'
        (bt:condition-notify send-ok)
        (loop while (%recv-blocks-p channel)
           do (if (or blockp (plusp (channel-writers channel)))
                  (bt:condition-wait (channel-recv-ok channel) lock)
                  (return-from recv (values nil nil))))
        (values (channel-grab-value channel) t)))))

(defun %recv-blocks-p (channel)
  ;; The reason for this kludge is a bit complicated. Basically, we need a special
  ;; form of recv-blocks-p to use internally that doesn't check the number of writers.
  ;; If we check the writers here, we end up not releasing the lock and letting SEND
  ;; do its thing before we keep going forward.
  (if (channel-buffered-p channel)
      (and (queue-empty-p (channel-buffer channel))
           (eq *secret-unbound-value* (channel-value channel)))
      (and (eq *secret-unbound-value* (channel-value channel)))))

(defun recv-blocks-p (channel)
  ;; This is the actual -interface- version of recv-blocks-p that we can use if we want
  ;; to check, from outside, if a channel would block. VERY CLEVER RITE?! :D -- sykopomp
  (and (not (plusp (channel-writers channel)))
       (%recv-blocks-p channel)))

(defun channel-grab-value (channel)
  ;; This one's a doozy. The special case of having a buffered channel means we need
  ;; to do a bit of juggling. We want the sender to be able to queue something, but
  ;; if our queue is full, we can't let them do that. The solution is to use the value
  ;; slot in the struct. Once the dequeue into channel-value is done, we can treat the
  ;; channel as unbuffered, returning the value we just dequeued, and finally setting
  ;; the channel-value back to the sentinel. We're done here. --sykopomp
  (when (and (channel-buffered-p channel)
             (not (queue-empty-p (channel-buffer channel)))
             (eq *secret-unbound-value* (channel-value channel)))
    (setf (channel-value channel) (dequeue (channel-buffer channel))))
  (prog1 (channel-value channel)
    (setf (channel-value channel) *secret-unbound-value*)))

