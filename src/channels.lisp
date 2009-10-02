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

;;; unbuffered
(defclass channel ()
  ((value :initform *secret-unbound-value* :accessor channel-value)
   (readers :initform 0 :accessor channel-readers)
   (writers :initform 0 :accessor channel-writers)
   (lock :initform (bt:make-recursive-lock) :accessor channel-lock)
   (send-ok :initform (bt:make-condition-variable) :accessor channel-send-ok)
   (recv-ok :initform (bt:make-condition-variable) :accessor channel-recv-ok)))

(defgeneric channelp (channel)
  (:method ((anything-else t)) nil)
  (:method ((channel channel)) t))

;;; buffered
(defconstant +maximum-buffer-size+ (- array-total-size-limit 2)
  "The exclusive upper bound on the size of a channel's buffer.")

(defclass buffered-channel (channel)
  ((buffer :initarg :buffer :accessor channel-buffer)))

(defgeneric channel-buffered-p (channel)
  (:method ((anything-else t)) nil)
  (:method ((channel buffered-channel)) t))

(defmethod print-object ((channel buffered-channel) stream)
  (print-unreadable-object (channel stream :type t :identity t)
    (let ((buffer (channel-buffer channel)))
      (format stream "[~A/~A]" (queue-count buffer) (queue-length buffer)))))

(defun make-channel (&optional (buffer-size 0))
  (assert (and (typep buffer-size 'fixnum)
               (not (minusp buffer-size))) () "Buffer size must be a non-negative fixnum..")
  (cond ((> buffer-size 0)
         (make-instance 'buffered-channel :buffer (make-queue buffer-size)))
        ((= buffer-size 0)
         (make-instance 'channel))
        (t (error "Hm. It's not supposed to get here."))))

;;;
;;; Messaging
;;;

;;; Sending
(defmacro with-write-state ((channel) &body body)
  `(unwind-protect
        (progn (incf (channel-writers ,channel))
               ,@body)
     (decf (channel-writers ,channel))))

(defgeneric send (chan value &optional blockp)
  (:method ((channel channel) value &optional (blockp t))
    (with-accessors ((lock channel-lock)
                     (recv-ok channel-recv-ok))
        channel
      (bt:with-recursive-lock-held (lock)
        (with-write-state (channel)
          (loop while (send-blocks-p channel)
             if blockp
             do (bt:condition-wait (channel-send-ok channel) lock)
             else do (return-from send nil)))
        (bt:condition-notify recv-ok)
        (channel-insert-value channel value) ; wake up a sleeping reader
        channel)))
  (:method ((channels sequence) value &optional (blockp t))
    (loop do (map nil (fun (when (send _ value nil)
                             (return _)))
                  channels)
       unless blockp
       return nil))
  (:documentation "Tries to send VALUE into CHAN-OR-CHANS. If a sequence of channels is provided
instead of a single channel, SEND will send the value into the first channel that doesn't block.  If
BLOCKP is true, SEND will continue to block until it's able to actually send a value. If BLOCKP is
NIL, SEND will immediately return NIL instead of blocking, if there's no channel available to send
input into. When SEND succeeds, it returns the channel the value was sent into."))

(defgeneric channel-insert-value (channel value)
  ;; We have to do some sleight-of hand here when buffered channels are involved.
  ;; otherwise, we really just set the value to the actual value we want to send.
  ;; This'll temporarily clobber the sentinel (recv should restore it once it
  ;; grabs the value).
  (:method ((channel channel) value)
    (setf (channel-value channel) value))
  (:method ((channel buffered-channel) value)
    (when (queue-full-p (channel-buffer channel))
      (setf (channel-value channel) (dequeue (channel-buffer channel))))
    (enqueue value (channel-buffer channel))))

(defgeneric send-blocks-p (channel)
  ;; This is a bit of a logical mess. The points to note are:
  ;; 1. We must make special accomodations for buffered channels, since
  ;;    they don't block if there's still space in the buffer.
  ;; 2. We block unless *both* of these conditions are filled:
  ;;    a. at least one reader somewhere currently trying to read from the channel.
  ;;    b. the sentinel value is present (meaning we're in the middle of a send already,
  ;;       for some reason. Is this even possible?)
  (:method ((channel channel))
    (not (and (plusp (channel-readers channel))
              (eq (channel-value channel)
                  *secret-unbound-value*))))
  (:method ((channel buffered-channel))
    (and (call-next-method) (queue-full-p (channel-buffer channel))))
  (:documentation "Returns T if trying to SEND to CHANNEL would block. Note that this is not an
atomic operation, and should not be relied on in production. It's mostly meant for
interactive/debugging purposes."))

;;; Sending
(defmacro with-read-state ((channel) &body body)
  `(unwind-protect
        (progn (incf (channel-readers ,channel))
               ,@body)
     (decf (channel-readers ,channel))))

(defgeneric recv (chan &optional blockp)
  (:method ((channel channel) &optional (blockp t))
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
          (values (channel-grab-value channel) channel)))))
  (:method ((channels sequence) &optional (blockp t))
    (loop do (map nil (fun (multiple-value-bind (return-val succeeded) (recv _ nil)
                             (when succeeded (return (values return-val _)))))
                  channels)
       unless blockp
       return (values nil nil)))
  (:documentation "Tries to receive from either a single channel, or a sequence of channels.  If
BLOCKP is true, RECV will block until it's possible to receive something.  Returns two values: The
first is the actual value received through the channel.  The second is the channel the value was
received from. When BLOCKP is NIL, RECV will immediately return (values NIL NIL) instead of
blocking (if it would block)"))

(defgeneric %recv-blocks-p (channel)
  (:method ((channel channel))
    (eq *secret-unbound-value* (channel-value channel)))
  (:method ((channel buffered-channel))
    ;; The reason for this kludge is a bit complicated. Basically, we need a special
    ;; form of recv-blocks-p to use internally that doesn't check the number of writers.
    ;; If we check the writers here, we end up not releasing the lock and letting SEND
    ;; do its thing before we keep going forward.
    (and (queue-empty-p (channel-buffer channel))
         (call-next-method))))

(defgeneric recv-blocks-p (channel)
  (:method ((channel channel))
    (and (not (plusp (channel-writers channel)))
         (%recv-blocks-p channel)))
  (:documentation "Returns T if trying to RECV from CHANNEL would block. Note that this is not an
atomic operation, and should not be relied on in production. It's mostly meant for
interactive/debugging purposes."))

(defgeneric channel-grab-value (channel)
  (:method ((channel channel))
    (prog1 (channel-value channel)
      (setf (channel-value channel) *secret-unbound-value*)))
  (:method :before ((channel buffered-channel))
    ;; This one's a doozy. The special case of having a buffered channel means we need
    ;; to do a bit of juggling. We want the sender to be able to queue something, but
    ;; if our queue is full, we can't let them do that. The solution is to use the value
    ;; slot in the struct. Once the dequeue into channel-value is done, we can treat the
    ;; channel as unbuffered, returning the value we just dequeued, and finally setting
    ;; the channel-value back to the sentinel. We're done here. --sykopomp
    (when (and (not (queue-empty-p (channel-buffer channel)))
               (eq *secret-unbound-value* (channel-value channel)))
      (setf (channel-value channel) (dequeue (channel-buffer channel))))))

