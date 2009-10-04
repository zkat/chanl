;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan, Adlai Chandrasekhar
;;;;
;;;; Channel Definition
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :chanl)

;;;
;;; Abstract channel interface
;;;
(defclass abstract-channel () ())

(defgeneric channelp (channel)
  (:method ((anything-else t)) nil)
  (:method ((channel abstract-channel)) t))

(defgeneric send (chan value &optional blockp)
  (:method ((channels sequence) value &optional (blockp t))
    (loop do (mapc (fun (when (send _ value nil) (return _)))
                   channels)
       unless blockp return nil))
  (:documentation "Tries to send VALUE into CHAN. If a sequence of channels is provided
instead of a single channel, SEND will send the value into the first channel that doesn't block.  If
BLOCKP is true, SEND will continue to block until it's able to actually send a value. If BLOCKP is
NIL, SEND will immediately return NIL instead of blocking, if there's no channel available to send
input into. When SEND succeeds, it returns the channel the value was sent into."))

(defgeneric recv (chan &optional blockp)
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

;;;
;;; Unbuffered channels
;;;
(defvar *secret-unbound-value* (gensym "SECRETLY-UNBOUND-")
  "This value is used as a sentinel in channels.")

(defclass channel (abstract-channel)
  ((value :initform *secret-unbound-value* :accessor channel-value)
   (readers :initform 0 :accessor channel-readers)
   (writers :initform 0 :accessor channel-writers)
   (lock :initform (bt:make-recursive-lock) :accessor channel-lock)
   (send-ok :initform (bt:make-condition-variable) :accessor channel-send-ok)
   (recv-ok :initform (bt:make-condition-variable) :accessor channel-recv-ok)))

(defun channel-being-read-p (channel)
  (plusp (channel-readers channel)))

(defun channel-being-written-p (channel)
  (plusp (channel-writers channel)))

;;; Sending
(defmacro with-write-state ((channel) &body body)
  `(unwind-protect
        (progn (incf (channel-writers ,channel))
               ,@body)
     (decf (channel-writers ,channel))))

(defmethod send ((channel channel) value &optional blockp)
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
      (channel-insert-value channel value)
      channel)))

(defgeneric channel-insert-value (channel value)
  (:method ((channel channel) value)
    (setf (channel-value channel) value)))

(defgeneric send-blocks-p (channel)
  (:method ((channel channel))
    (not (and (channel-being-read-p channel)
              (eq (channel-value channel)
                  *secret-unbound-value*))))
  (:documentation "Returns T if trying to SEND to CHANNEL would block. Note that this is not an
atomic operation, and should not be relied on in production. It's mostly meant for
interactive/debugging purposes."))

;;; Receiving
(defmacro with-read-state ((channel) &body body)
  `(unwind-protect
        (progn (incf (channel-readers ,channel))
               ,@body)
     (decf (channel-readers ,channel))))

(defmethod recv ((channel channel) &optional blockp)
  (with-accessors ((lock channel-lock)
                   (send-ok channel-send-ok))
      channel
    (bt:with-recursive-lock-held (lock)
      (with-read-state (channel)
        (bt:condition-notify send-ok)
        (loop while (recv-blocks-p channel)
           do (if (or blockp (channel-being-written-p channel))
                  (bt:condition-wait (channel-recv-ok channel) lock)
                  (return-from recv (values nil nil))))
        (values (channel-grab-value channel) channel)))))

(defgeneric recv-blocks-p (channel)
  (:method ((channel channel))
    (eq *secret-unbound-value* (channel-value channel))))

(defgeneric channel-grab-value (channel)
  (:method ((channel channel))
    (prog1 (channel-value channel)
      (setf (channel-value channel) *secret-unbound-value*))))

;;;
;;; Buffered channels
;;;
(defclass buffered-channel (channel) ()
  (:documentation "Abstract class for channels using various buffering styles."))

(defgeneric channel-buffered-p (channel)
  (:method ((anything-else t)) nil)
  (:method ((channel buffered-channel)) t))

(defgeneric channel-peek (channel)
  (:documentation
   "Peek at the next value CHANNEL would dequeue. Note that this cannot
be used atomically. Returns two values: The first is the value of interest
or NIL, the second is a generalized boolean that is NIL when there is no
available value in the queue."))

;;;
;;; Stack-buffered channels
;;;
(defclass stack-channel (buffered-channel) ())

(defmethod initialize-instance :after ((channel stack-channel) &key)
  (setf (channel-value channel) nil))

(defmethod print-object ((channel stack-channel) stream)
  (print-unreadable-object (channel stream :type t :identity t)
    (format stream "[~A]" (length (channel-value channel)))))

(defmethod channel-peek ((channel stack-channel))
  (if (channel-value channel)
      (values (car (channel-value channel)) t)
      (values nil nil)))

(defgeneric channel-pop (channel)
  (:method ((channel stack-channel))
    (pop (channel-value channel))))

(defgeneric channel-push (value channel)
  (:method (value (channel stack-channel))
    (push value (channel-value channel))))

(defmethod channel-insert-value ((channel stack-channel) value)
  (channel-push value channel))
(defmethod channel-grab-value ((channel stack-channel))
  (channel-pop channel))

(defmethod send-blocks-p ((channel stack-channel)) nil)
(defmethod recv-blocks-p ((channel stack-channel))
  (null (channel-value channel)))

;;;
;;; Queue-buffered channels.
;;;
(defclass queue-channel (buffered-channel) ()
  (:documentation "These channels buffer objects in some sort of queue."))

(defgeneric channel-enqueue (value channel)
  (:documentation "Enqueue VALUE in CHANNEL's buffer queue."))
(defgeneric channel-dequeue (channel)
  (:documentation "Dequeue a value from CHANNEL's buffer queue."))

(defmethod channel-insert-value ((channel queue-channel) value)
  (channel-enqueue value channel))

(defmethod channel-grab-value ((channel queue-channel))
  (channel-dequeue channel))

;;;
;;; Bounded Buffered Channels
;;;
(defconstant +maximum-buffer-size+ (- array-total-size-limit 2)
  "The exclusive upper bound on the size of a channel's buffer.")

(defclass bounded-channel (queue-channel) ())

(defmethod initialize-instance :after ((channel bounded-channel) &key size)
  (assert (typep size `(integer 1 ,(1- +maximum-buffer-size+))) (size)
          "Buffer size must be a non-negative fixnum..")
  (setf (channel-value channel) (make-queue size)))

(define-print-object ((channel bounded-channel) :type t :identity t)
  (let ((buffer (channel-value channel)))
    (format t "[~A/~A]" (queue-count buffer) (queue-length buffer))))

(defmethod channel-peek ((channel bounded-channel))
  (queue-peek (channel-value channel)))

;;; Sending
(defmethod send-blocks-p ((channel bounded-channel))
  (queue-full-p (channel-value channel)))

(defmethod channel-enqueue (value (channel bounded-channel))
  (enqueue value (channel-value channel)))

;;; Receiving
(defmethod recv-blocks-p ((channel bounded-channel))
  (queue-empty-p (channel-value channel)))

(defmethod channel-dequeue ((channel bounded-channel))
  (dequeue (channel-value channel)))

;;;
;;; Unbounded Channels
;;;
(defclass unbounded-channel (queue-channel) ())

(defmethod initialize-instance :after ((channel unbounded-channel) &key)
  (setf (channel-value channel) (cons nil nil)))

(define-print-object ((channel unbounded-channel) :type t :identity t)
  (format t "[~A]" (length (car (channel-value channel)))))

(defmethod channel-peek ((channel unbounded-channel))
  (car (channel-value channel)))

;;; Sending
(defmethod send-blocks-p ((channel unbounded-channel)) nil)

(defmethod channel-enqueue (value (channel unbounded-channel))
  (let ((queue (channel-value channel)))
    (pushend value (car queue) (cdr queue))))

;;; Receiving
(defmethod recv-blocks-p ((channel unbounded-channel))
  (null (car (channel-value channel))))

(defmethod channel-dequeue ((channel unbounded-channel))
  (pop (car (channel-value channel))))
