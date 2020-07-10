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

(defgeneric send (chan value &key)
  (:method ((channels null) (value t) &key) (warn "Ignored SEND to empty list"))
  (:method ((channels sequence) value &key (blockp t))
    (loop do (mapc (fun (when (send _ value :blockp nil) (return _)))
                   channels)
       unless blockp return nil))
  (:documentation "Tries to send VALUE into CHAN. If a sequence of channels is provided
instead of a single channel, SEND will send the value into the first channel that doesn't block.  If
BLOCKP is true, SEND will continue to block until it's able to actually send a value. If BLOCKP is
NIL, SEND will immediately return NIL instead of blocking, if there's no channel available to send
input into. When SEND succeeds, it returns the channel the value was sent into."))

(defgeneric recv (chan &key)
  (:method ((channels null) &key) (warn "Ignored RECV from empty list"))
  (:method ((channels sequence) &key (blockp t))
    (loop do (map nil (fun (multiple-value-bind (return-val succeeded) (recv _ :blockp nil)
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
   (lock :initform (bt:make-recursive-lock))
   (send-ok :initform (bt:make-condition-variable))
   (recv-ok :initform (bt:make-condition-variable))
   (send-return-wait :initform (bt:make-condition-variable)
                     :accessor channel-send-return-wait)
   (recv-grabbed-value-p :initform nil :accessor recv-grabbed-value-p)))

(defun channel-being-read-p (channel)
  (plusp (channel-readers channel)))

(defun channel-being-written-p (channel)
  (plusp (channel-writers channel)))

;;; Semaphores that delegate locking to the calling context
(macrolet ((define-channel-state-macro (name place)
             `(defmacro ,name (channel &body body)
                `(unwind-protect (progn (incf (,',place ,channel)) ,@body)
                   (decf (,',place ,channel))
                   (when (minusp (,',place ,channel))
                     (error "Something bad happened"))))))
  (define-channel-state-macro with-write-state channel-writers)
  (define-channel-state-macro with-read-state channel-readers))

(defmacro with-channel-slots ((lock recv-ok send-ok) channel &body body)
  `(with-slots ((,lock lock) (,recv-ok recv-ok) (,send-ok send-ok)) ,channel
     (bt:with-recursive-lock-held (,lock) ,@body)))

;;; Sending
(defmethod send ((channel channel) value &key (blockp t))
  (with-channel-slots (lock recv-ok send-ok) channel
    (with-write-state channel
      (loop while (send-blocks-p channel)
         do (if (or blockp (channel-being-read-p channel))
                (bt:condition-wait send-ok lock)
                (return-from send nil))))
    (bt:condition-notify recv-ok)
    (let ((block-status (channel-being-read-p channel)))
      (channel-insert-value channel value)
      (when (and block-status (send-blocks-p channel))
        (loop until (recv-grabbed-value-p channel)
           do (bt:condition-wait (channel-send-return-wait channel) lock)
           finally (setf (recv-grabbed-value-p channel) nil))))
    (when (and (channel-being-read-p channel)
               (channel-being-written-p channel))
      (bt:condition-notify recv-ok))
    channel))

(defgeneric channel-insert-value (channel value)
  (:method ((channel channel) value)
    (setf (channel-value channel) value)))

(defgeneric send-blocks-p (channel)
  (:method ((channel channel))
    (not (and (channel-being-read-p channel)
              (eq (channel-value channel)
                  *secret-unbound-value*))))
  (:documentation "Returns T if trying to SEND to CHANNEL would block.

Assumes that the calling context holds the channel's lock."))

;;; Receiving
(defmethod recv ((channel channel) &key (blockp t))
  (with-channel-slots (lock recv-ok send-ok) channel
    (with-read-state channel
      (loop while (recv-blocks-p channel)
         do (bt:condition-notify send-ok)
         do (if (or blockp (channel-being-written-p channel))
                (bt:condition-wait recv-ok lock)
                (return-from recv (values nil nil))))
      (multiple-value-prog1
          (values (channel-grab-value channel) channel)
        (setf (recv-grabbed-value-p channel) t)
        (bt:condition-notify (channel-send-return-wait channel))))))

(defgeneric recv-blocks-p (channel)
  (:method ((channel channel))
    (eq *secret-unbound-value* (channel-value channel)))
  (:documentation "Returns T if trying to RECV from CHANNEL would block.

Assumes that the calling context holds the channel's lock."))

(defgeneric channel-grab-value (channel)
  (:method ((channel channel))
    (shiftf (channel-value channel) *secret-unbound-value*)))

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
   "Peek at a possible next value CHANNEL would dequeue. An actual call to RECV
may return a different value, if the previously-peeked one has been received by
a different thread in the meantime.

Returns two values: the value of interest or NIL, and a generalized boolean that
is NIL when there is no available value in the queue."))

;;;
;;; Stack-buffered channels
;;;
(defclass stack-channel (buffered-channel) ())

(defmethod initialize-instance :after ((channel stack-channel) &key)
  (setf (channel-value channel) nil))

(define-print-object ((channel stack-channel))
  (format t "[~A]" (length (channel-value channel))))

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

(defmethod initialize-instance :after ((channel bounded-channel) &key (size 1))
  (assert (typep size `(integer 1 ,(1- +maximum-buffer-size+))) (size)
          "Buffer size must be a non-negative fixnum..")
  (setf (channel-value channel) (make-queue size)))

(define-print-object ((channel bounded-channel))
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

(define-print-object ((channel unbounded-channel))
  (format t "[~A]" (length (car (channel-value channel)))))

(defmethod channel-peek ((channel unbounded-channel))
  (if (car (channel-value channel))
      (values (caar (channel-value channel)) t)
      (values nil nil)))

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

;;;
;;; CAS Channels
;;;
(defclass cas-channel (abstract-channel)
  ((vector :initform (vector *secret-unbound-value* 0 0 nil) :accessor channel-vector))
  (:documentation
   "These channels use COMPARE-AND-SWAP to do their thing, instead of locks+condition-vars.
Ideally, these would be faster than regular channels. In reality, they're not. It's possible
there might be a way to speed these guys up while keeping the same behavior in the interface,
but for now, they're about 100x slower, not to mention non-portable."))

#+ (and ccl (or x86 x86-64))
(defmethod initialize-instance :before ((channel cas-channel) &key)
  (warn "COMPARE-AND-SWAP on x86-based CCL is experimental and buggy. Beware."))

#- (or sbcl (and ccl (or x86 x86-64)))
(defmethod initialize-instance ((channel cas-channel) &key)
  (error "COMPARE-AND-SWAP is not supported on this platform yet.~%Platform details: ~A ~A"
         (lisp-implementation-version) (lisp-implementation-type)))

(defmethod channel-value ((channel cas-channel))
  (svref (channel-vector channel) 0))
(defmethod channel-readers ((channel cas-channel))
  (svref (channel-vector channel) 1))
(defmethod channel-writers ((channel cas-channel))
  (svref (channel-vector channel) 2))
(defmethod recv-grabbed-value-p ((channel cas-channel))
  (svref (channel-vector channel) 3))

(defun cas-channel-set (slot-name channel value)
  (let ((index (case slot-name (value 0) (readers 1) (writers 2) (recv-grabbed-value-p 3))))
    (loop for old = (svref (channel-vector channel) index)
         when (eq old (compare-and-swap (svref (channel-vector channel) index) old value))
         return value)))

(macrolet ((define-cas-channel-state-macro (name place)
             `(defmacro ,name (channel &body body)
                `(unwind-protect
                      (progn
                        (atomic-incf (svref (channel-vector ,channel) ,',place))
                        ,@body)
                   (atomic-incf (svref (channel-vector ,channel) ,',place) -1)
                   (when (minusp (svref (channel-vector ,channel) ,',place))
                     (error "Something bad happened"))))))
  (define-cas-channel-state-macro with-cas-write-state 2)
  (define-cas-channel-state-macro with-cas-read-state 1))

;;; writing
(defmethod send ((channel cas-channel) value &key (blockp t))
  (with-cas-write-state channel
    (loop while (send-blocks-p channel)
       unless (or blockp (channel-being-read-p channel))
       do (return-from send nil))
    (let ((block-status (channel-being-read-p channel)))
      (channel-insert-value channel value)
      (when block-status
        (loop until (recv-grabbed-value-p channel)
           finally (cas-channel-set 'recv-grabbed-value-p channel nil))))
    channel))

(defmethod send-blocks-p ((channel cas-channel))
  (not (and (channel-being-read-p channel)
            (eq (channel-value channel)
                *secret-unbound-value*))))

(defmethod channel-insert-value ((channel cas-channel) value)
  (cas-channel-set 'value channel value))

;;; reading
(defmethod recv ((channel cas-channel) &key (blockp t))
  (with-cas-read-state channel
    (loop while (recv-blocks-p channel)
       unless (or blockp (channel-being-written-p channel))
       do (return-from recv (values nil nil)))
    (multiple-value-prog1
        (values (channel-grab-value channel) channel)
      (cas-channel-set 'recv-grabbed-value-p channel t))))

(defmethod recv-blocks-p ((channel cas-channel))
  (eq *secret-unbound-value* (channel-value channel)))

(defmethod channel-grab-value ((channel cas-channel))
  (prog1 (channel-value channel)
    (cas-channel-set 'value channel *secret-unbound-value*)))
