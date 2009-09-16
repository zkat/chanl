;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;; support for CSP-style channels.
;; ported from plan9's libthread/channel.c via plan9port
;; by roger peppe (rog@vitanuova.com).
;;
;; see http://swtch.com/~rsc/thread/ for a good position paper on
;; the CSP paradigm, or http://www.usingcsp.com/cspbook.pdf
;; for some theory.
;;
;; e.g.
;; create a channel:
;;    (defvar *c* (chan))
;; create a buffered channel with a buffer size of 5
;;    (defvar *c* (chan 5))
;; read a value from a channel (blocks if channel is empty)
;;    (? *c*)
;; write a value to a channel (blocks if channel is full)
;;    (! *c* 99)
;; wait for any of a number of things to occur:
;;    (alt
;;        ((? sync)
;;            (format t "got some value from sync~%"))
;;        ((? c d)
;;            (format t "got ~a from c~%" d))
;;        ((! e val)
;;            (format t "sent val on e~%"))
;;        ((? f (&key arg reply))
;;            (format t "got arg ~a, reply ~a~% from f" f arg reply))
;;        (:*
;;            (format t "would have blocked~%")))"
;; create a new process continually reading values and printing them:
;;    (spawn (loop (format t "~a~%" (? *c*))))
;;
;; TO DO:
;; - thread termination (need to unlock *chanlock* correctly).
;; - default handler for new threads? - wtf?
;; - Get this to play nice with the condition system

(defpackage :chanl
  (:use :common-lisp)
  (:import-from
   :bordeaux-threads
   #:make-thread
   #:thread-name
   #:make-lock
   #:acquire-lock
   #:release-lock
   #:with-lock-held
   #:make-condition-variable
   #:condition-wait
   #:condition-notify
   #:current-thread
   #:interrupt-thread)
  (:export
   #:alt
   #:chan
   #:chanalt
   #:channel
   #:add-inherit
   #:inherit
   #:kill
   #:note
   #:spawn
   #:terminate
   #:send
   #:!
   #:recv
   #:?
   #:*proc*
   #:*dynamic-variables*))

(in-package :chanl)

;;;
;;; Utils
;;;
(defmacro fun (&body body)
  "This macro puts the FUN back in FUNCTION."
  `(lambda (&optional _) (declare (ignorable _)) ,@body))

(defun random-elt (sequence)
  "Returns a random element from SEQUENCE."
  (elt sequence (random (length sequence))))

;;;
;;; Threads
;;;
(defun opposite-op (op)
  (case op (:send :recv) (:recv :send)))

(defun kill (thread)
  (bt:destroy-thread thread))

(defmacro spawn (&body body)
  "Spawn a new process to run each form in sequence. If the first item in the macro body
is a string, and there's more forms to execute, the first item in BODY is used as the
new thread's name."
  (let* ((thread-name (when (and (stringp (car body)) (cdr body)) (car body)))
         (forms (if thread-name (cdr body) body)))
    `(make-thread (lambda () ,@forms)
                  ,@(when thread-name `(:name ,thread-name)))))

;;;
;;; Channels
;;;
(defclass channel ()
  ((buffer :accessor channel-buffer :initform nil)
   (buffer-size :accessor channel-buffer-size :initarg :buffer-size)
   (lock :accessor channel-lock :initform (bt:make-lock))
   (enq-ok-condition :accessor enq-ok-condition :initform (bt:make-condition-variable))
   (deq-ok-condition :accessor deq-ok-condition :initform (bt:make-condition-variable))))

(defgeneric channel-empty-p (channel)
  (:method ((channel channel)) (null (channel-buffer channel))))

(defgeneric channel-enqueue (channel obj)
  (:method ((channel channel) obj)
    (with-accessors ((buffer channel-buffer)
                     (buffer-size channel-buffer-size)
                     (chan-empty-p channel-empty-p)
                     (lock channel-lock)
                     (enq-ok enq-ok-condition))
        channel
      (bt:with-lock-held (lock)
        (cond (chan-empty-p
               (setf buffer (list obj)))
              ((<= (1+ buffer-size) (length buffer))
               (bt:condition-wait enq-ok lock)
               (setf buffer (nconc buffer (list obj))))
              (t (setf buffer (nconc buffer (list obj)))))))))

(defgeneric channel-dequeue (channel)
  (:method ((channel channel))
    (with-accessors ((buffer channel-buffer)
                     (chan-empty-p channel-empty-p)
                     (lock channel-lock)
                     (enq-ok enq-ok-condition)
                     (deq-ok deq-ok-condition))
        channel
      (bt:with-lock-held (lock)
        (if chan-empty-p
            (loop (bt:condition-wait deq-ok lock)
               (unless chan-empty-p (return (pop buffer))))
            (pop buffer))))))

(defmethod print-object ((channel channel) stream)
  (print-unreadable-object (channel stream :type t :identity t)
    (format stream "~a" (length (channel-buffer channel)))))

(defun chan (&optional (buffer-size 0))
  "Create a new channel. The optional argument gives the size
   of the channel's buffer (default 0)"
  (make-instance 'channel :buffer-size buffer-size))

(defun recv (channel)
  "Receive a value from the CHANNEL"
  (channel-dequeue channel))

(defun send (channel value)
  "Send VALUE down CHANNEL"
  (channel-enqueue channel value) value)

;;; These shall remain until I know I can get rid of them.
(defun ! (channel value)
  (send channel value))

(defun ? (channel)
  (recv channel))
