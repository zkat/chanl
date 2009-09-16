;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan
;;;; 
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defgeneric channel-full-p (channel)
  (:method ((channel channel)) (= (channel-buffer-size channel)
                                  (length (channel-buffer channel)))))

(defgeneric send-blocks-p (channel)
  (:method ((channel channel)) (channel-full-p channel)))

(defgeneric recv-blocks-p (channel)
  (:method ((channel channel)) (channel-empty-p channel)))

(defgeneric channel-enqueue (channel obj)
  (:method ((channel channel) obj)
    (with-accessors ((buffer channel-buffer)
                     (chan-full-p channel-full-p)
                     (chan-empty-p channel-empty-p)
                     (lock channel-lock)
                     (enq-ok enq-ok-condition)
                     (deq-ok deq-ok-condition))
        channel
      (bt:with-lock-held (lock)
        (cond (chan-empty-p
               (setf buffer (list obj))
               (bt:condition-notify deq-ok))
              (chan-full-p
               (bt:condition-wait enq-ok lock)
               (setf buffer (nconc buffer (list obj))))
              (t (setf buffer (nconc buffer (list obj)))))))))

(defgeneric channel-dequeue (channel)
  (:method ((channel channel))
    (with-accessors ((buffer channel-buffer)
                     (chan-full-p channel-full-p)
                     (chan-empty-p channel-empty-p)
                     (lock channel-lock)
                     (enq-ok enq-ok-condition)
                     (deq-ok deq-ok-condition))
        channel
      (bt:with-lock-held (lock)
        (cond (chan-empty-p
               (loop (bt:condition-wait deq-ok lock)
                  (unless chan-empty-p (return (pop buffer)))))
              (chan-full-p
               (prog1 (pop buffer)
                 (bt:condition-notify enq-ok)))
              (t (pop buffer)))))))

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
