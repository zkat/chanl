;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :chanl
  (:use :common-lisp)
  (:import-from
   :bordeaux-threads
   #:make-thread
   #:make-lock
   #:with-lock-held
   #:make-condition-variable
   #:condition-wait
   #:condition-notify
   #:current-thread)
  (:export
   ;; processes
   #:spawn #:kill
   ;; channels
   #:chan #:send #:recv
   #:channel #:channel-empty-p #:channel-full-p
   #:send-blocks-p #:recv-blocks-p))

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

(defgeneric send (channel obj)
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
              (t (setf buffer (nconc buffer (list obj)))))))
    obj))

(defgeneric recv (channel)
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
    (format stream "~A/~A" (length (channel-buffer channel)) (channel-buffer-size channel))))

(defun chan (&optional (buffer-size 0))
  "Create a new channel. The optional argument gives the size
   of the channel's buffer (default 0)"
  (make-instance 'channel :buffer-size buffer-size))

;;;
;;; muxing macro
;;;
(defmacro mux (&body body)
  (let ((sends (remove-if-not 'send-clause-p body))
        (recvs (remove-if-not 'recv-clause-p body))
        (else (remove-if-not 'else-clause-p body)))
    ))

(defun send-clause-p (clause)
  (eq 'send (caar clause)))
(defun recv-clause-p (clause)
  (eq 'recv (caar clause)))
(defun else-clause-p (clause)
  (eq t (car clause)))

