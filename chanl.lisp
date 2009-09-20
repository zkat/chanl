;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Josh Marchan
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
   #:spawn #:kill #:all-procs
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
    `(bt:make-thread (lambda () ,@forms)
                     ,@(when thread-name `(:name ,thread-name)))))

(defun all-procs ()
  (bt:all-threads))

;;;
;;; Channels
;;;
(defparameter *secret-unbound-value* (gensym "SEKRIT"))

(defstruct (channel (:constructor chan))
  (value *secret-unbound-value*)
  being-read-p
  (lock (bt:make-lock))
  (send-ok-condition (bt:make-condition-variable))
  (recv-ok-condition (bt:make-condition-variable)))

(defun send-ok-condition (channel)
  (channel-send-ok-condition channel))
(defun recv-ok-condition (channel)
  (channel-recv-ok-condition channel))

;; <pkhuong> sykopomp: you'll need two sentinels, since your channels are synchronous. One for a
;;     channel that's ready to receive a value, and another for a channel that's been read, but
;;     whose writer hasn't been notified yet.
;; I DON'T GET IT
(defun send (channel obj)
  (with-accessors ((value channel-value)
                   (being-read-p channel-being-read-p)
                   (lock channel-lock)
                   (send-ok send-ok-condition)
                   (recv-ok recv-ok-condition))
      channel
    (bt:with-lock-held (lock)
      (loop
         until (and (eq value *secret-unbound-value*)
                    being-read-p)
         do (bt:condition-wait send-ok lock)
         finally (setf being-read-p nil) (return (setf value obj)))
      (bt:condition-notify recv-ok)
      obj)))

(defun recv (channel)
  (with-accessors ((value channel-value)
                   (being-read-p channel-being-read-p)
                   (lock channel-lock)
                   (send-ok send-ok-condition)
                   (recv-ok recv-ok-condition))
      channel
    (bt:with-lock-held (lock)
      (prog1
          (loop
             while (or (eq *secret-unbound-value* value)
                       being-read-p)
             do (bt:condition-wait recv-ok lock)
             finally (setf value *secret-unbound-value*) (return value))
        (bt:condition-notify send-ok)
        (setf read-already t)))))

(defmethod print-object ((channel channel) (s stream))
  (print-unreadable-object (channel s :type t :identity t)))
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

