;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :chanl
  (:use :common-lisp)
  (:import-from :bordeaux-threads :*default-special-bindings*)
  (:export
   ;; processes
   #:proc-call #:proc-exec #:kill #:current-proc
   #:proc-alive-p #:procp #:proc-name
   #:*default-special-bindings* #:all-procs
   ;; channels
   #:make-channel #:send #:recv
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
(defun current-proc ()
  (bt:current-thread))

(defun proc-alive-p (proc)
  (bt:thread-alive-p proc))

(defun procp (proc)
  (bt:threadp proc))

(defun proc-name (proc)
  (bt:thread-name proc))

(defun kill (thread)
  (bt:destroy-thread thread))

(defun proc-call (function &key name (initial-bindings *default-special-bindings*))
  (bt:make-thread function :name name :initial-bindings initial-bindings))

(defmacro proc-exec ((&key name initial-bindings) &body body)
  `(proc-call (lambda () ,@body)
              ,@(when name `(:name ,name))
              ,@(when initial-bindings `(:initial-bindings ,initial-bindings))))

(defun all-procs ()
  (bt:all-threads))

;;;
;;; Channels
;;;
(defparameter *secret-unbound-value* (make-symbol "unbound value"))

(defstruct (channel (:constructor make-channel (&optional name))
                    (:print-object
                     (lambda (channel stream)
                       (print-unreadable-object (channel stream :type t :identity t)
                         (format stream "~A" (channel-name channel))))))
  (value *secret-unbound-value*)
  (being-read-p nil :type (member t nil))
  (name "Anonymous" :type string :read-only t)
  (lock (bt:make-lock) :read-only t)
  (send-ok (bt:make-condition-variable) :read-only t)
  (recv-ok (bt:make-condition-variable) :read-only t))

(defun send (channel obj)
  (with-accessors ((value channel-value)
                   (being-read-p channel-being-read-p)
                   (lock channel-lock)
                   (send-ok channel-send-ok)
                   (recv-ok channel-recv-ok))
      channel
    (bt:with-lock-held (lock)
      (loop
         until (and being-read-p (eq value *secret-unbound-value*))
         do (bt:condition-wait send-ok lock)
         finally (setf value obj))
      (bt:condition-notify recv-ok)
      obj)))

(defun recv (channel)
  (with-accessors ((value channel-value)
                   (being-read-p channel-being-read-p)
                   (lock channel-lock)
                   (send-ok channel-send-ok)
                   (recv-ok channel-recv-ok))
      channel
    (bt:with-lock-held (lock)
      (setf being-read-p t)
      (bt:condition-notify send-ok)
      (prog1 (loop
                while (eq *secret-unbound-value* value)
                do (bt:condition-wait recv-ok lock)
                finally (return value))
        (setf value           *secret-unbound-value*
              being-read-p    nil)))))

;;;
;;; muxing macro
;;;
;; TODO - write this out. It should turn each clause into an actual object that can then be
;;        iterated over. I first need to figure out how to check whether send/recv would block.
;;        That should be easy enough, though. Maybe it'll help me figure out the CCL annoyance.
;;        -- zkat
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
