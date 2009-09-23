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

;; TODO - this whole thing locks up regularly on CCL and I have no idea why.
;;        as far as I can tell, I think I'm using this stuff right, and when
;;        I read through it, I don't see any reason for a deadlock to happen.
;;        At least no obvious reason. I really don't want to have to read
;;        all of art of multiproc programming just to figure out this one
;;        retarded thing.
;;        At least it works on SBCL :( -- zkat
(defun send (channel obj)
  (with-accessors ((value channel-value)
                   (being-read-p channel-being-read-p)
                   (lock channel-lock)
                   (send-ok channel-send-ok-condition)
                   (recv-ok channel-recv-ok-condition))
      channel
    (bt:with-lock-held (lock)
      (loop
         until (and being-read-p (eq value *secret-unbound-value*))
         do (bt:condition-wait send-ok lock))
      (setf value obj)
      (bt:condition-notify recv-ok)
      obj)))

(defun recv (channel)
  (with-accessors ((value channel-value)
                   (being-read-p channel-being-read-p)
                   (lock channel-lock)
                   (send-ok channel-send-ok-condition)
                   (recv-ok channel-recv-ok-condition))
      channel
    (bt:with-lock-held (lock)
      ;; Begin ANSI-non-compliant code.
      ;; Please refer to http://www.lispworks.com/documentation/HyperSpec/Body/m_prog1c.htm
      ;; for further details on this incompatibility, and refer to your implementation's
      ;; documentation to determine whether this will cause breakage.
      ;; -- zkat
      (setf being-read-p t)
      (bt:condition-notify send-ok)
      (prog2 (loop
                while (eq *secret-unbound-value* value)
                do (bt:condition-wait recv-ok lock))
          value
        ;; End ANSI-non-compliant code.
        (setf value           *secret-unbound-value*
              being-read-p    nil)))))

(defmethod print-object ((channel channel) stream)
  (print-unreadable-object (channel stream :type t :identity t)))

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

