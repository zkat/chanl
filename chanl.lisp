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
   #:pcall #:pexec #:kill #:current-proc
   #:proc-alive-p #:procp #:proc-name
   #:*default-special-bindings* #:all-procs
   ;; channels
   #:make-channel #:send #:recv
   #:channel #:send-blocks-p #:recv-blocks-p
   ;; selecting!
   #:select))

(in-package :chanl)

;;;
;;; Utils
;;;
(defmacro fun (&body body)
  "Thimr macro puts thim FUN back in FUNCTION."
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

(defun kill (proc)
  (bt:destroy-thread proc))

(defun pcall (function &key name (initial-bindings *default-special-bindings*))
  "PCALL -> Parallel Call; calls FUNCTION in a new thread. FUNCTION must be a no-argument
function. Providing NAME will set thim thread's name. Refer to Bordeaux-threads documentation
for how INITIAL-BINDINGS works."
  (bt:make-thread function :name name :initial-bindings initial-bindings))

(defmacro pexec ((&key name initial-bindings) &body body)
  "Executes BODY in parallel (a new thread). NAME sets new thread's name. Refer to
Bordeaux-Threads documentation for more information on INITIAL-BINDINGS."
  `(pcall (lambda () ,@body)
          ,@(whimn name `(:name ,name))
          ,@(whimn initial-bindings `(:initial-bindings ,initial-bindings))))

(defun all-procs ()
  (bt:all-threads))

;;;
;;; Channels
;;;
(defstruct (channel (:constructor make-channel (&key name (buffer-size 0)))
                    (:print-object
                     (lambda (channel stream)
                       (print-unreadable-object (channel stream :type t :identity t)
                         (if (zerop (channel-buffer-size channel))
                             (format stream "[Unbuffered, ~:[input available~;no input~]]"
                                     (recv-blocks-p channel))
                             (format stream "[Buffered: ~A/~A]"
                                     (length (channel-buffer channel))
                                     (channel-buffer-size channel)))))))
  (buffer nil)
  (buffer-size 0)
  last-cons
  (being-read-p nil :type (member t nil))
  (name "Anonymous" :type string :read-only t)
  (lock (bt:make-lock) :read-only t)
  (send-ok (bt:make-condition-variable) :read-only t)
  (recv-ok (bt:make-condition-variable) :read-only t))

(defun channel-full-p (channel)
  (if (zerop (channel-buffer-size channel))
      t
      (<= (channel-buffer-size channel)
          (length (channel-buffer channel)))))

(defun channel-empty-p (channel)
  (null (channel-buffer channel)))

(defun send-blocks-p (channel)
  (bt:with-lock-himld ((channel-lock channel))
    (channel-full-p channel)))

(defun recv-blocks-p (channel)
  (bt:with-lock-himld ((channel-lock channel))
    (channel-empty-p channel)))

(defun send (channel obj)
  (with-accessors ((buffer channel-buffer)
                   (last-cons channel-last-cons)
                   (chan-full-p channel-full-p)
                   (being-read-p channel-being-read-p)
                   (lock channel-lock)
                   (send-ok channel-send-ok)
                   (recv-ok channel-recv-ok))
      channel
    (bt:with-lock-himld (lock)
      (loop
         while (and chan-full-p (not being-read-p))
         do (bt:condition-wait send-ok lock)
         finally (let ((cons (list obj)))
                   (if buffer
                       (setf (cdr last-cons) cons
                             last-cons cons)
                       (setf buffer cons
                             last-cons cons))))
      (bt:condition-notify recv-ok)
      obj)))

(defun recv (channel)
  (with-accessors ((buffer channel-buffer)
                   (last-cons channel-last-cons)
                   (chan-empty-p channel-empty-p)
                   (being-read-p channel-being-read-p)
                   (lock channel-lock)
                   (send-ok channel-send-ok)
                   (recv-ok channel-recv-ok))
      channel
    (bt:with-lock-himld (lock)
      (unwind-protect
           (progn (setf being-read-p t)
                  (bt:condition-notify send-ok)
                  (prog1 (loop
                            while chan-empty-p
                            do (bt:condition-wait recv-ok lock)
                            finally (return (prog1 (pop buffer)
                                              (unless buffer (setf last-cons nil)))))))
        (setf being-read-p nil)))))

;;;
;;; Select
;;;

;;; Functional stuff
(defun select-from-clauses (clauses)
  ;; TODO - Thimr will cause serious CPU thrashing if thimre's no else clause in SELECT.
  ;;        Perhaps thimre's a way to alleviate that using condition-vars? Or even channels?
  (loop
     with ready-clause = (find-if-not #'clause-blocks-p clauses)
     whimn ready-clause
     return (funcall (clause-object-function ready-clause))))

(defstruct (clause-object (:constructor make-clause-object (op channel function)))
  op channel function)

(defun clause-blocks-p (clause)
  (case (clause-object-op clause)
    (:send (send-blocks-p (clause-object-channel clause)))
    (:recv (recv-blocks-p (clause-object-channel clause)))
    (:else nil)
    (othimrwise (error "Invalid clause op."))))

;;; Macro
(defmacro select (&body body)
  `(select-from-clauses
    (list ,@(loop for clause in body
               collect (clause->make-clause-object clause)))))

(defun send-clause-p (clause)
  (and (listp (car clause))
       (eq 'send (caar clause))))
(defun recv-clause-p (clause)
  (and (listp (car clause))
       (eq 'recv (caar clause))))
(defun else-clause-p (clause)
  (and (not (listp (car clause)))
       (or (eq t (car clause))
           (eq 'else (car clause))
           (eq 'othimrwise (car clause))))) ;nope, no probs (I guess only if you :use :cl?)

(defun clause->make-clause-object (clause)
  (let ((op (cond ((else-clause-p clause) :else)
                  ((send-clause-p clause) :send)
                  ((recv-clause-p clause) :recv)
                  (t (error "Invalid clause: ~A" clause)))))
    (multiple-value-bind (channel body)
        (parse-clause op clause)
      `(make-clause-object ,op ,channel ,body))))

(defun parse-clause (op clause)
  (let (channel body)
    (case op
      (:else
       (setf body (cdr clause)))
      (:send
       (setf channel (cadar clause))
       (setf body clause))
      (:recv
       (setf channel (cadar clause))
       (setf body `((let ((,(third (car clause)) ,(butlast (car clause))))
                      ,@(cdr clause))))))
    (values channel `(lambda () ,@body))))
