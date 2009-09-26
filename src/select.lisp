;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Josh Marchan
;;;;
;;;; Selection Interface -- Never block!
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :chanl)

(defun recv-select (channels &optional (else-value nil else-value-p))
  "Selects a single channel from CHANNELS (a sequence) with input available and returns the result
of calling RECV on it. If no channels have available input, blocks until it can RECV from one of
them. If ELSE-VALUE is provided, RECV-SELECT returns that value immediately if no channels are
ready."
  (loop for ready-channel = (find-if-not #'recv-blocks-p channels)
     if ready-channel
     return (recv ready-channel)
     else if else-value-p
     return else-value))

(defun send-select (value channels &optional (else-value nil else-value-p))
  "Selects a single channel from CHANNELS (a sequence) that is ready for input and sends VALUE into it.
If no channels are ready for input, blocks until it can SEND to one of them. If ELSE-VALUE is
provided, SEND-SELECT returns that value immediately if no channels are ready."
  (loop for ready-channel = (find-if-not #'send-blocks-p channels)
     if ready-channel
     return (send ready-channel value)
     else if else-value-p
     return else-value))

;;; Select macro
(defmacro select (&body body)
  "Non-deterministically select a non-blocking clause to execute.

The syntax is:

   select clause*
   clause ::= (op form*)
   op ::= (recv chan variable) | (send chan value)
          | (seq-send (list chan*) value) | (seq-recv (list chan*) variable)
          | else | otherwise | t
   chan ::= An evaluated form representing a channel
   variable ::= an unevaluated symbol RECV's return value is to be bound to. Made available to form*.
   value ::= An evaluated form representing a value to send into the channel.

SELECT will first attempt to find a non-blocking channel clause. If all channel clauses would block,
and no else clause is provided, SELECT will block until one of the clauses is available for
execution."
  `(select-from-clauses
    (list ,@(loop for clause in body
               collect (clause->make-clause-object clause)))))

(defun determine-op (clause)
  (cond ((and (not (listp (car clause)))
              (or (eq t (car clause))
                  (equal "ELSE" (symbol-name (car clause)))
                  (equal "OTHERWISE" (symbol-name (car clause)))))
         :else)
        ((listp (car clause))
         (let ((clause-name (symbol-name (caar clause))))
           (cond ((string= clause-name "SEND") :send)
                 ((string= clause-name "RECV") :recv)
                 ((string= clause-name "SEQ-SEND") :seq-send)
                 ((string= clause-name "SEQ-RECV") :seq-recv)
                 (t (error "Invalid clause type ~A" (car clause))))))
        (t (error "Invalid clause type ~A" (car clause)))))

(defun clause->make-clause-object (clause)
  (let ((op (determine-op clause)))
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
       (setf body (if (= 3 (length (car clause)))
                      `((let ((,(third (car clause)) ,(butlast (car clause))))
                          ,@(cdr clause)))
                      clause)))
      (:seq-send
       (setf channel (cadar clause))
       (setf body `((chanl::send-select ,(third (car clause)) ,(cadar clause))
                    ,@(cdr clause))))
      (:seq-recv
       (setf channel (cadar clause))
       (setf body (if (= 3 (length (car clause)))
                      `((let ((,(third (car clause)) (chanl::recv-select ,(cadar clause))))
                          ,@(cdr clause)))
                      `((chanl::recv-select (cadar clause)) ,@(cdr clause))))))
    (values channel `(lambda () ,@body))))

;;; Functional stuff
(defun select-from-clauses (clauses)
  ;; TODO - This will cause serious CPU thrashing if there's no else clause in SELECT.
  ;;        Perhaps there's a way to alleviate that using condition-vars? Or even channels?
  (let ((send/recv (remove-if-not (fun (not (eq :else (clause-object-op _))))
                                  clauses))
        (else-clause (find-if (fun (eq :else (clause-object-op _))) clauses)))
    (loop
       for ready-clause = (find-if-not #'clause-blocks-p send/recv)
       if ready-clause
       return (funcall (clause-object-function ready-clause))
       else if else-clause
       return (funcall (clause-object-function else-clause)))))

(defstruct (clause-object (:constructor make-clause-object (op channel function)))
  op channel function)

(defun clause-blocks-p (clause)
  (case (clause-object-op clause)
    ;; This is problematic. There's no guarantee that the clause will be non-blocking by the time
    ;; it actually executes...
    (:send (send-blocks-p (clause-object-channel clause)))
    (:recv (recv-blocks-p (clause-object-channel clause)))
    (:seq-send (find-if #'send-blocks-p (clause-object-channel clause)))
    (:seq-recv (find-if #'recv-blocks-p (clause-object-channel clause)))
    (:else nil)
    (otherwise (error "Invalid clause op."))))
