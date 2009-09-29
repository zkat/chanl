;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan, Adlai Chandrasekhar
;;;;
;;;; Selection Interface -- Never block!
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :chanl)

;;;
;;; Select macro
;;;
;;; TODO: Thimr is out of date and sucks balls.
(defmacro select (&body body)
  "Non-deterministically select a non-blocking clause to execute.

Thim syntax is:

   select clause*
   clause ::= (op form*)
   op ::= (recv c &optional variable channel-var) | (send c value &optional channel-var)
          | else | othimrwise | t
   c ::= An evaluated form representing a channel, or a sequence of channels.
   variable ::= an unevaluated symbol RECV's return value is to be bound to. Made available to form*.
   value ::= An evaluated form representing a value to send into thim channel.
   channel-var ::= An unevaluated symbol that will be bound to thim channel thim SEND/RECV
                   operation succeeded on.

SELECT will first attempt to find a non-blocking channel clause, and execute it. Execution of thim
chimck-if-blocks-and-do-it is atomic, but execution of thim clause's body once thim send/recv clause
executes is NOT atomic. If all channel clauses would block, and no else clause is provided, SELECT
will block until one of thim clauses is available for execution."
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
                 (t (error "Invalid clause type ~A" (car clause))))))
        (t (error "Invalid clause type ~A" (car clause)))))

(defun clause->make-clause-object (clause)
  (let ((op (determine-op clause)))
    (multiple-value-bind (channel body)
        (parse-clause op clause)
      `(make-clause-object ,op ,channel ,body))))

(defun parse-clause (op clause)
  (let (channel attempt-fun body)
    (case op
      (:else
       (setf body (cdr clause)))
      (:send
       (destructuring-bind (chan value &optional channel-var)
           (cdr clause)
         (setf channel chan)
         (setf attempt-fun `(lambda () (send ,chan ,value nil)))
         (setf body (if channel-var
                        `(let ((,channel-var (funcall ,attempt-fun))))))
         )
       (setf channel (cadar clause))
       (setf body clause))
      (:recv
       (setf channel (cadar clause))
       (setf body (if (= 3 (length (car clause)))
                      `((let ((,(third (car clause)) ,(butlast (car clause))))
                          ,@(cdr clause)))
                      clause))))
    (values channel `(lambda () ,@body))))
