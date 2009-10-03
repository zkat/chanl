;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Josh Marchan, Adlai Chandrasekhar
;;;;
;;;; Selection Interface -- Never block!
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :chanl)

;;;
;;; Select macro
;;;

(defvar *select-block*)
(setf (documentation '*select-block* 'variable)
      "The block name of the `select' form currently being assembled.
This is only bound within the scope of building a `select' form.")

(defmacro select (&body clauses)
  "Non-deterministically select a non-blocking clause to execute.

The syntax is:

   select clause*
   clause ::= (op form*)
   op ::= (recv c &optional variable channel-var) | (send c value &optional channel-var)
          | else | otherwise | t
   c ::= An evaluated form representing a channel, or a sequence of channels.
   variable ::= an unevaluated symbol RECV's return value is to be bound to. Made available to form*.
   value ::= An evaluated form representing a value to send into the channel.
   channel-var ::= An unevaluated symbol that will be bound to the channel the SEND/RECV
                   operation succeeded on.

SELECT will first attempt to find a clause with a non-blocking op, and execute it. Execution of the
check-if-blocks-and-do-it part is atomic, but execution of the clause's body once the SEND/RECV
clause executes is NOT atomic. If all channel clauses would block, and no else clause is provided,
SELECT will block until one of the clauses is available for execution.

SELECT's non-determinism is, in fact, very non-deterministic. Clauses are chosen at random, not
in the order they are written. It's worth noting that SEND/RECV, when used on sequences of
channels, are still linear in the way they go through the sequence -- the random selection is
reserved for individual SELECT clauses."
  (with-gensyms (*select-block* clause-vector)
    `(block ,*select-block*
       (let ((,clause-vector (vector ,@(mapcar 'wrap-select-clause
                                               (remove :else clauses :key 'clause-type)))))
         ,(aif (find :else clauses :key 'clause-type)
               `(loop repeat (length ,clause-vector)
                   for index = (random (length ,clause-vector)) then
                   (if (= (length ,clause-vector) (incf index)) 0 index)
                   do (funcall (svref ,clause-vector index))
                   finally ,(wrap-select-clause it))
               `(loop for starting-index = (random (length ,clause-vector)) do
                   (loop repeat (length ,clause-vector)
                      for index = starting-index then
                      (if (= (length ,clause-vector) (incf index)) 0 index)
                      do (funcall (svref ,clause-vector index)))))))))

(defun clause-type (clause)
  (cond ((when (symbolp (car clause))
           (or (string-equal (car clause) "t")
               (string-equal (car clause) "else")
               (string-equal (car clause) "otherwise")))
         :else)
        ((atom (car clause)) (error "Invalid selector: ~S" (car clause)))
        ((string-equal (caar clause) "send") :send)
        ((string-equal (caar clause) "recv") :recv)
        (t (error "Invalid selector: ~S" (caar clause)))))

(defun wrap-select-clause (clause)
  (case (clause-type clause)
    (:else `(return-from ,*select-block*
              (block nil ,@(cdr clause))))
    (:send (let ((op (car clause)))
             `(lambda ()
                ,(aif (fourth op)
                      `(when-bind ,it (,@(subseq op 0 3) nil)
                         (return-from ,*select-block*
                           (block nil ,@(cdr clause))))
                      `(when (,@(subseq op 0 3) nil)
                         (return-from ,*select-block*
                           (block nil ,@(cdr clause))))))))
    (:recv (let ((op (car clause)))
             `(lambda ()
                ,(aif (fourth op)
                      `(multiple-value-bind (,(third op) ,it)
                           (,@(subseq op 0 2) nil)
                         (when ,it
                           (return-from ,*select-block*
                             (block nil ,@(cdr clause)))))
                      (let ((chan (gensym)))
                        `(multiple-value-bind (,(third op) ,chan)
                             (,@(subseq op 0 2) nil)
                           (when ,chan
                             (return-from ,*select-block*
                               (block nil ,@(cdr clause))))))))))
    (t (error "This error shouldn't happen -- there's a bug in CHANL::CLAUSE-TYPE"))))
