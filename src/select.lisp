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


