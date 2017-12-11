;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan, Adlai Chandrasekhar
;;;;
;;;; Preparation for the ChanL test suite
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :chanl)

;;; Setting up the :CHANL package to include 5AM stuff
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(5am:def-suite 5am:run! 5am:is 5am:in-suite 5am:signals))
  (export 'run-all-tests))

(defmacro test (name &body body)
  `(5am:test ,name ,@body))

;;; Preparing the test suite
(def-suite chanl)

(defun run-all-tests ()
  (run! 'chanl))

(in-suite chanl)

