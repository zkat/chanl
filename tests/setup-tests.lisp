;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Josh Marchan, Adlai Chandrasekhar
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

;;; Hooking into ASDF
(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :chanl.tests))))
  (format t "~2&*******************~@
                ** Starting test **~@
                *******************~%")
  (run-all-tests)
  (format t "~2&*****************************************~@
                **            Tests finished           **~@
                *****************************************~@
                ** If there were any failures on your  **~@
                ** platform, please report them to me: **~@
                **  (sykopomp at sykosomatic dot org)  **~@
                ** or just file a bugreport on github: **~@
                **   github.com/sykopomp/chanl/issues  **~@
                *****************************************~%"))
