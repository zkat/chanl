;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan, Adlai Chandrasekhar
;;;;
;;;; Preparation for the ChanL test suite
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :chanl)

;;; Setting up the :CHANL package to include Eos stuff
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(Eos:def-suite Eos:run! Eos:is Eos:in-suite Eos:signals Eos:def-fixture))
  (export 'run-all-tests))

(defmacro test (name &body body)
  `(eos:test ,name ,@body))

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
                **  (zkat at sykosomatic dot org)  **~@
                ** or just file a bugreport on github: **~@
                **   github.com/zkat/chanl/issues  **~@
                *****************************************~%"))
