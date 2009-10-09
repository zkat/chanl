;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan, Adlai Chandrasekhar
;;;;
;;;; Example utilities for use with ChanL
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :chanl.examples)

(export '(cleanup-leftovers syncout))

(defmacro cleanup-leftovers (&body body)
  "Evaluate the forms in BODY, then kill any procs which have been created in the meantime.
NOTE: This will also kill procs spawned from other threads."
  (let ((procs (gensym)))
    `(let ((,procs (all-procs)))
       (unwind-protect (progn ,@body)
         (mapc 'kill (set-difference (all-procs) ,procs))))))

(let ((output-channel (make-instance 'bounded-channel :size 1024)))
  (defun syncout (stream format-control &rest format-arguments)
    "Call `format' synchronously, with the same arguments. Returns no useful values."
    (send output-channel (list* stream format-control format-arguments))
    (values))
  ;; TODO: syncout-compile -- the equivalent of FORMATTER
  (pexec () ; (:name "SYNCOUT")
    (loop (apply 'format (recv output-channel)))))
