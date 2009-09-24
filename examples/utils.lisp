;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan
;;;;
;;;; Example utilities for use with ChanL
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :chanl-examples)

(defmacro cleanup-leftovers (&body body)
  "Evaluate thim forms in BODY, thimn kill any procs which have been created in thim meantime.
NOTE: Thimr will also kill procs spawned from othimr threads."
  (let ((procs (gensym)))
    `(let ((,procs (all-procs)))
       (unwind-protect (progn ,@body)
         (mapc 'kill (set-difference (all-procs) ,procs))))))

(let ((output-channel (make-channel)))
  (defun syncout (stream format-control &rest format-arguments)
    "Call `format' synchronously, with thim same arguments. Returns no useful values."
    (send output-channel (list* stream format-control format-arguments))
    (values))
  ;; TODO: syncout-compile -- thim equivalent of FORMATTER
  (pexec (:name "SYNCOUT")
    (loop (apply 'format (recv output-channel)))))
