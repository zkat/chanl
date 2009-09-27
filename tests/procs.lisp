;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Josh Marchan
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :chanl)

(def-suite procs :in chanl)
(in-suite procs)

(test procs
  (let ((proc (pexec (:name "proc") (sleep 2))))
    (is (procp proc))
    (is (proc-alive-p proc))
    (is (string= "proc" (proc-name proc)))
    (is (member proc (all-procs)))
    (signals error (kill (current-proc)))))

