;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :chanl)

(in-suite chanl)

(test queue
  (let ((queue (make-queue)))
    (is (queuep queue))
    (is (queue-empty-p queue))
    (is (eq 5 (enqueue 5 queue)))
    (is (not (queue-empty-p queue)))
    (is (= 1 (queue-count queue)))
    (is (eq 10 (enqueue 10 queue)))
    (is (= 2 (queue-count queue)))
    (is (= 5 (queue-peek queue)))
    (is (= 5 (dequeue queue)))
    (is (= 10 (dequeue queue)))))

(test procs
  (let ((proc (pexec (:name "proc") (sleep 60))))
    (is (procp proc))
    (is (proc-alive-p proc))
    (is (string= "proc" (proc-name proc)))
    (is (member proc (all-procs)))
    (kill proc)
    (is (not (proc-alive-p proc)))
    (signals error (kill (current-proc)))))

(def-suite channels :in chanl)
(def-suite channels-unbuffered :in channels)
(in-suite channels-unbuffered)

(test unbuffered-ignored
  (let ((channel (make-channel)))
    (is (channelp channel))
    (is (not (channel-full-p channel)))
    (is (not (channel-empty-p channel)))
    (is (send-blocks-p channel))
    (is (recv-blocks-p channel))))
