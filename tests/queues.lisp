;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan, Adlai Chandrasekhar
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :chanl)

(def-suite queues :in chanl)
(in-suite queues)

(test queue
  (let ((queue (make-queue 2)))
    (is (queue-empty-p queue))
    (is (eq 5 (enqueue 5 queue)))
    (is (not (queue-empty-p queue)))
    (is (= 1 (queue-count queue)))
    (is (eq 10 (enqueue 10 queue)))
    (is (= 2 (queue-count queue)))
    (is (= 5 (queue-peek queue)))
    (is (= 5 (dequeue queue)))
    (is (= 10 (dequeue queue)))))

