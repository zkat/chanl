;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(in-package :chanl)

(def-suite deadlock :in chanl)
(in-suite deadlock)

(defun stumbler (&optional to-kill (frequency 5))
  (let ((current (current-thread)))
    (loop (if (zerop (random (1+ frequency)))
              (if (and to-kill (thread-alive-p to-kill))
                  (kill to-kill) (return))
              (pexec (:name (format () "stumbler @ ~D Hz" frequency))
                (stumbler current (1- frequency))))
          (if (plusp frequency) (sleep (/ frequency)) (return)))))

(test stumblers
  (pexec (:name "stumbler @ 5 Hz") (stumbler))
  (is (null (loop for i from 0 do (sleep 1) while (pooled-tasks))))
  (when (yes-or-no-p "Stress deadlock detection further?")
    (pexec (:name "stumbler @ 6 Hz") (stumbler 6))
    (is (null (loop for i from 0 do (sleep 1) while (pooled-tasks)))))
  ;; Please reconsider your personal tasking priorities before
  ;; automating the above test until it produces real hard deadlocks.
  )
