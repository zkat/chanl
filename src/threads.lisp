;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan
;;;;
;;;; Thread Abstraction
;;;;
;;;; The thread pool here is taken directly from Eager Future. See COPYRIGHT for relevant info.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :chanl)

(defclass thread-pool ()
  ((threads :accessor pool-threads :initform nil)
   (free-thread-counter :accessor free-thread-counter :initform 0)
   (soft-limit :accessor pool-soft-limit :initform 1000) ; this seems like a sane-ish default
   (lock :reader pool-lock :initform (bt:make-lock "thread pool lock"))
   (leader-lock :reader pool-leader-lock :initform (bt:make-lock "thread leader lock"))
   (leader-notifier :reader pool-leader-notifier :initform (bt:make-condition-variable))
   (tasks :accessor pool-tasks :initform nil)))

(defvar *thread-pool* (make-instance 'thread-pool))

(define-symbol-macro %thread-pool-soft-limit (pool-soft-limit *thread-pool*))

(defun new-worker-thread (thread-pool &optional task)
  (push (bt:make-thread
         (lambda ()
           (unwind-protect
                (loop (when task (funcall task))
                   (bt:with-lock-held ((pool-lock thread-pool))
                     (if (and (pool-soft-limit thread-pool)
                              (> (length (pool-threads thread-pool))
                                 (pool-soft-limit thread-pool)))
                         (return)
                         (incf (free-thread-counter thread-pool))))
                   (bt:with-lock-held ((pool-leader-lock thread-pool))
                     (bt:with-lock-held ((pool-lock thread-pool))
                       (setf task
                             (loop until (pool-tasks thread-pool)
                                do (bt:condition-wait (pool-leader-notifier thread-pool)
                                                      (pool-lock thread-pool))
                                finally (return (pop (pool-tasks thread-pool)))))
                       (decf (free-thread-counter thread-pool)))))
             (bt:with-lock-held ((pool-lock thread-pool))
               (setf (pool-threads thread-pool)
                     (delete (bt:current-thread) (pool-threads thread-pool))))))
         :name "ChanL Thread Pool Worker")
        (pool-threads thread-pool)))

(defmethod assign-task (task (thread-pool thread-pool))
  (bt:with-lock-held ((pool-lock thread-pool))
    (if (= (free-thread-counter thread-pool) (length (pool-tasks thread-pool)))
        (new-worker-thread thread-pool task)
        (setf (pool-tasks thread-pool)
              (nconc (pool-tasks thread-pool) (list task)))))
  (bt:condition-notify (pool-leader-notifier thread-pool)))

(defun current-thread ()
  (bt:current-thread))

(defun thread-alive-p (proc)
  (bt:thread-alive-p proc))

(defun threadp (proc)
  (bt:threadp proc))

(defun thread-name (proc)
  (bt:thread-name proc))

(defun kill (proc)
  (bt:destroy-thread proc))

(defun pcall (function)
  "PCALL -> Parallel Call; calls FUNCTION in a new thread. FUNCTION must be a no-argument function."
  (assign-task function *thread-pool*)
  t)

(defmacro pexec (() &body body)
  ;; note: the () is present because there -will- be options.
  "Executes BODY in parallel."
  `(pcall (lambda () ,@body)))

(defun all-procs ()
  (bt:all-threads))
