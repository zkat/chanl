;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Josh Marchan
;;;;
;;;; Process Abstraction
;;;;
;;;; The thread pool here is taken directly from Eager Future. See COPYRIGHT for relevant info.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :chanl)

(defclass thread-pool ()
  ((threads :accessor pool-threads :initform ())
   (free-thread-counter :accessor free-thread-counter :initform 0)
   (soft-limit :accessor pool-soft-limit :initform nil)
   (lock :reader pool-lock :initform (make-lock "thread pool lock"))
   (leader-lock :reader pool-leader-lock :initform (make-lock "thread leader lock"))
   (leader-notifier :reader pool-leader-notifier :initform (make-condition-variable))
   (tasks :accessor pool-tasks :initform nil)))

(defvar *thread-pool* (make-instance 'thread-pool))

(define-symbol-macro %thread-pool-soft-limit (soft-limit *thread-pool*))

(defun new-worker-thread (thread-pool task)
  (push (bt:make-thread
         (lambda ()
           (unwind-protect
                (loop (when task (ignore-errors (funcall task)))
                   (with-lock-held ((lock thread-pool))
                     (if (and (soft-limit thread-pool)
                              (> (length (threads thread-pool))
                                 (soft-limit thread-pool)))
                         (return)
                         (incf (free-thread-counter thread-pool))))
                   (with-lock-held ((leader-lock thread-pool))
                     (with-lock-held ((lock thread-pool))
                       (setf task
                             (or #1=(pop (tasks thread-pool))
                                 (progn
                                   (condition-wait (leader-notifier thread-pool)
                                                   (lock thread-pool))
                                   #1#)))
                       (decf (free-thread-counter thread-pool)))))
             (with-lock-held ((lock thread-pool))
               (setf (threads thread-pool)
                     (remove (current-thread) (threads thread-pool))))))
         :name "Eager Futures Thread Pool Worker")
        (threads thread-pool)))

(defmethod assign-task (task (thread-pool thread-pool))
  (with-lock-held ((lock thread-pool))
    (if (= (free-thread-counter thread-pool) (length (tasks thread-pool)))
        (new-worker-thread thread-pool task)
        (push task (tasks thread-pool))))
  (condition-notify (leader-notifier thread-pool)))

(defun current-proc ()
  (bt:current-thread))

(defun proc-alive-p (proc)
  (bt:thread-alive-p proc))

(defun procp (proc)
  (bt:threadp proc))

(defun proc-name (proc)
  (bt:thread-name proc))

(defun kill (proc)
  (bt:destroy-thread proc))

(defun pcall (function &key name (initial-bindings *default-special-bindings*))
  "PCALL -> Parallel Call; calls FUNCTION in a new thread. FUNCTION must be a no-argument
function. Providing NAME will set the thread's name. Refer to Bordeaux-threads documentation
for how INITIAL-BINDINGS works."
  (bt:make-thread function :name name :initial-bindings initial-bindings))

(defmacro pexec ((&key name initial-bindings) &body body)
  "Executes BODY in parallel (a new thread). NAME sets new thread's name. Refer to
Bordeaux-Threads documentation for more information on INITIAL-BINDINGS."
  `(pcall (lambda () ,@body)
          ,@(when name `(:name ,name))
          ,@(when initial-bindings `(:initial-bindings ,initial-bindings))))

(defun all-procs ()
  (bt:all-threads))
