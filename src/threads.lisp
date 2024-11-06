;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan, Adlai Chandrasekhar
;;;;
;;;; Thread Abstraction
;;;;
;;;; The thread pool here is taken directly from Eager Future. See COPYRIGHT for relevant info.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :chanl)

;;;
;;; Thread pools
;;;
(defclass thread-pool ()
  ((threads :accessor pool-threads :initform nil)
   (tasks :accessor pool-tasks :initform nil)))

(defclass soft-thread-pool (thread-pool)
  ((threads :accessor pool-threads :initform nil)
   (free-thread-counter :accessor free-thread-counter :initform 0)
   (soft-limit :accessor pool-soft-limit :initform 1000) ; this seems like a sane-ish default
   (lock :reader pool-lock :initform (bt:make-lock "thread pool lock"))
   (leader-lock :reader pool-leader-lock :initform (bt:make-lock "thread leader lock"))
   (leader-notifier :reader pool-leader-notifier :initform (bt:make-condition-variable))
   (timeout :accessor pool-timeout :initform nil)
   (pending-tasks :accessor pool-pending-tasks :initform nil)
   (tasks :accessor pool-tasks :initform nil)))

(defclass task ()
  ((name :accessor task-name :initform "Anonymous Task" :initarg :name)
   (function :reader task-function :initarg :function
             :initform (error "Must supply a task-function"))
   (status :accessor task-status :initform :pending)
   (thread :accessor task-thread :initform nil)))

(define-print-object ((task task))
  (format t "~A [~A]" (task-name task) (task-status task)))

(defvar *thread-pool* (make-instance 'soft-thread-pool))

(define-symbol-macro %thread-pool-soft-limit (pool-soft-limit *thread-pool*))

(defun pooled-threads ()
  (pool-threads *thread-pool*))

(defun pooled-tasks ()
  (pool-tasks *thread-pool*))

(defun pool-health (&optional (thread-pool *thread-pool*))
  (with-slots (tasks threads) thread-pool
    (list (length tasks) (length threads)
          (length (bt:all-threads)))))

(defmethod worker-function ((thread-pool soft-thread-pool) &optional task)
  (with-slots (lock tasks threads soft-limit) thread-pool
    (lambda ()
      (unwind-protect
           (loop
             (when task
               (with-slots (thread status function) task
                 (unwind-protect
                      (progn (setf thread (current-thread) status :alive)
                             (funcall (task-function task)))
                   (setf thread nil status :terminated)
                   (bt:with-lock-held (lock)
                     (setf tasks (remove task tasks))))))
             (bt:with-lock-held (lock)
               (if (and soft-limit (> (length threads) soft-limit)) (return)
                   (incf (free-thread-counter thread-pool))))
             (with-slots (pending-tasks leader-lock leader-notifier) thread-pool
               (with-simple-restart
                   (continue "Release leader lock and requeue")
                 (bt:with-lock-held (leader-lock)
                   (bt:with-lock-held (lock)
                     (do () (pending-tasks (setf task (pop pending-tasks)))
                       (bt:condition-wait leader-notifier lock
                                          :timeout (pool-timeout thread-pool)))
                     (decf (free-thread-counter thread-pool)))))))
        (bt:with-lock-held (lock)
          (setf threads (remove (bt:current-thread) threads)))))))

(defun new-worker-thread (thread-pool &optional task)
  (push (bt:make-thread (worker-function thread-pool task)
                        :name "ChanL Soft Worker")
        (pool-threads thread-pool)))

(defgeneric assign-task (thread-pool task)
  (:method ((thread-pool thread-pool)      (task task))
    (cerror "Run the task in the current thread"
            "~A has no specialized pooling implementation"
            (class-of thread-pool))
    (unwind-protect (progn
                      (setf (task-thread task) (current-thread)
                            (task-status task) :alive)
                      (funcall (task-function task)))
      (setf (task-thread task) () (task-status task) :terminated)))
  (:method ((thread-pool soft-thread-pool) (task task))
    (bt:with-lock-held ((pool-lock thread-pool))
      (push task (pool-tasks thread-pool))
      (if (= (free-thread-counter thread-pool)
             (length (pool-pending-tasks thread-pool)))
          (new-worker-thread thread-pool task)
          (setf (pool-pending-tasks thread-pool)
                (nconc (pool-pending-tasks thread-pool) (list task)))))
    (bt:condition-notify (pool-leader-notifier thread-pool))
    task))

;;;
;;; Threads
;;;
;;; - The goal of just wrapping BT functions with the same
;;;    names is to eventually get rid of the BT dependency.
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

(defun all-threads ()
  (bt:all-threads))

(defun pcall (function &key (name "Anonymous task")
                         (initial-bindings *default-special-bindings*))
  "PCALL -> Parallel Call; calls `FUNCTION' in a new thread.
`FUNCTION' must not require any arguments; `INITIAL-BINDINGS'
sets dynamic variable bindings around `BODY' using the same
format as `PROGV': '((*var* value))."
  (assign-task *thread-pool*
               (make-instance 'task :name name :function
                              (fun (multiple-value-bind (vars bindings)
                                       (unzip-alist initial-bindings)
                                     (progv vars bindings
                                       (funcall function)))))))

(defmacro pexec ((&key initial-bindings name) &body body)
  "Executes `BODY' in parallel; syntax sugar around `PCALL'."
  `(pcall (lambda () ,@body)
          ,@(when initial-bindings `(:initial-bindings ,initial-bindings))
          ,@(when name `(:name ,name))))
