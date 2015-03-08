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
;;; Thread pool
;;;
(defclass thread-pool ()
  ((threads :accessor pool-threads :initform nil)
   (free-thread-counter :accessor free-thread-counter :initform 0)
   (soft-limit :accessor pool-soft-limit :initform 1000) ; this seems like a sane-ish default
   (lock :reader pool-lock :initform (bt:make-lock "thread pool lock"))
   (leader-lock :reader pool-leader-lock :initform (bt:make-lock "thread leader lock"))
   (leader-notifier :reader pool-leader-notifier :initform (bt:make-condition-variable))
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

(defvar *thread-pool* (make-instance 'thread-pool))

(define-symbol-macro %thread-pool-soft-limit (pool-soft-limit *thread-pool*))

(defun pooled-threads ()
  (pool-threads *thread-pool*))

(defun pooled-tasks ()
  (pool-tasks *thread-pool*))

(defun new-worker-thread (thread-pool &optional task)
  (push (bt:make-thread
         (lambda ()
           (unwind-protect
                (loop
                   (when task
                     (unwind-protect
                          (progn (setf (task-thread task) (current-thread))
                                 (setf (task-status task) :alive)
                                 (funcall (task-function task)))
                       (setf (task-thread task) nil)
                       (setf (task-status task) :terminated)
                       (bt:with-lock-held ((pool-lock thread-pool))
                         (setf (pool-tasks thread-pool)
                               (remove task (pool-tasks thread-pool))))))
                   (bt:with-lock-held ((pool-lock thread-pool))
                     (if (and (pool-soft-limit thread-pool)
                              (> (length (pool-threads thread-pool))
                                 (pool-soft-limit thread-pool)))
                         (return)
                         (incf (free-thread-counter thread-pool))))
                   (bt:with-lock-held ((pool-leader-lock thread-pool))
                     (bt:with-lock-held ((pool-lock thread-pool))
                       (setf task
                             (loop until (pool-pending-tasks thread-pool)
                                do (bt:condition-wait (pool-leader-notifier thread-pool)
                                                      (pool-lock thread-pool))
                                finally (return (pop (pool-pending-tasks thread-pool)))))
                       (decf (free-thread-counter thread-pool)))))
             (bt:with-lock-held ((pool-lock thread-pool))
               (setf (pool-threads thread-pool)
                     (delete (bt:current-thread) (pool-threads thread-pool))))))
         :name "ChanL Thread Pool Worker")
        (pool-threads thread-pool)))

(defgeneric assign-task (thread-pool task)
  (:method ((thread-pool thread-pool) (task task))
    (bt:with-lock-held ((pool-lock thread-pool))
      (push task (pool-tasks thread-pool))
      (if (= (free-thread-counter thread-pool) (length (pool-pending-tasks thread-pool)))
          (new-worker-thread thread-pool task)
          (setf (pool-pending-tasks thread-pool)
                (nconc (pool-pending-tasks thread-pool) (list task)))))
    (bt:condition-notify (pool-leader-notifier thread-pool))
    task))

;;;
;;; Threads
;;;
;;; - The reason we're basically just wrapping BT functions with the same names is that it might
;;;   be good to eventually get rid of the BT dependency.
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

(defun pcall (function &key (initial-bindings *default-special-bindings*) (name "Anonymous task"))
  "PCALL -> Parallel Call; calls FUNCTION in a new thread. FUNCTION must be a no-argument function.
INITIAL-BINDINGS, if provided, should be an alist representing dynamic variable bindings that BODY
is to be executed with. The format is: '((*var* value))."
  (assign-task *thread-pool*
               (make-instance 'task :name name
                              :function (fun (multiple-value-bind (vars bindings)
                                                 (unzip-alist initial-bindings)
                                               (progv vars bindings
                                                 (funcall function)))))))

(defmacro pexec ((&key initial-bindings name) &body body)
  "Executes BODY in parallel. INITIAL-BINDINGS, if provided, should be an alist representing
dynamic variable bindings that BODY is to be executed with. The format is: '((*var* value))."
  `(pcall (lambda () ,@body)
          ,@(when initial-bindings `(:initial-bindings ,initial-bindings))
          ,@(when name `(:name ,name))))
