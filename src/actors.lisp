;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2015 Adlai Chandrasekhar
;;;;
;;;; Channel-Chattering Actors - A Prototype
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage #:chanl.actors
  (:use #:cl #:chanl)
  (:export #:actor #:perform #:halt #:name #:slot-channel #:compute-tubes
           #:execute #:command #:abbrev #:tasks #:ensure-running #:parent
           #:children #:adopt #:christen #:delegates))

(in-package #:chanl.actors)

;;; TODO: factor all this apart (delegates -> sheeple? add method combinations?)
(defclass actor ()
  ((name :initarg :name :reader name    ; if you must, use (setf slot-value)
         :documentation "Name for identifying this actor and its tasks")
   (abbrev :initform () :allocation :class)
   (tubes :documentation "Channels used for communication")
   (tasks :initform nil :documentation "Tasks performing this actor's behavior")
   (delegates :initform nil :initarg :delegates
              :documentation "Other actors for delegating missing slot values")
   (command :documentation "Command being executed by the actor")))

(defun slot-channel (actor slot) (cdr (assoc slot (slot-value actor 'tubes))))

(defgeneric compute-tubes (actor)
  (:documentation "Calculates the list of communication slots for `actor'")
  (:method-combination append :most-specific-last)
  (:method append ((actor actor)) `((command . ,(make-instance 'channel)))))

;;; from scalpl.util ; TODO: #.(if (find-package :scalpl.util) ...)
(defun strftime (&optional datep &aux bits)
  (let ((data (multiple-value-list      ; my kingdom for a stack!
               (decode-universal-time (get-universal-time)))))
    (symbol-macrolet ((next (princ-to-string (pop data))))
      (macrolet ((collect (&rest xs)
                   `(progn ,@(mapcar (lambda (x) `(push ,x bits)) xs))))
        (collect next ":" next ":" next)
        (when datep (collect " " next "-" next)))))
  (apply 'concatenate 'string bits))

(defgeneric christen (actor type)
  (:method ((actor actor) (type (eql 'actor))) (strftime t))
  (:method ((actor actor) (type (eql 'task)))
    (with-slots (name abbrev) actor (format nil "~A~@[ ~A~]" name abbrev)))
  (:method :around ((actor actor) (type (eql 'task)))
    (concatenate 'string (strftime) " " (call-next-method))))

(defmethod slot-unbound ((class t) (actor actor) (slot-name (eql 'name)))
  (setf (slot-value actor 'name) (christen actor 'actor)))

(defmethod print-object ((actor actor) stream)
  (print-unreadable-object (actor stream :type t :identity t)
    (write-string (name actor) stream)))

(defmacro define-delegated-slot-operation (operation return)
  `(defmethod slot-missing ((class t) (object actor) slot-name
                            (operation (eql ',operation)) &optional new-value)
     (declare (ignore new-value))       ; a sufficiently smart compiler...
     (dolist (actor (slot-value object 'delegates) (call-next-method))
       (when (slot-boundp actor slot-name) (return ,return)))))
(define-delegated-slot-operation slot-value (slot-value actor slot-name))

(defgeneric execute (actor command)
  (:method ((actor actor) (command function)) (funcall command actor))
  (:method ((actor actor) (command (eql :halt))) (throw :halt actor)))

(defgeneric enqueue (actor)
  (:method ((actor actor))
    (pexec (:name (christen actor 'task)
            :initial-bindings '((*read-default-float-format* double-float)))
      (catch :halt (perform actor)))))

(define-method-combination select (&optional (sleep 1))
  ((recv (recv . *)) (send (send . *)) (default ())
   (before (:before)) (after (:after)) (around (:around)))
  (:arguments actor)
  (flet ((build-recv (method &aux (slot (second (method-qualifiers method))))
           `((recv (slot-channel ,actor ',slot) value)
             (setf (slot-value ,actor ',slot) value) (call-method ,method)))
         (build-send (method &aux (slot (second (method-qualifiers method))))
           `((send (slot-channel ,actor ',slot) (slot-value ,actor ',slot))
             (call-method ,method)))
         (call-methods (methods)
           (mapcar (lambda (method) `(call-method ,method)) methods)))
    (let ((form `(multiple-value-prog1
                     (progn ,@(call-methods before)
                            (select ,@(mapcar #'build-recv recv)
                                    ,@(mapcar #'build-send send)
                                    (t ,(if default `(call-method ,@default)
                                            `(sleep ,sleep)))))
                   ,@(call-methods after))))
      (if (null around) form
          `(call-method ,(first around)
                        (,@(rest around) (make-method ,form)))))))

(defgeneric perform (actor)
  (:documentation "Implement actor's behavior, executing commands by default")
  (:method-combination select)
  (:method :before ((actor actor))
    (with-slots (tasks) actor
      (setf tasks (remove :terminated tasks :key #'task-status))))
  (:method recv command ((actor actor))
    (execute actor (slot-value actor 'command)))
  (:method :after ((actor actor))
    (push (enqueue actor) (slot-value actor 'tasks))))

(defgeneric ensure-running (actor)
  (:method ((actor actor) &aux (cache (slot-value actor 'tasks)))
    (with-slots (tasks) actor           ; this cache business, blech… scheduler?
      (let ((it (and (find :alive cache :key #'task-status) (eq tasks cache))))
        (if it it (push (enqueue actor) tasks))))))

(defgeneric halt (actor)
  (:documentation "Signals `actor' to terminate")
  (:method ((actor actor)) (send (slot-channel actor 'command) :halt)))

(defmethod initialize-instance :before ((actor actor) &key)
  (setf (slot-value actor 'tubes) (compute-tubes actor)))

(defmethod shared-initialize :after ((actor actor) (slot-names t) &key)
  (ensure-running actor))

;;;
;;; Parent
;;;

(defclass parent (actor) ((children :initform nil)))

(define-delegated-slot-operation slot-boundp t)

(defun map-children (parent function)   ; ... i'm not sure what i expected
  (mapcar function (slot-value parent 'children)))

(defmethod ensure-running :after ((parent parent))
  (map-children parent #'ensure-running))

(defmethod halt :before ((parent parent))
  (map-children parent #'halt))

(defgeneric adopt (parent child)
  (:method ((parent parent) (child actor))
    (pushnew child  (slot-value parent 'children))))

(defgeneric disown (parent child)
  (:method ((parent parent) (child actor))
    (with-slots (children) parent (setf children (remove child children)))))
