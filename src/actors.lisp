;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2015 Adlai Chandrasekhar
;;;;
;;;; Channel-Chattering Actors - A Prototype
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage #:chanl.actors
  (:use #:cl #:chanl) (:import-from #:chanl #:ensure-list)
  (:export #:actor #:perform #:halt #:name #:slot-channel #:compute-tubes
           #:execute #:command #:abbrev #:state #:ensure-running #:boss))

(in-package #:chanl.actors)

;;; TODO: factor all this apart (delegates -> sheeple? merge into bossing?)
(defclass actor ()
  ((name :initarg :name :reader name    ; if you must, use (setf slot-value)
         :documentation "Name for identifying this actor and its tasks")
   (abbrev :initform () :allocation :class)
   ;; this is traditional "message passing", each actor gets its own state
   (state :initform 'perform :documentation "Represents/performs actor's state")
   (tubes :documentation "Channels used for communication")
   (boss :documentation "For whom['s benefit] the bell tolls"
         :reader boss :initarg :boss)
   (command :documentation "Command being executed by the actor")))

(defun slot-channel (actor slot) (cdr (assoc slot (slot-value actor 'tubes))))

(defgeneric compute-tubes (actor)
  (:documentation "Calculates the list of communication slots for `actor'")
  (:method-combination list :most-specific-last)  ; TODO: lazy-append
  (:method :around ((actor actor))
    (mapcan (lambda (tubing)
              (mapcar (lambda (tube)
                        (destructuring-bind (name . spec) (ensure-list tube)
                          (cons name (apply #'make-instance
                                            (or spec '(channel))))))
                      (ensure-list tubing)))
            (call-next-method)))
  (:method list ((actor actor)) 'command))

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
  `(defmethod slot-missing ((class t) (actor actor) slot-name
                            (operation (eql ',operation)) &optional new-value)
     (declare (ignore new-value))       ; a sufficiently smart compiler...
     (if (and (slot-boundp actor 'boss) (slot-boundp (boss actor) slot-name))
         ,return (call-next-method))))
(define-delegated-slot-operation slot-value (slot-value (boss actor) slot-name))

(define-method-combination select (&optional (sleep 1))
  ((select *)) (:arguments actor)
  (let (before after around recv send default)
    (dolist (method select)
      (ecase (first (method-qualifiers method))
        (:before (push method before))  ; FIX
        (:around (push method around))  ; ME!
        (:after  (push method after))   ; DEF
        ( send   (push method send))    ; MAC
        (  recv  (push method recv))    ; R/O
        (   (()) (push method default))))
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
                          (,@(rest around) (make-method ,form))))))))

(defgeneric perform (actor)
  (:documentation "Implement actor's behavior, executing commands by default")
  (:method-combination select)
  (:method recv command ((actor actor))
    (execute actor (slot-value actor 'command))))

(defgeneric execute (actor command)
  (:method ((actor actor) (command function)) (funcall command actor))
  (:method ((actor actor) (command (eql :halt))) (throw :halt actor)))

(defun launch (actor)
  (bt:make-thread (lambda ()
                    (catch :halt
                      (loop (funcall (slot-value actor 'state) actor))))
                  :name (christen actor 'task)))

(defgeneric ensure-running (actor)
  (:method ((actor actor))
    (with-slots (boss) actor
      (if (not (slot-boundp actor 'boss)) (setf boss (launch actor))
          (typecase boss
            (bt:thread
             (cond ((eq boss (bt:current-thread))
                    (warn "~A tried to revive itself" actor))
                   ((bt:thread-alive-p boss)
                    (warn "~A revived before death" actor))
                   (t (warn "races ahoy!") (setf boss (launch actor)))))
            (boss (send (slot-channel boss 'to-run) actor)))))))

(defgeneric act (class &key)
  (:method ((class symbol) &rest initargs) ; :metaclass actor-class
    (ensure-running (apply #'make-instance class initargs))))

(defgeneric halt (actor)                ; TODO: blocking? timeouts? kill?
  (:documentation "Signals `actor' to terminate")
  (:method ((actor actor)) (send (slot-channel actor 'command) :halt)))

(defmethod initialize-instance :before ((actor actor) &key)
  (setf (slot-value actor 'tubes) (compute-tubes actor)))

(defmethod shared-initialize :after ((actor actor) (slot-names t) &key)
  (ensure-running actor))

;;;
;;; Bureaucracies
;;;

(defclass boss (actor)
  ((workers :initform nil :documentation "Workers managed by this boss")
   (to-run :documentation "New actor to manage")
   (to-halt :documentation "Actor to halt, but keep its link")
   (to-fire :documentation "Actor to both halt and unlink")))

(defmethod compute-tubes list ((boss boss)) '(to-run to-halt to-fire))

(define-delegated-slot-operation slot-boundp t)

(defun map-workers (boss function)   ; ... i'm not sure what i expected
  (mapcar function (mapcar #'car (slot-value boss 'workers))))

(defmethod ensure-running :after ((boss boss))
  (unless (eq (bt:current-thread) (boss boss))
    (send (slot-channel boss 'command)
          (lambda (boss) (map-workers boss #'ensure-running)))))

(defmethod halt :before ((boss boss)) (map-workers boss #'halt))

(defmethod perform recv to-run ((boss boss))
  (with-slots (to-run workers) boss
    (if (eq boss to-run) (warn "~A told to boss itself" boss)
        (let ((link (assoc to-run workers))) ; assumes there's only one link
          (declare (type (or null (cons actor bt:thread)) link))
          (cond ((null link) (push (cons to-run (launch to-run)) workers))
                ((bt:thread-alive-p (cdr link))) ; nothing to see here
                (t (rplacd link (launch to-run)))))))) ; re-launch the worker

(defmethod perform recv to-halt ((boss boss))
  (with-slots (to-halt workers) boss
    (if (eq boss to-halt) (warn "~A told to halt itself" boss)
        (let ((link (assoc to-halt workers))) ; makes an ass out of you and me
          (declare (type (or null (cons actor bt:thread)) link))
          (and link (bt:thread-alive-p (cdr link)) (halt to-halt))))))

(defmethod perform recv to-fire ((boss boss))
  (with-slots (to-fire workers) boss    ; FIXME: pater, pater everywhere, but...
    (if (eq boss to-fire) (warn "~A told to fire itself" boss)
        (let ((link (assoc to-fire workers))) ; this isn't even funny anymore
          (declare (type (or null (cons actor bt:thread)) link))
          (and link (or (not (bt:thread-alive-p (cdr link))) (halt to-fire))
               (setf workers (remove link workers)))))))
