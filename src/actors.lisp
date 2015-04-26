;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2015 Adlai Chandrasekhar
;;;;
;;;; Channel-Chattering Actors - A Prototype
;;;; TODO: http://archive.adaic.com/standards/83rat/html/ratl-13-02.html#13.2.4
;;;; The goal is for channels to be as invisible as threads and pointers
;;;; When that happens, this may very well just belong in a separate library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage #:chanl.actors
  (:use #:cl #:chanl) (:import-from #:chanl #:ensure-list)
  (:export #:actor #:perform #:halt #:name #:slot-channel #:compute-tubes
           #:execute #:command #:abbrev #:state
           #:ensure-running #:boss #:die #:fire))

(in-package #:chanl.actors)

(defvar *boss*)

;;; TODO: factor all this apart (delegates -> sheeple? merge into bossing?)
(defclass actor ()
  ((name :initarg :name :reader name    ; if you must, use (setf slot-value)
         :documentation "Name for identifying this actor and its tasks")
   (abbrev :initform () :allocation :class)
   ;; this is traditional "message passing", each actor gets its own state
   (state :initform 'perform :documentation "Represents/performs actor's state")
   (tubes :documentation "Channels used for communication")
   (boss :documentation "For whom['s benefit] the bell tolls" :reader boss
         :initform *boss* :initarg :boss :type (or bt:thread boss))
   (command :documentation "Command being executed by the actor")))

(defun slot-channel (actor slot)
  "Returns the channel associated with `slot' in `actor'"
  (let ((spec (cdr (assoc slot (slot-value actor 'tubes)))))
    (etypecase spec (channel spec) (symbol (slot-value actor spec)))))

(defgeneric compute-tubes (actor)
  (:documentation "Calculates the list of communication slots for `actor'.
Methods should return a list of specifications (or a single one as an atom)")
  (:method-combination list :most-specific-last)  ; TODO: lazy-append
  (:method :around ((actor actor))      ; &rest?
    "Combines the specifications, creating channels if necessary"
    (mapcan (lambda (tubing)
              (mapcar (lambda (tube)
                        (destructuring-bind (name . spec) (ensure-list tube)
                          (cons name    ; ( data-slot :from channel-slot )
                                (if (member (car spec)'(:to :from)) (cadr spec)
                                    (apply #'make-instance
                                           (or spec '(channel)))))))
                      (ensure-list tubing))) ; in case a method returns an atom
            (call-next-method)))
  (:method list ((actor actor)) '(command death)))

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

(defgeneric christen (actor)
  (:method ((actor actor))
    (with-slots (name abbrev) actor (format nil "~A~@[ ~A~]" name abbrev)))
  (:method :around ((actor actor))
    (concatenate 'string (strftime) " " (call-next-method))))

(defmethod slot-unbound ((class t) (actor actor) (slot-name (eql 'name)))
  (setf (slot-value actor 'name) (strftime t)))

(defmethod print-object ((actor actor) stream)
  (print-unreadable-object (actor stream :type t :identity t)
    (write-string (name actor) stream)))

(macrolet ((delegate-slot-operation (op return) ;P
             `(defmethod slot-missing ((class t) (actor actor) slot
                                       (op (eql ',op)) &optional new-value)
                (declare (ignore new-value)) ; a sufficiently smart compiler...
                (with-slots (boss) actor
                  (if (and (typep boss 'actor) (slot-boundp boss slot))
                      ,return (call-next-method))))))
  (delegate-slot-operation slot-value (slot-value boss slot))
  (delegate-slot-operation slot-boundp t))

(define-method-combination select (&optional (sleep 1/7))
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
  (:method ((actor actor) (command (eql :die))) (throw :die (current-thread))))

(defun launch (actor)
  (bt:make-thread (lambda ()
                    (catch :die
                      (loop (funcall (slot-value actor 'state) actor))))
                  :name (christen actor)))

(defgeneric ensure-running (actor)
  (:method ((actor actor))
    (with-slots (boss) actor
      (symbol-macrolet ((launch (setf boss (launch actor))))
        (typecase boss
          (null launch)
          (bt:thread (cond ((eq boss (bt:current-thread)) ; nop
                            (warn "~A tried to revive itself" actor))
                           ((bt:thread-alive-p boss) ; nop
                            (warn "~A revived before death" actor))
                           (t (warn "races ahoy!") launch)))
          (boss (send (slot-channel boss 'to-run) actor)))))))

(defgeneric act (class &key)
  (:method ((class symbol) &rest initargs) ; :metaclass actor-class
    (ensure-running (apply #'make-instance class initargs))))

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

(defmethod compute-tubes list ((boss boss))
  '((to-run unbounded-channel) to-halt to-fire))

(defvar *boss*
  (make-instance 'boss :name "atp" :boss
                 (prog1 (bt:make-thread #'list) (sleep 3)))) ; bootstrap!

(defun map-workers (boss function)   ; ... i'm not sure what i expected
  (mapcar function (mapcar #'car (slot-value boss 'workers))))

(defmethod ensure-running :after ((boss boss))
  (map-workers boss #'ensure-running))

(defun %kill (actor) (send (slot-channel actor 'command) :die))

(defmethod execute :before ((boss boss) (command (eql :die)))
  (map-workers boss #'%kill))

(defun halt (actor)
  (typecase (boss actor)
    (bt:thread (%kill actor))
    (boss (send (slot-channel (boss actor) 'to-halt) actor))))

(defun fire (actor) (send (slot-channel (boss actor) 'to-fire) actor))

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
          (and link (bt:thread-alive-p (cdr link)) (%kill to-halt))))))

(defmethod perform recv to-fire ((boss boss))
  (with-slots (to-fire workers) boss    ; FIXME: pater, pater everywhere, but...
    (if (eq boss to-fire) (warn "~A told to fire itself" boss)
        (let ((link (assoc to-fire workers))) ; this isn't even funny anymore
          (declare (type (or null (cons actor bt:thread)) link))
          (and link (or (not (bt:thread-alive-p (cdr link))) (%kill to-fire))
               (setf workers (remove link workers)))))))
