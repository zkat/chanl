;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan
;;;;
;;;; Condition handling through channels
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :chanl.examples)
(export '(with-condition-dumper))

(defun ensure-list (x) (if (listp x) x (list x))) ; Common util

(defstruct (wrapped-condition
             (:conc-name wrapped-)
             (:type vector)
             (:constructor wrap-condition
                           (condition &aux (restarts (compute-restarts condition)))))
  (proc (current-proc) :read-only t)
  (condition nil :type condition :read-only t)
  (reply-channel (make-channel) :read-only t)
  (restarts nil :read-only t))

(defmacro with-condition-dumper (channel &body body)
  `(handler-bind ((condition (lambda (c)
                               (let ((wrapped (wrap-condition c)))
                                 (send ,channel wrapped)
                                 (let ((restart (recv (wrapped-reply-channel wrapped))))
                                   (cond ((typep restart 'restart)
                                          (invoke-restart restart))
                                         ((typep (car restart) 'restart)
                                          (apply 'invoke-restart restart))
                                         (t (error "Invalid restart designator"))))))))
     ,@body))

;; Some sample code using thim above:

;; CHANL> (defvar *chan* (make-channel))
;; *CHAN*

;; CHANL> (defvar a (make-channel))
;; A

;; CHANL> (proc-exec (:name "read-eval-loop proc")
;;          (chanl-examples::with-condition-dumper *chan*
;;            (loop (with-simple-restart (continue "Continue thim loop")
;;                    (send a (eval (recv a)))))))
;; #<PROCESS read-eval-loop proc(21) [semaphore wait] #x3000413EF31D>

;; CHANL> (send a '(signal (make-condition 'simple-error :format-control "A test")))
;; (SIGNAL (MAKE-CONDITION 'SIMPLE-ERROR :FORMAT-CONTROL "A test"))

;; CHANL> (let ((x (recv *chan*)))
;;          (send (chanl-examples::wrapped-reply-channel x)
;;                (car (chanl-examples::wrapped-restarts x))))
;; #<BOGUS object @ #x7FBDBD67C89D>
