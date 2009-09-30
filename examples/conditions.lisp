;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan, Adlai Chandrasekhar
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
  (thread (current-thread) :read-only t)
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

;; Some sample code using the above:

;; CHANL> (defvar *chan* (make-channel))
;; *CHAN*

;; CHANL> (defvar a (make-channel))
;; A

;; CHANL> (pexec ()
;;          (chanl.examples::with-condition-dumper *chan*
;;            (loop (with-simple-restart (continue "Continue the loop")
;;                    (send a (funcall (recv a)))))))
;; T

;; CHANL> (send a (lambda () (signal (make-condition 'simple-error :format-control "A test"))))
;; #<CHANNEL [unbuffered] @ #xBABEBABEBABE>

;; CHANL> (let ((x (recv *chan*))
;;              (some-restart (car (chanl.examples::wrapped-restarts x))))
;;          (send (chanl.examples::wrapped-reply-channel x) some-restart)
;;          some-restart)
;; #<BOGUS object @ #x7FBDBD67C89D>
