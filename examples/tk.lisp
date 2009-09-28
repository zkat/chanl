;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan, Adlai Chandrasekhar
;;;;
;;;; A trivial example using channels with the lisp Tk implementation.
;;;; Example adapted from csp's tk example by roger peppe
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :chanl.examples)

(use-package '(:ltk))
(export '(ltk-button-demo))

(defparameter *tkc* (make-channel))

(defun string+ (&rest s)
  (apply #'concatenate 'string s))

(defmacro tkcmd (&rest forms)
  `(let ((reply (make-channel)))
     (send *tkc* (list (lambda () ,@forms) reply))
     (recv reply)))

(defun button (channel msg)
  (tkcmd (pack (make-instance 'button
                              :text msg
                              :master nil
                              ;; For some reason, this doesn't seem to be getting called when
                              ;; a button is pressed. It seems I'll have to get this to execute
                              ;; in the main loop inside with-ltk.
                              :command (lambda () (send channel msg))))))

(defun ltk-button-demo ()
  (let* ((c (make-channel))
         (button-message-thread
          (pexec ()
            (loop
               for i from 0
               do (let ((title (recv c)))
                    (button c (format nil "~a.~d" title i))))))
         (initial-button-thread
          (pexec () (button c "hello"))))
    (unwind-protect
         (with-ltk ()
           (loop (let ((reply (recv *tkc*)))
                   (send (cadr reply) (funcall (car reply))))))
      (kill button-message-thread)
      (kill initial-button-thread))))
