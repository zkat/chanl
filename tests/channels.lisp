;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Josh Marchan
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :chanl)

(def-suite channels :in chanl)
(def-suite channels-unbuffered :in channels)
(in-suite channels-unbuffered)

(test unbuffered-ignored
  (let ((channel (make-channel)))
    (is (channelp channel))
    (signals error (channel-full-p channel))
    (signals error (channel-empty-p channel))
    (is (send-blocks-p channel))
    (is (recv-blocks-p channel))))

(test unbuffered-recv-context
  (let* ((channel (make-channel))
         (procs (loop repeat 20 collect (pexec () (recv channel)))))
    (unwind-protect
         (progn
           (signals error (channel-full-p channel))
           (signals error (channel-empty-p channel))
           (is (not (send-blocks-p channel)))
           (is (recv-blocks-p channel)))
      (mapcar 'kill procs))))

(test unbuffered-send-context
  (let* ((channel (make-channel))
         (procs (loop repeat 20 collect (pexec () (send channel nil)))))
    (unwind-protect
         (progn
           (signals error (channel-full-p channel))
           (signals error (channel-empty-p channel))
           (is (send-blocks-p channel))
           (is (not (recv-blocks-p channel))))
      (mapcar 'kill procs))))
