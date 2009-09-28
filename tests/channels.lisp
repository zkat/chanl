;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :chanl)

(def-suite channels :in chanl)
(def-suite make-channel :in chanl)

(test buffered
  (let ((chan (make-channel 10)))
    (is (channelp chan))
    (is (channel-buffered-p chan))
    (is (queuep (channel-buffer chan)))
    (is (= 10 (queue-max-size (channel-buffer chan))))
    (is (= 0 (channel-readers chan)))
    (is (= 0 (channel-writers chan)))
    (is (eq *secret-unbound-value* (channel-value chan)))
    (is (not (send-blocks-p chan)))
    (is (recv-blocks-p chan))
    ;; We don't really have predicates for these, but if they exist, we assume
    ;; they're what they're suposed to be.
    (is (channel-lock chan))
    (is (channel-send-ok chan))
    (is (channel-recv-ok chan))))

(test unbuffered
  (let ((chan (make-channel)))
    (is (channelp chan))
    (is (not (channel-buffered-p chan)))
    (is (null (channel-buffer chan)))
    (is (= 0 (channel-readers chan)))
    (is (= 0 (channel-writers chan)))
    (is (eq *secret-unbound-value* (channel-value chan)))
    (is (send-blocks-p chan))
    (is (recv-blocks-p chan))
    ;; We don't really have predicates for these, but if they exist, we assume
    ;; they're what they're suposed to be.
    (is (channel-lock chan))
    (is (channel-send-ok chan))
    (is (channel-recv-ok chan))))

(test invalid
  (signals error (make-channel nil))
  (signals error (make-channel -1)))

(def-suite messaging :in chanl)
(def-suite sending :in messaging)

(test send-blocks-p
  (let ((channel (make-channel))
        test-proc)
    (is (send-blocks-p channel))
    (setf test-proc (pexec () (recv channel)))
    (sleep 0.5)
    (is (not (send-blocks-p channel)))
    (kill test-proc)))

(test channel-insert-value)
(test send)

(def-suite receiving :in messaging)

(test recv)
(test recv-blocks-p)
(test %recv-blocks-p)
(test channel-grab-value)
