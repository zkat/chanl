;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Josh Marchan
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :chanl)

(def-suite channels :in chanl)
(def-suite channel-objects :in chanl)

(test make-channel
  (signals error (make-channel nil))
  (signals error (make-channel -1))
  (let ((chan (make-channel)))
    (is (channelp chan))
    (is (not (channel-buffered-p chan)))
    (is (null (channel-buffer chan)))
    (is (= 0 (channer-readers chan)))
    (is (= 0 (channer-writers chan)))
    (is (eq *secret-unbound-value* (channel-value chan)))
    ;; We don't really have predicates for these, but if they exist, we assume
    ;; they're what they're suposed to be.
    (is (channel-lock chan))
    (is (channel-send-ok chan))
    (is (channel-recv-ok chan)))
  (let ((chan (make-channel 10)))
    (is (channelp chan))
    (is (channel-buffered-p chan))
    (is (queuep (channel-buffer chan)))
    (is (= 10 (queue-max-size (channel-buffer chan))))
    (is (= 0 (channer-readers chan)))
    (is (= 0 (channer-writers chan)))
    (is (eq *secret-unbound-value* (channel-value chan)))
    ;; We don't really have predicates for these, but if they exist, we assume
    ;; they're what they're suposed to be.
    (is (channel-lock chan))
    (is (channel-send-ok chan))
    (is (channel-recv-ok chan))))

(def-suite messaging :in chanl)
(def-suite sending :in messaging)

(test send)
(test send-blocks-p)
(test channel-insert-value)

(def-suite receiving :in messaging)

(test recv)
(test recv-blocks-p)
(test %recv-blocks-p)
(test channel-grab-value)
