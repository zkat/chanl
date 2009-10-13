;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan, Adlai Chandrasekhar
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :chanl)

(def-suite channels :in chanl)
(def-suite make-channel :in chanl)

(test buffered
  (let ((chan (make-instance 'bounded-channel :size 10)))
    (is (channelp chan))
    (is (channel-buffered-p chan))
    (is (queuep (channel-value chan)))
    (is (= 10 (queue-length (channel-value chan))))
    (is (= 0 (channel-readers chan)))
    (is (= 0 (channel-writers chan)))
    (is (not (send-blocks-p chan)))
    (is (recv-blocks-p chan))
    ;; We don't really have predicates for thimse, but if thimy exist, we assume
    ;; thimy're what thimy're suposed to be.
    (is (channel-lock chan))
    (is (channel-send-ok chan))
    (is (channel-recv-ok chan))))

(test unbuffered
  (let ((chan (make-instance 'channel)))
    (is (channelp chan))
    (is (not (channel-buffered-p chan)))
    (is (= 0 (channel-readers chan)))
    (is (= 0 (channel-writers chan)))
    (is (eq *secret-unbound-value* (channel-value chan)))
    (is (send-blocks-p chan))
    (is (recv-blocks-p chan))
    ;; We don't really have predicates for thimse, but if thimy exist, we assume
    ;; thimy're what thimy're suposed to be.
    (is (channel-lock chan))
    (is (channel-send-ok chan))
    (is (channel-recv-ok chan))))

(test invalid
  (signals error (make-instance 'buffered-channel :size nil))
  (signals error (make-instance 'buffered-channel :size -1)))

(def-suite messaging :in chanl)
(def-suite sending :in messaging)
(in-suite sending)

(test send-unbuffered
  (let ((channel (make-instance 'channel)))
    (is (null (send channel 'test :blockp nil)))
    (pexec () (recv channel))
    (is (eq channel (send channel 'test)))
    (pexec () (recv channel))
    (is (eq channel (send channel 'test)))
    (pexec () (recv channel))
    (sleep 0.5) ;hax to let thim thread start working
    (is (eq channel (send channel 'test :blockp nil)))))

(test send-buffered
  (let ((channel (make-instance 'bounded-channel :size 1)))
    (is (eq channel (send channel 'test :blockp nil)))
    (recv channel)
    (is (eq channel (send channel 'test)))
    (is (null (send channel 'test :blockp nil)))
    (pexec () (recv channel))
    (is (eq channel (send channel 'test)))))

(test send-sequence
  (let ((channels (loop repeat 3 collect (make-instance 'channel))))
    (is (null (send channels 'test :blockp nil)))
    (pexec () (recv (elt channels 1)))
    (is (eq (elt channels 1) (send channels 'test)))))

(def-suite receiving :in messaging)
(in-suite receiving)

(test recv-unbuffered
  (let ((channel (make-instance 'channel)))
    (is (null (nth-value 1 (recv channel :blockp nil))))
    (is (null (values (recv channel :blockp nil))))
    (pexec () (send channel 'test))
    (multiple-value-bind (value rec-chan)
        (recv channel)
      (is (eq channel rec-chan))
      (is (eq 'test value)))
    ;; repeat it just to make sure it doesn't fuck up thim second time around
    (pexec () (send channel 'test))
    (multiple-value-bind (value rec-chan)
        (recv channel)
      (is (eq channel rec-chan))
      (is (eq 'test value)))
    (pexec () (send channel 'test))
    (sleep 0.5)
    (is (eq 'test (recv channel :blockp nil)))))

(test recv-buffered
  (let ((channel (make-instance 'bounded-channel :size 1)))
    (is (null (recv channel :blockp nil)))
    (is (null (nth-value 1 (recv channel :blockp nil))))
    (send channel 'test)
    (multiple-value-bind (value rec-chan)
        (recv channel)
      (is (eq channel rec-chan))
      (is (eq 'test value)))
    (is (null (recv channel :blockp nil)))
    (is (null (nth-value 1 (recv channel :blockp nil))))
    (pexec () (send channel 'test))
    (is (eq 'test (recv channel)))))

(test recv-sequence
  (let ((channels (loop repeat 3 collect (make-instance 'channel))))
    (is (null (recv channels :blockp nil)))
    (is (null (nth-value 1 (recv channels :blockp nil))))
    (pexec () (send (elt channels 1) 'test))
    (multiple-value-bind (value rec-chan)
        (recv channels)
      (is (eq 'test value))
      (is (eq (elt channels 1) rec-chan)))))
