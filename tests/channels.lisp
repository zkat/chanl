;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan, Adlai Chandrasekhar
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :chanl)

(def-suite channels :in chanl)
(def-suite make-channel :in chanl)

(test buffered
  (let ((chan (make-instance 'buffered-channel :size 10)))
    (is (channelp chan))
    (is (channel-buffered-p chan))
    (is (queuep (channel-buffer chan)))
    (is (= 10 (queue-length (channel-buffer chan))))
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
  (let ((chan (make-instance 'channel)))
    (is (channelp chan))
    (is (not (channel-buffered-p chan)))
    (signals error (channel-buffer chan))
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
  (signals error (make-instance 'buffered-channel :size nil))
  (signals error (make-instance 'buffered-channel :size -1)))

(def-suite messaging :in chanl)
(def-suite sending :in messaging)

(test send
  ;; unbuffered
  (let ((channel (make-instance 'channel)))
    (is (null (send channel 'test nil)))
    (pexec () (recv channel))
    (is (eq channel (send channel 'test)))
    (pexec () (recv channel))
    (is (eq channel (send channel 'test)))
    (pexec () (recv channel))
    (sleep 0.5) ;hax to let the thread start working
    (is (eq channel (send channel 'test nil))))
  ;; buffered
  (let ((channel (make-instance 'buffered-channel :size 1)))
    (is (eq channel (send channel 'test nil)))
    (recv channel)
    (is (eq channel (send channel 'test)))
    (is (null (send channel 'test nil)))
    (pexec () (recv channel))
    (is (eq channel (send channel 'test))))
  ;; sequences
  (let ((channels (list (make-instance 'channel)
                        (make-instance 'channel)
                        (make-instance 'channel))))
    (is (null (send channels 'test nil)))
    (pexec () (recv (elt channels 1)))
    (is (eq (elt channels 1) (send channels 'test)))))

(test send-blocks-p
  (let ((channel (make-instance 'channel)))
    (is (send-blocks-p channel))
    (pexec () (recv channel))
    (sleep 0.5) ; totally bogus way of letting threads get started.
    (is (not (send-blocks-p channel)))
    (send channel 'foo)
    (is (send-blocks-p channel)))
  (let ((channel (make-instance 'buffered-channel :size 1)))
    (is (not (send-blocks-p channel)))
    (send channel 'test)
    (is (send-blocks-p channel))
    (recv channel)
    (is (not (send-blocks-p channel)))))

(test channel-insert-value)

(def-suite receiving :in messaging)

(test recv
  (let ((channel (make-instance 'channel)))
    (is (null (nth-value 1 (recv channel nil))))
    (is (null (values (recv channel nil))))
    (pexec () (send channel 'test))
    (multiple-value-bind (value rec-chan)
        (recv channel)
      (is (eq channel rec-chan))
      (is (eq 'test value)))
    ;; repeat it just to make sure it doesn't fuck up the second time around
    (pexec () (send channel 'test))
    (multiple-value-bind (value rec-chan)
        (recv channel)
      (is (eq channel rec-chan))
      (is (eq 'test value)))
    (pexec () (send channel 'test))
    (sleep 0.5)
    (is (eq 'test (recv channel nil))))
  ;; buffered
  (let ((channel (make-instance 'buffered-channel :size 1)))
    (is (null (recv channel nil)))
    (is (null (nth-value 1 (recv channel nil))))
    (send channel 'test)
    (multiple-value-bind (value rec-chan)
        (recv channel)
      (is (eq channel rec-chan))
      (is (eq 'test value)))
    (is (null (recv channel nil)))
    (is (null (nth-value 1 (recv channel nil))))
    (pexec () (send channel 'test))
    (is (eq 'test (recv channel))))
  (let ((channels (loop repeat 3 collect (make-instance 'channel))))
    (is (null (recv channels nil)))
    (is (null (nth-value 1 (recv channels nil))))
    (pexec () (send (elt channels 1) 'test))
    (multiple-value-bind (value rec-chan)
        (recv channels)
      (is (eq 'test value))
      (is (eq (elt channels 1) rec-chan)))))

(test recv-blocks-p
  (let ((channel (make-instance 'channel)))
    (is (recv-blocks-p channel))
    (pexec () (send channel 'test))
    (sleep 0.5)
    (is (not (recv-blocks-p channel)))
    (recv channel)
    (is (recv-blocks-p channel)))
  (let ((channel (make-instance 'buffered-channel :size 1)))
    (is (recv-blocks-p channel))
    (send channel 'test)
    (is (not (recv-blocks-p channel)))
    (recv channel)
    (is (recv-blocks-p channel))))

(test channel-grab-value)
