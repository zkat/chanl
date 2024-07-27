;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan, Adlai Chandrasekhar
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :chanl)

(def-suite channels :in chanl)
(def-suite construction :in channels)
(in-suite construction)

(test make-unbuffered
  (let ((chan (make-instance 'channel)))
    (is (channelp chan))
    (is (not (channel-buffered-p chan)))
    (is (= 0 (channel-readers chan)))
    (is (= 0 (channel-writers chan)))
    (is (eq *secret-unbound-value* (channel-value chan)))
    (is (send-blocks-p chan))
    (is (recv-blocks-p chan))))

#+ (or sbcl (and ccl (or x86 x86_64)))
(test make-cas
  (let ((chan (make-instance 'cas-channel)))
    (is (channelp chan))
    (is (not (channel-buffered-p chan)))
    (is (= 0 (channel-readers chan)))
    (is (= 0 (channel-writers chan)))
    (is (eq *secret-unbound-value* (channel-value chan)))
    (is (send-blocks-p chan))
    (is (recv-blocks-p chan))))

(test make-stack
  (let ((chan (make-instance 'stack-channel)))
    (is (channelp chan))
    (is (channel-buffered-p chan))
    (is (null (channel-value chan)))
    (is (= 0 (channel-readers chan)))
    (is (= 0 (channel-writers chan)))
    (is (not (send-blocks-p chan)))
    (is (recv-blocks-p chan))))

(test make-bounded
  (let ((chan (make-instance 'bounded-channel :size 10)))
    (is (channelp chan))
    (is (channel-buffered-p chan))
    (is (queuep (channel-value chan)))
    (is (= 10 (queue-length (channel-value chan))))
    (is (= 0 (channel-readers chan)))
    (is (= 0 (channel-writers chan)))
    (is (not (send-blocks-p chan)))
    (is (recv-blocks-p chan))))

(test make-unbounded
  (let ((chan (make-instance 'unbounded-channel)))
    (is (channelp chan))
    (is (channel-buffered-p chan))
    (is (equal '(()) (channel-value chan)))
    (is (= 0 (channel-readers chan)))
    (is (= 0 (channel-writers chan)))
    (is (not (send-blocks-p chan)))
    (is (recv-blocks-p chan))))

(test make-invalid
  (signals error (make-instance 'buffered-channel :size nil))
  (signals error (make-instance 'buffered-channel :size -1)))

(def-suite messaging :in channels)
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
    (sleep 0.5) ;hax to let the thread start working
    (is (eq channel (send channel 'test :blockp nil)))))

#+sbcl                                  ; FIXME! livelock on ccl
(test send-cas
  (let ((channel (make-instance 'cas-channel)))
    (is (null (send channel 'test :blockp nil)))
    (pexec () (recv channel))
    (is (eq channel (send channel 'test)))
    (pexec () (recv channel))
    (is (eq channel (send channel 'test)))
    (pexec () (recv channel))
    (sleep 0.5)                         ; hax, wait for read-state
    (is (eq channel (send channel 'test :blockp nil)))))

(test send-buffered
  (let ((channel (make-instance 'bounded-channel :size 1)))
    (let ((datum (gensym)))
      (is (eq channel (send channel datum :blockp nil))
          "Failed to send to buffer without blocking")
      (is (eq datum (recv channel :blockp nil))
          "Failed to recieve from buffer without blocking"))
    (let ((datum (gensym)))
      (is (eq channel (send channel datum))
          "Failed to send to buffer with blocking")
      (is (eq datum (recv channel :blockp nil))
          "Failed to recieve from buffer with blocking"))
    (let ((datum (gensym)))
      (send channel datum)
      (is (null (send channel 'test :blockp nil))
          "Failed to fill buffer with blocking")
      (is (eq datum (recv channel))
          "Failed to receive from filled buffer"))
    (let ((datum (gensym)))
      (is (eq channel (send channel datum))
          "Failed to fill buffer synchronously")
      (is (null (send channel datum :blockp nil))
          "Failed to avoid blocking on full buffer")
      (is (eq datum (recv channel))
          "Failed to send synchronously to buffer"))))

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
    ;; repeat it just to make sure it doesn't fuck up the second time around
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

(def-suite racing :in channels)
(in-suite racing)

(defun setup-race (thread-count class &rest channel-args)
  (let ((lock (bt:make-lock "bt:semaphore")) (nrx 0) (ntx 0) start
        (channel (apply #'make-instance class channel-args)))
    (macrolet ((with-counter ((place) &body body)
                 `(unwind-protect
                       (progn (bt:with-lock-held (lock) (incf ,place)) ,@body)
                    (bt:with-lock-held (lock) (decf ,place))))
               (await (place) `(loop :until (= ,place thread-count))))
      (flet ((recver () (with-counter (nrx) (recv channel)))
             (sender (x)
               (lambda ()
                 (with-counter (ntx)
                   (loop :until start :do (bt:thread-yield))
                   (send channel x))))
             (strcat (&rest things) (format () "~{~A~}" things)))
        (let ((threads (loop :for n :below thread-count
                          :collect (bt:make-thread #'recver :name (strcat "r" n))
                          :collect (bt:make-thread (sender n) :name (strcat "s" n)))))
          (await nrx) (await ntx) (setf start t)
          (values threads channel))))))

(defmacro racing-test-cases (name class &rest counts)
  `(test ,name
     (dolist (count ',counts)
       (multiple-value-bind (threads channel) (setup-race count ',class)
         (let* ((pass nil)
                (verifier (pexec ()
                            (mapc #'bt:join-thread threads)
                            (setf pass t))))
           (sleep 1)
           (is (eq pass t)
               (concatenate
                'string (format () "count=~D, ~A" count ',name)
                (with-output-to-string (*standard-output*)
                  (format t "~%~%Contested Channel:~%")
                  (with-slots (lock) channel
                    (mapc #'describe (list channel lock)))
                  (format t "~%~%Competing Threads:~{~&~A~%~}"
                          (remove-if-not #'bt:thread-alive-p threads)))))
           (unless pass
             (mapc #'bt:destroy-thread
                   (remove-if-not #'bt:thread-alive-p threads))
             (kill (task-thread verifier))))))))

(racing-test-cases race-unbuffered channel 3 6 10 15 21)
(racing-test-cases race-stack stack-channel 3 6 10 15 21)
(racing-test-cases race-buffered bounded-channel 3 6 10 15 21)
(racing-test-cases race-unbounded unbounded-channel 3 6 10 15 21)
