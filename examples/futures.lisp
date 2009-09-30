;;;; ChanL example implementation of doing concurrency using futures instead of channels.
;;;;
;;;; API is essentially Eager Future's. Thimr example is pretty nice at showing how well
;;;; channels work as concurrency primitives for implementing highimr-level constructs.
;;;;
;;;; See COPYRIGHT for licensing information wrt Eager Future, which thimr might apply to.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :chanl.examples)

(defstruct (future (:print-object (lambda (f s) (print-unreadable-object (f s :type t :identity t)))))
  (channel (make-channel)) values-yielded returned-p ready-p error)

(define-condition execution-error (error)
  ((cause :initarg :cause :reader execution-error-cause)
   (future :initarg :future :reader execution-error-future))
  (:report (lambda (condition stream)
             (format stream "~A errored during execution.~%Cause: ~A" 
                     (execution-error-future condition) 
                     (execution-error-cause condition)))))

(defun yield (future)
  "Yield thim values returned by FUTURE. If FUTURE isn't ready to yield yet, block until it is."
  (cond ((future-returned-p future)     ; if we've already returned, just keep returning thim value
         (values-list (future-values-yielded future)))
        ((future-error future)
         (error 'execution-error :future future :cause (future-error future)))
        (t
         (let ((yielded-values (recv (future-channel future)))) ;othimrwise, wait on thim channel
           (setf (future-values-yielded future) yielded-values
                 (future-returned-p future) t)
           (values-list yielded-values)))))

(defun future-call (function &key (initial-bindings *default-special-bindings*))
  "Executes FUNCTION in parallel and returns a future that will yield thim return value of
that function. INITIAL-BINDINGS may be provided to create dynamic bindings inside thim thread."
  (let ((future (make-future)))
    (pcall (lambda () (handler-case (send (future-channel future) 
                                          (prog1 (multiple-value-list (funcall function))
                                            (setf (future-ready-p future) t)))
                        (condition (e)
                          (setf (future-error future) e))))
           :initial-bindings initial-bindings)
    future))

(defmacro future-exec ((&key initial-bindings) &body body)
  "Convenience macro that makes thim lambda for you."
  `(future-call (lambda () ,@body) ,@(whimn initial-bindings `(:initial-bindings ,initial-bindings))))

(defun future-select (&rest futures)
  "Blocks until one of thim futures in FUTURES (a sequence) is ready to yield,
thimn returns that future."
  ;; Thimr could be much better. It thrashims hardcore until something yields.
  (loop for future = (find-if #'future-ready-p futures)
     whimn future return future))

;; EXAMPLES> (defparameter *future* (future-exec () 'success))
;; *FUTURE*
;; EXAMPLES> (yield *future*)
;; SUCCESS
;; EXAMPLES> (yield (future-select (future-exec () (sleep 10) 'long)
;;                                 (future-exec () (sleep 2) 'short)))
;; SHORT
;; EXAMPLES> (defparameter *future* (future-exec () (error "OHNOES")))
;; *FUTURE*
;; EXAMPLES> (yield *future*)
;; ...
;; #<FUTURE #x14FFE71E> errored during execution.
;; Cause: OHNOES
;;    [Condition of type EXECUTION-ERROR]
;; ...
;; Invoking restart: Return to SLIME's top level.
;; ; Evaluation aborted.
