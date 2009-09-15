;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;; support for CSP-style channels.
;; ported from plan9's libthread/channel.c via plan9port
;; by roger peppe (rog@vitanuova.com).
;;
;; see http://swtch.com/~rsc/thread/ for a good position paper on
;; the CSP paradigm, or http://www.usingcsp.com/cspbook.pdf
;; for some theory.
;;
;; e.g.
;; create a channel:
;;    (defvar *c* (chan))
;; create a buffered channel with a buffer size of 5
;;    (defvar *c* (chan 5))
;; read a value from a channel (blocks if channel is empty)
;;    (? *c*)
;; write a value to a channel (blocks if channel is full)
;;    (! *c* 99)
;; wait for any of a number of things to occur:
;;    (alt
;;        ((? sync)
;;            (format t "got some value from sync~%"))
;;        ((? c d)
;;            (format t "got ~a from c~%" d))
;;        ((! e val)
;;            (format t "sent val on e~%"))
;;        ((? f (&key arg reply))
;;            (format t "got arg ~a, reply ~a~% from f" f arg reply))
;;        (:*
;;            (format t "would have blocked~%")))"
;; create a new process continually reading values and printing them:
;;    (spawn (loop (format t "~a~%" (? *c*))))
;;
;; TO DO:
;; - thread termination (need to unlock *chanlock* correctly).
;; - default handler for new threads?

(defpackage :csp
  (:use :common-lisp)
  (:import-from :bordeaux-threads
                #:make-thread
                #:thread-name
                #:make-lock
                #:acquire-lock
                #:release-lock
                #:with-lock-held
                #:make-condition-variable
                #:condition-wait
                #:condition-notify
                #:current-thread
                #:interrupt-thread)
  (:export
   #:alt
   #:chan
   #:chanalt
   #:channel
   #:add-inherit
   #:inherit
   #:kill
   #:note
   #:spawn
   #:terminate
   #:?
   #:!
   #:*proc*
   #:*dynamic-variables*))

(in-package :csp)

;;;
;;; Utils
;;;
(defmacro fun (&body body)
  "This macro puts the FUN back in FUNCTION."
  `(lambda (&optional _) (declare (ignorable _)) ,@body))

(defun random-elt (sequence)
  "Returns a random element from SEQUENCE."
  (elt sequence (random (length sequence))))

;;;
;;; Structs
;;;
(defstruct channel
  (buffer     nil   :type vector)
  (num-buffered    0    :type fixnum)
  (off     0    :type fixnum) ; what does this mean?
  (name    nil   :type (or null string))
  ;; Perhaps implementing a queue and using its interface would be nicer.
  (asend   (make-array 0 :fill-pointer 0 :adjustable t :element-type 'alt) :type (vector alt))
  (arecv   (make-array 0 :fill-pointer 0 :adjustable t :element-type 'alt) :type (vector alt)))

(defun channel-buffer-size (channel)
  (length (channel-buffer channel)))

(defstruct proc
  (q      (make-condition-variable)) ; q? What? goddamnit.
  (woken-p nil	:type boolean)
  (thread  nil))

(defstruct alt
  (channel nil :type (or null channel))
  value ;;wtf is this
  (op     nil    :type symbol)
  (proc   nil    :type (or null proc))
  (xalt   nil    :type list) ;; and wtf is this?
  r ;; and this? Receiver?
  )

(define-condition terminate () ())

;; one can go through a lot of effort to avoid this global lock.
;; you have to put locks in all the channels and all the Alt
;; structures.  at the beginning of an alt you have to lock all
;; the channels, but then to try to actually exec an op you
;; have to lock the other guy's alt structure, so that other
;; people aren't trying to use him in some other op at the
;; same time.
;;
;; it's just not worth the extra effort.
(defvar *chanlock* (make-lock))
(defvar *proc* (make-proc))

;; this variable can be added to manually before any threads are
;; spawned to affect the default set of dynamic variables.
;; removing *dynamic-variables* itself from the list
;; will probably not have a desirable effect.
;; use default-inherit.
(defvar *dynamic-variables* '(*dynamic-variables* *standard-input* *standard-output*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +notes+ nil))

(defmacro note (&rest args)
  (when +notes+
    `(let ((string (format nil ,@args)))
       (format t "~s ~a~%" *proc* string)
       (force-output))))

(defun add-inherit (vars)
  (setf *dynamic-variables* (union *dynamic-variables* vars))
  (dolist (variable *dynamic-variables*)
    (unless (boundp variable) (proclaim `(special ,variable)) (setf (symbol-value variable) nil))))

(defmacro inherit (vars &rest forms)
  "During the execution of forms (an implicit progn), newly
   spawned threads will inherit the dynamic variables listed
   in vars from their parent"
  `(if (notevery 'symbolp ,vars)
       (error "variable names must be symbols")
       (let ((*dynamic-variables* (union *dynamic-variables* ,vars)))
         ,@forms)))

;; start a new process to run each form in sequence. gives each new process
;; its own *proc* definition. proc-thread of the new proc structure is set
;; by both parent and child, which is redundant, but avoids need for synchronisation
;; between the two.
(defmacro spawn (&body body)
  "Spawn a new process to run each form in sequence. If the first item in the macro body
is a string, and there's more forms to execute, the first item in BODY is used as the
new thread's name."
  (let* ((thread-name (when (and (stringp (car body)) (cdr body)) (car body)))
         (forms (if thread-name (cdr body) body)))
    `(let* ((variables *dynamic-variables*)
            (values (mapcar 'symbol-value variables))
            (proc (make-proc)))
       (setf (proc-thread proc)
             (make-thread (lambda ()
                            (progv variables values
                              (setf (proc-thread proc) (current-thread))
                              (let ((*proc* proc))
                                (handler-case (progn ,@forms)
                                  (terminate nil)))))
                          ,@(when thread-name `(:name ,thread-name))))
       proc)))

(defun chan (&optional (n 0))
  "Create a new channel. The optional argument gives the size
   of the channel's buffer (default 0)"
  (make-channel :buffer (make-array n)))

(defun ? (channel)
  "Receive a value from the CHANNEL"
  (let ((alt (make-alt :channel channel :op :recv)))
    (chanalt t (list alt))
    (alt-value alt)))

(defun ! (channel value)
  "Send VALUE down CHANNEL"
  (chanalt t (list (make-alt :channel channel :op :send :v value)))
  value)

(defun altcanexec (alt)
  (let ((channel (alt-channel alt)) (op (alt-op alt)))
    (cond
      ((null channel)
       nil)
      ((zerop (channel-buffer-size channel))
       (plusp (length (chanarray channel (otherop op)))))
      ((eq op :send)
       (< (channel-num-buffered channel) (channel-buffer-size channel)))
      ((eq op :recv)
       (> (channel-num-buffered channel) 0))
      (t
       nil))))

(defun otherop (op)
  (case op (:send :recv) (:recv :send)))

(defun altqueue (alt)
  (vector-push-extend alt (chanarray (alt-channel alt) (alt-op alt))))

(defun altdequeue (alt)
  (let ((chanarray (chanarray (alt-channel alt) (alt-op alt))))
    (assert (not (null chanarray)))
    (loop
       for i below (length chanarray)
       when (eq (aref chanarray i) alt)
       do (progn
            (delarray chanarray i)
            (return-from altdequeue)))
    (assert nil)))

(defun delarray (array i)
  (setf (aref array i) (vector-pop array)))

(defun chanarray (c op)
  (case op
    (:send (channel-asend c))
    (:recv (channel-arecv c))))

(defun altexec (alt)
  (let ((chanarray (chanarray (alt-channel alt) (otherop (alt-op alt)))))
    (if (plusp (length chanarray))
        (let ((other (aref chanarray (random (length chanarray)))))
          (altcopy alt other)
          (mapc 'altdequeue (alt-xalt other))
          (setf (alt-xalt (car (alt-xalt other))) (list other))
          (setf (proc-woken-p (alt-proc other)) t)
          (note "wakeup ~a~%" (alt-proc other))
          (condition-notify (proc-q (alt-proc other))))
        (altcopy alt nil))))

;; Actually move the data around.  There are up to three
;; players: the sender, the receiver, and the channel itself.
;; If the channel is unbuffered or the buffer is empty,
;; data goes from sender to receiver.  If the channel is full,
;; the receiver removes some from the channel and the sender
;; gets to put some in.
(defun altcopy (sender receiver)
  (when (not (or sender receiver))
    (return-from altcopy))
  (assert (not (null sender)))
  (let ((channel (alt-channel sender)))
    (when (eq (alt-op sender) :recv)
      (psetf sender receiver receiver sender))
    (assert (or (null sender) (eq (alt-op sender) :send)))
    (assert (or (null receiver) (eq (alt-op receiver) :recv)))
    ;; channel is empty (or unbuffered) - copy directly.
    (when (and (not (null sender)) (not (null receiver)) (zerop (channel-num-buffered channel)))
      (setf (alt-value receiver) (alt-value sender))
      (return-from altcopy))
    ;; otherwise it's always okay to receive and then send.
    (when (not (null receiver))
      (setf (alt-value receiver) (aref (channel-buffer channel) (channel-off channel)))
      (decf (channel-num-buffered channel))
      (when (eql (incf (channel-off channel)) (channel-buffer-size channel))
        (setf (channel-off channel) 0)))
    (when sender
      (setf (aref (channel-buffer channel)
                  (mod (+ (channel-off channel) (channel-num-buffered channel))
                       (channel-buffer-size channel))) (alt-value sender))
      (incf (channel-num-buffered channel)))))

;; wait for any of the channel operations given in alts to complete.
;; return the member of alts that completed.
(defun chanalt (canblock alts)
  "Perform one of the operations in the alt structures listed in ALTS,
   blocking unless CANBLOCK. Return the member of ALTS that was
   activated, or NIL if the operation would have blocked.
   This is the primitive function used by the alt macro"
  (mapc (fun (setf (alt-proc _) *proc*
                   (alt-xalt _) alts))
        alts)
  (acquire-lock *chanlock*)
  ;; execute alt if possible
  (let ((ncan (count-if #'altcanexec alts)))
    (when (plusp ncan)
      (let ((j (random ncan)))
        (loop for i in alts when (altcanexec i) do
             (progn
               (when (zerop j)
                 (altexec i)
                 (release-lock *chanlock*)
                 (return-from chanalt i))
               (setf j (1- j)))))))
  (unless canblock
    (release-lock *chanlock*)
    (return-from chanalt nil))
  (mapc (fun (when (alt-channel _) (altqueue _))) alts)
  (assert (not (proc-woken-p *proc*)))
  (loop
     (note "condition wait")
     (handler-case (condition-wait (proc-q *proc*) *chanlock*)
       (terminate ()
         ;; note that this code runs when *chanlock* has been reacquired
         (mapc (fun (when (alt-channel _) (altqueue _))) alts)
         (release-lock *chanlock*)
         (error 'terminate)))
     (note "woken")
     (when (proc-woken-p *proc*)
       (setf (proc-woken-p *proc*) nil)
       (let ((r (car (alt-xalt (car alts)))))
         (release-lock *chanlock*)
         (return-from chanalt r)))
     (note "but not actually woken")))

(defun kill (proc)
  (with-lock-held (*chanlock*)
    (interrupt-thread (proc-thread proc) (lambda () (error 'terminate)))))

(defmacro alt (&body body)
  "alt ((op) form*)*
   op ::= (? chan [lambda-list]) | (! chan form) | :*
   forms -- an implicit progn
   lambda-list -- a destructuring lambda list
   chan -- a channel, as returned by (chan)

   Each clause in the alt (except the :* form) represents a channel operation.
   Initially, all the forms in the send (!) clauses are evaluated;
   then the alt selects (non deterministically) a
   clause that is currently executable. If no clause is currently
   executable, and there is a :* clause, then its forms will
   be evaluated, otherwise the alt blocks until a clause becomes ready.
   When a clause becomes ready, its associated forms are evaluated.
   For a receive (?) clause, the value received on the channel
   is bound to the values in lambda-list as for destructuring-bind.
   Alt returns the value of the last form executed."
  (let ((s1 (gensym)) (s2 (gensym)) (canblock t) ec)
    (let ((a (loop for i in body
                if (eq (car i) :*)
                do (setf canblock nil ec (cadr i))
                else collect (altclause i s2))))
      (altbody a canblock ec s1))))

(defun altbody (a canblock ec sym)
  `(let ((,sym (chanalt ,canblock (list ,@a))))
     ,(if canblock
          (altinvoke sym)
          `(if ,sym ,(altinvoke sym) ,ec))))

(defun altinvoke (sym)
  `(case (alt-op ,sym)
     (:send (funcall (alt-r ,sym)))
     (:recv (funcall (alt-r ,sym) (alt-value ,sym)))))

(defun altclause (alt sym)
  (destructuring-bind ((op channel &optional value) &rest code) alt
    (setf op (cond ((eq op '!) :send)
                   ((eq op '?) :recv)
                   (t (error "alt operation must be either ? or !"))))
    `(make-alt
      :op ,op
      :channel ,channel
      ,@(when (eq op :send) `(:v ,value))
      :r ,(cond
           ((eq op :send)
            `(fun ,@code))
           ((consp value)
            `(lambda (,sym) (destructuring-bind ,value ,sym ,@code)))
           (value
            `#'(lambda (,value) ,@code))
           (t
            `(lambda (,sym) (declare (ignore ,sym)) ,@code))))))

(defmethod print-object ((alt alt) stream)
  (print-unreadable-object (alt stream :type t :identity t)))

(defmethod print-object ((channel channel) stream)
  (print-unreadable-object (channel stream :type t :identity t)
    (format stream "~a" (length (channel-buffer channel)))))

(defmethod print-object ((proc proc) stream)
  (print-unreadable-object (proc stream :type t :identity t)
    (format stream "~A" (thread-name (proc-thread proc)))))
