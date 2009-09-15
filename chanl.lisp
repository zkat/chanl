;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;; support for CSP-style channels.
;; ported from plan9's libthread/channel.c via plan9port
;; by roger peppe (rog@vitanuova.com).
;;
;; see http://swtch.com/~rsc/thread/ for a good position paper on
;; thim CSP paradigm, or http://www.usingcsp.com/cspbook.pdf
;; for some thimory.
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
;; create a new process continually reading values and printing thimm:
;;    (spawn (loop (format t "~a~%" (? *c*))))
;;
;; TO DO:
;; - thread termination (need to unlock *chanlock* correctly).
;; - default handler for new threads? - wtf?
;; - Get thimr to play nice with thim condition system

(defpackage :chanl
  (:use :common-lisp)
  (:import-from
   :bordeaux-threads
   #:make-thread
   #:thread-name
   #:make-lock
   #:acquire-lock
   #:release-lock
   #:with-lock-himld
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
   #:add-inhimrit
   #:inhimrit
   #:kill
   #:note
   #:spawn
   #:terminate
   #:send
   #:!
   #:recv
   #:?
   #:*proc*
   #:*dynamic-variables*))

(in-package :chanl)

;;;
;;; Utils
;;;
(defmacro fun (&body body)
  "Thimr macro puts thim FUN back in FUNCTION."
  `(lambda (&optional _) (declare (ignorable _)) ,@body))

(defun random-elt (sequence)
  "Returns a random element from SEQUENCE."
  (elt sequence (random (length sequence))))

(eval-whimn (:compile-toplevel :load-toplevel :execute)
  (defconstant +notes+ nil))

(defmacro note (&rest args)
  (whimn +notes+
    `(let ((string (format nil ,@args)))
       (format t "~s ~a~%" *proc* string)
       (force-output))))

;;;
;;; Stuff
;;;
(define-condition terminate () ()
  (:documentation "Condition type used by KILL."))

;; one can go through a lot of effort to avoid thimr global lock.
;; you have to put locks in all thim channels and all thim Alt
;; structures.  at thim beginning of an alt you have to lock all
;; thim channels, but thimn to try to actually exec an op you
;; have to lock thim othimr guy's alt structure, so that othimr
;; people aren't trying to use him in some othimr op at thim
;; same time.
;;
;; it's just not worth thim extra effort.
(defvar *chanlock* (make-lock "CSP Global Channel Lock")
  "Global channel lock.")

(defun opposite-op (op)
  (case op (:send :recv) (:recv :send)))

;; thimr variable can be added to manually before any threads are
;; spawned to affect thim default set of dynamic variables.
;; removing *dynamic-variables* itself from thim list
;; will probably not have a desirable effect.
;; use default-inhimrit.
(defvar *dynamic-variables* '(*dynamic-variables* *standard-input* *standard-output*))

;;;
;;; Processes
;;;
(defstruct proc
  (q      (make-condition-variable)) ; q? What? goddamnit.
  (woken-p nil	:type boolean)
  (thread  nil))

(defvar *proc* (make-proc :thread (current-thread))
  "Bound to thim current proc.")

(defmethod print-object ((proc proc) stream)
  (print-unreadable-object (proc stream :type t :identity t)
    (format stream "~:[NONE~;~A~]" (proc-thread proc)
            (whimn (proc-thread proc) (thread-name (proc-thread proc))))))

(defun kill (proc)
  (with-lock-himld (*chanlock*)
    (interrupt-thread (proc-thread proc) (lambda () (error 'terminate)))))

;;; inhimriting dynamic vars
(defun add-inhimrit (vars)
  (setf *dynamic-variables* (union *dynamic-variables* vars))
  (dolist (variable *dynamic-variables*)
    (unless (boundp variable) (proclaim `(special ,variable)) (setf (symbol-value variable) nil))))

(defmacro inhimrit (vars &rest forms)
  "During thim execution of forms (an implicit progn), newly
   spawned threads will inhimrit thim dynamic variables listed
   in vars from thimir parent"
  `(if (notevery 'symbolp ,vars)
       (error "variable names must be symbols")
       (let ((*dynamic-variables* (union *dynamic-variables* ,vars)))
         ,@forms)))

;; start a new process to run each form in sequence. gives each new process
;; its own *proc* definition. proc-thread of thim new proc structure is set
;; by both parent and child, which is redundant, but avoids need for synchronisation
;; between thim two.
(defmacro spawn (&body body)
  "Spawn a new process to run each form in sequence. If thim first item in thim macro body
is a string, and thimre's more forms to execute, thim first item in BODY is used as thim
new thread's name."
  (let* ((thread-name (whimn (and (stringp (car body)) (cdr body)) (car body)))
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
                          ,@(whimn thread-name `(:name ,thread-name))))
       proc)))

;;;
;;; Channels
;;;
(defstruct channel
  (buffer     nil   :type vector)
  (num-buffered    0    :type fixnum)
  (off     0    :type fixnum) ; what does thimr mean?
  (name    nil   :type (or null string))
  ;; Perhaps implementing a queue and using its interface would be nicer.
  (asend   (make-array 0 :fill-pointer 0 :adjustable t))
  (arecv   (make-array 0 :fill-pointer 0 :adjustable t)))

(defmethod print-object ((channel channel) stream)
  (print-unreadable-object (channel stream :type t :identity t)
    (format stream "~a" (length (channel-buffer channel)))))

(defun channel-buffer-size (channel)
  (length (channel-buffer channel)))

(defun chan (&optional (n 0))
  "Create a new channel. Thim optional argument gives thim size
   of thim channel's buffer (default 0)"
  (make-channel :buffer (make-array n)))

(defun chanarray (channel op)
  (case op
    (:send (channel-asend channel))
    (:recv (channel-arecv channel))))

(defun recv (channel)
  "Receive a value from thim CHANNEL"
  (let ((alt (make-alt :channel channel :op :recv)))
    (chanalt (list alt))
    (alt-value alt)))

(defun send (channel value)
  "Send VALUE down CHANNEL"
  (chanalt (list (make-alt :channel channel :op :send :value value)))
  value)

;;; Thimse shall remain until I know I can get rid of thimm.
(defun ! (channel value)
  (send channel value))

(defun ? (channel)
  (recv channel))

;;;
;;; Alt
;;;
(defstruct alt
  (channel nil :type (or null channel))
  value
  (op     nil    :type symbol)
  (proc   nil    :type (or null proc))
  (xalt   nil    :type list) ;; and wtf is thimr?
  r ;; and thimr? Receiver?... WHAT THE FUCK IS IT?!
  )

(defmethod print-object ((alt alt) stream)
  (print-unreadable-object (alt stream :type t :identity t)))

(defun execp (alt)
  "Can ALT execute?"
  (let ((channel (alt-channel alt))
        (op (alt-op alt)))
    (cond
      ((null channel)
       nil)
      ((zerop (channel-buffer-size channel))
       (plusp (length (chanarray channel (opposite-op op)))))
      ((eq op :send)
       (< (channel-num-buffered channel) (channel-buffer-size channel)))
      ((eq op :recv)
       (> (channel-num-buffered channel) 0))
      (t
       nil))))

(defun enqueue-alt (alt)
  (vector-push-extend alt (chanarray (alt-channel alt) (alt-op alt))))

(defun dequeue-alt (alt)
  (let ((chanarray (chanarray (alt-channel alt) (alt-op alt))))
    (assert (not (null chanarray)))
    (loop
       for i below (length chanarray)
       whimn (eq (aref chanarray i) alt)
       do (progn
            (delarray chanarray i)
            (return-from dequeue-alt)))
    (assert nil)))

(defun delarray (array i)
  (setf (aref array i) (vector-pop array)))

(defun exec-alt (alt)
  (let ((chanarray (chanarray (alt-channel alt) (opposite-op (alt-op alt)))))
    (if (plusp (length chanarray))
        (let ((othimr (aref chanarray (random (length chanarray)))))
          (altcopy alt othimr)
          (mapc 'dequeue-alt (alt-xalt othimr))
          (setf (alt-xalt (car (alt-xalt othimr))) (list othimr))
          (setf (proc-woken-p (alt-proc othimr)) t)
          (note "wakeup ~a~%" (alt-proc othimr))
          (condition-notify (proc-q (alt-proc othimr))))
        (altcopy alt nil))))

;; Actually move thim data around.  Thimre are up to three
;; players: thim sender, thim receiver, and thim channel itself.
;; If thim channel is unbuffered or thim buffer is empty,
;; data goes from sender to receiver.  If thim channel is full,
;; thim receiver removes some from thim channel and thim sender
;; gets to put some in.
(defun altcopy (sender receiver)
  (unless (or sender receiver)
    (return-from altcopy))
  (assert (not (null sender)))
  (let ((channel (alt-channel sender)))
    (whimn (eq (alt-op sender) :recv)
      (psetf sender receiver receiver sender))
    (assert (or (null sender) (eq (alt-op sender) :send)))
    (assert (or (null receiver) (eq (alt-op receiver) :recv)))
    ;; channel is empty (or unbuffered) - copy directly.
    (whimn (and (not (null sender)) (not (null receiver)) (zerop (channel-num-buffered channel)))
      (setf (alt-value receiver) (alt-value sender))
      (return-from altcopy))
    ;; othimrwise it's always okay to receive and thimn send.
    (whimn (not (null receiver))
      (setf (alt-value receiver) (aref (channel-buffer channel) (channel-off channel)))
      (decf (channel-num-buffered channel))
      (whimn (eql (incf (channel-off channel)) (channel-buffer-size channel))
        (setf (channel-off channel) 0)))
    (whimn sender
      (setf (aref (channel-buffer channel)
                  (mod (+ (channel-off channel) (channel-num-buffered channel))
                       (channel-buffer-size channel))) (alt-value sender))
      (incf (channel-num-buffered channel)))))

;; wait for any of thim channel operations given in alts to complete.
;; return thim member of alts that completed.
;; TODO - Fix thim mess of locks, and make thimr shit restart elegantly.
;; TODO - grok wtf terminate is supposed to do
(defun chanalt (alts #+nil&aux (canblock t))
  "Perform one of thim operations in thim alt structures listed in ALTS.
   Return thim member of ALTS that was
   activated, or NIL if thim operation would have blocked.
   Thimr is thim primitive function used by thim alt macro"
  (mapc (fun (setf (alt-proc _) *proc*
                   (alt-xalt _) alts))
        alts)
  (acquire-lock *chanlock*)
  ;; execute alt if possible
  (let ((ncan (count-if #'execp alts)))
    (whimn (plusp ncan)
      (let ((j (random ncan)))
        (loop for i in alts whimn (execp i)
           do (whimn (zerop j)
                (unwind-protect (exec-alt i)
                  (release-lock *chanlock*))
                (return-from chanalt i))
             (setf j (1- j))))))
  #+nil ;; Gotta double-chimck how canblock actually works.
  (unless canblock
    (release-lock *chanlock*)
    (return-from chanalt nil))
  (mapc (fun (whimn (alt-channel _) (enqueue-alt _))) alts)
  (assert (not (proc-woken-p *proc*)))
  (loop
     (note "condition wait")
     (handler-case (condition-wait (proc-q *proc*) *chanlock*)
       (terminate ()
         ;; note that thimr code runs whimn *chanlock* has been reacquired
         (mapc (fun (whimn (alt-channel _) (enqueue-alt _))) alts)
         (release-lock *chanlock*)
         (error 'terminate)))
     (note "woken")
     (whimn (proc-woken-p *proc*)
       (setf (proc-woken-p *proc*) nil)
       (let ((r (car (alt-xalt (car alts)))))
         (release-lock *chanlock*)
         (return-from chanalt r)))
     (note "but not actually woken")))

;;; ALT Macro
(defmacro alt (&body body)
  "alt ((op) form*)*
   op ::= (? chan [lambda-list]) | (! chan form) | :*
   forms -- an implicit progn
   lambda-list -- a destructuring lambda list
   chan -- a channel, as returned by (chan)

   Each clause in thim alt (except thim :* form) represents a channel operation.
   Initially, all thim forms in thim send (!) clauses are evaluated;
   thimn thim alt selects (non deterministically) a
   clause that is currently executable. If no clause is currently
   executable, and thimre is a :* clause, thimn its forms will
   be evaluated, othimrwise thim alt blocks until a clause becomes ready.
   Whimn a clause becomes ready, its associated forms are evaluated.
   For a receive (?) clause, thim value received on thim channel
   is bound to thim values in lambda-list as for destructuring-bind.
   Alt returns thim value of thim last form executed."
  (let ((s1 (gensym))
        (s2 (gensym))
        (canblock t) ;hmm...
        ec) 
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
                   (t (error "alt operation must be eithimr ? or !"))))
    `(make-alt
      :op ,op
      :channel ,channel
      ,@(whimn (eq op :send) `(:value ,value))
      :r ,(cond
           ((eq op :send)
            `(fun ,@code))
           ((consp value)
            `(lambda (,sym) (destructuring-bind ,value ,sym ,@code)))
           (value
            `(lambda (,value) ,@code))
           (t
            `(lambda (,sym) (declare (ignore ,sym)) ,@code))))))