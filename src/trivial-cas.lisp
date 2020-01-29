;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Adlai Chandrasekhar
;;;;
;;;; Trivial Compare-and-Swap
;;;;
;;;; Sketches of a Concurrency Primitive
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+ sbcl
(eval-when (:load-toplevel :compile-toplevel :execute)
  (let ((package (find-package :sb-impl)))
    (when package
      (let ((sym (find-symbol (string :+internal-features+) package)))
        (when (and (boundp sym) (member :compare-and-swap-vops (symbol-value sym)))
          (pushnew :compare-and-swap-vops *features*))))))

(defpackage :trivial-compare-and-swap
  (:use :cl)
  (:nicknames :trivial-cas)
  #+ (and sbcl compare-and-swap-vops)
  (:import-from :sb-ext :compare-and-swap)
  #+ (and ccl (or x86 x86-64))
  (:import-from :ccl :defx86lapfunction)
  (:export #:compare-and-swap
           #:atomic-incf))

(in-package :trivial-cas)

(defmacro atomic-incf (place &optional (delta 1))
  (let ((old (gensym)) (new (gensym)))
    `(loop for ,old = ,place for ,new = (+ ,delta ,old)
        when (eq ,old (compare-and-swap ,place ,old ,new))
        return ,new)))

#- (or sbcl ccl)
(defmacro compare-and-swap (place old new)
  (declare (ignore place old new))
  `(error "Not supported yet."))

#+ (and sbcl (not compare-and-swap-vops))
(defmacro compare-and-swap (place old new)
  (warn "COMPARE-AND-SWAP is not implemented atomically on this platform.")
  `(sb-ext:compare-and-swap ,place ,old ,new))

#+ (and ccl ccl-1.10)
(progn
  (warn "CCL's conditional-store operations are not exported. Caveat usor.")
  (defmacro compare-and-swap (place old new)
    (if (atom place)
        (error 'type-error :datum place :expected-type 'cons)
        (ecase (car place)    ; TODO: upstream these into ccl::conditional-store
          (car `(ccl::%rplaca-conditional ,(cadr place) ,old ,new))
          (cdr `(ccl::%rplacd-conditional ,(cadr place) ,old ,new))
          (svref `(ccl::conditional-store ,place ,old ,new))))))
