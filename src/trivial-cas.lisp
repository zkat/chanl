;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Adlai Chandrasekhar
;;;;
;;;; Trivial Compare-and-Swap
;;;;
;;;; Sketches of a Concurrency Primitive
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :trivial-compare-and-swap
  (:use :cl)
  (:nicknames :trivial-cas)
  #+ (and sbcl compare-and-swap-vops)
  (:import-from :sb-ext :compare-and-swap)
  #+ ccl
  (:import-from :ccl :defx86lapfunction)
  (:export #:compare-and-swap))

(in-package :trivial-cas)

#+ (and sbcl (not compare-and-swap-vops))
(defmacro compare-and-swap (place old new)
  (warn "COMPARE-AND-SWAP is not implemented atomically on this platform.")
  `(sb-ext:compare-and-swap ,place ,old ,new))

#+ (and ccl x86-64)
(progn
  (defmacro compare-and-swap (place old new)
    (if (atom place)
        (error 'type-error :datum place :expected-type 'cons)
        (ecase (car place)
          (car `(%compare-and-swap-car ,place ,old ,new))
          (cdr `(%compare-and-swap-cdr ,place ,old ,new)))))

  (defx86lapfunction %compare-and-swap-cdr ((cons arg_x) (old arg_y) (new arg_z))
    (movq (% old) (% imm0))
    (cmpxchgq (% new) (@ -3 (% cons)))
    (jne @fail)
    (movq (% old) (% arg_z))
    (single-value-return)
    @fail
    (movq (@ -3 (% cons)) (% arg_z))
    (single-value-return))

  (defx86lapfunction %compare-and-swap-car ((cons arg_x) (old arg_y) (new arg_z))
    (movq (% old) (% imm0))
    (cmpxchgq (% new) (@ 5 (% cons)))
    (jne @fail)
    (movq (% old) (% arg_z))
    (single-value-return)
    @fail
    (movq (@ 5 (% cons)) (% arg_z))
    (single-value-return)))