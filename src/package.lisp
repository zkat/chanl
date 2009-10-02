;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Josh Marchan, Adlai Chandrasekhar
;;;;
;;;; Package definition
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :chanl
  (:use :common-lisp)
  (:import-from :bordeaux-threads :*default-special-bindings*)
  (:export
   ;; threads
   #:current-thread #:thread-alive-p #:threadp
   #:thread-name #:kill #:all-threads #:pooled-threads
   #:pcall #:pexec #:*default-special-bindings*
   ;; channels
   #:make-channel #:send #:recv #:+maximum-buffer-size
   #:channel #:send-blocks-p #:recv-blocks-p
   ;; selecting!
   #:select))
