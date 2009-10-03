;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan, Adlai Chandrasekhar
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
   #:channel #:send #:recv
   #:buffered-channel #:+maximum-buffer-size
   #:send-blocks-p #:recv-blocks-p
   ;; selecting!
   #:select))
