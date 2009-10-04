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
   ;;; Channels
   ;; classes
   #:abstract-channel #:channel #:buffered-channel
   #:stack-channel #:queue-channel #:bounded-channel
   #:unbounded-channel
   ;; generic funs
   #:channelp #:send #:recv
   ;; channel extension genfuns
   #:send-blocks-p #:recv-blocks-p
   #:channel-insert-value #:channel-grab-value
   ;; other stuff
   #:+maximum-buffer-size+
   ;; demuxing
   #:select))
