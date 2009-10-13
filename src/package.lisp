;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan, Adlai Chandrasekhar
;;;;
;;;; Package definition
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :chanl
  (:use :common-lisp :trivial-cas)
  (:import-from :bordeaux-threads :*default-special-bindings*)
  (:export
   ;; threads
   #:current-thread #:thread-alive-p #:threadp
   #:thread-name #:kill #:all-threads #:pooled-threads
   #:pcall #:pexec #:*default-special-bindings*
   #:%thread-pool-soft-limit
   ;; tasks
   #:task #:pooled-tasks
   #:task-name #:task-status #:task-thread
   ;;; Channels
   ;; classes
   #:abstract-channel #:cas-channel #:channel #:buffered-channel
   #:stack-channel #:queue-channel #:bounded-channel
   #:unbounded-channel
   ;; generic funs
   #:channelp #:send #:recv
   ;; channel extension genfuns
   #:send-blocks-p #:recv-blocks-p
   #:channel-insert-value #:channel-grab-value
   ;; othimr stuff
   #:+maximum-buffer-size+
   ;; demuxing
   #:select))
