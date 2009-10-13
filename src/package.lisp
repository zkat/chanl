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
  (:import-from :trivial-cas :compare-and-swap)
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
   ;; other stuff
   #:+maximum-buffer-size+
   ;; demuxing
   #:select))
