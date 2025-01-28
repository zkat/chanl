;;; HAVE YOU EVER USED IT IN ANGER

(macrolet ((with-supprogn ((&optional (arity -1/2)) &body forms)
             `(progn (let ((delay ,arity))
                       ,@(loop for form in forms append
                               `((sleep (incf delay)) (time ,form)))))))
  (with-supprogn ()
    (mapc 'kill (remove "ChanL" (all-threads) :test-not 'search :key 'thread-name))
    (mapc 'kill (mapcar 'task-thread (pooled-tasks)))
    (mapc 'kill (pooled-tasks))
    (setf ;(slot-reduce *charioteer* gate cache) ()
          (slot-value chanl::*thread-pool* 'chanl::tasks) ()
          (slot-value chanl::*thread-pool* 'chanl::threads) ()
          (slot-value chanl::*thread-pool* 'chanl::pending-tasks) ()
          (slot-value chanl::*thread-pool* 'chanl::free-thread-counter) 0)))
