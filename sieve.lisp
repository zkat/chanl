;; The infamous parallel prime sieve from newsqueak

(in-package :chanl)

(defun counter (channel)
  (loop for i from 2 do (send channel i) (format t "COUNTER: Sent out ~A~%" i)))

(defun filter (prime in out)
  (format t "FILTER: filtering ~A using numbers from ~A, into ~A~%" prime in out)
  (loop for i = (recv in)
     do (format t "FILTER: Received ~A~%" i)
     when (plusp (mod i prime))
     do (format t "FILTER: Attempting to send out ~A~%" i)
       (send out i)
       (format t "FILTER: Sent out ~A~%" i)))

(defun sieve ()
  (let* ((c (chan))
         (prime-chan (chan)))
    (spawn (counter c))
    (spawn (loop
              (format t "SIEVE: Trying to read from ~A...~%" c)
              (let* ((prime (recv c))
                     (newc (chan)))
                (format t "SIEVE: Received ~A from ~A~%" prime c)
                (format t "SIEVE: Sending ~A down prime-chan.~%" prime)
                (send prime-chan prime)
                (format t "SIEVE: Sent ~A down prime-chan~%" prime)
                (spawn (filter prime c newc))
                (format t "SIEVE: Spawned new FILTER process, going from ~A to ~A~%" c newc)
                (setf c newc))))
    prime-chan))

(defun first-n-primes (n)
  (let* ((procs (all-procs))
         (prime-chan (sieve)))
    (unwind-protect (loop repeat n collect (recv prime-chan))
      (map nil 'kill (set-difference (all-procs) procs)))))
