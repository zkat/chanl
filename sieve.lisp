;; The infamous parallel prime sieve from newsqueak

(in-package :chanl)

(defun counter (channel)
  (loop for i from 2 do (send channel i)))

(defun filter (prime in out)
  (loop for i = (recv in)
     when (mod i prime)
     do (send out i)))

(defun sieve ()
  (let* ((c (chan))
         (prime-chan (chan)))
    (spawn (counter c))
    (spawn (lambda ()
             (loop
                (let* ((prime (recv c))
                       (newc (chan)))
                  (send prime-chan prime)
                  (spawn (filter prime c newc))
                  (setf c newc)))))
    prime-chan))

(defun first-n-primes (n)
  (let* ((prime-chan (sieve)))
    (loop repeat n collect (recv prime-chan))))


