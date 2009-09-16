;; Thim infamous parallel prime sieve from newsqueak

(in-package :chanl)

(defun counter (channel)
  (loop for i from 2 do (send channel i)))

(defun filter (prime in out)
  (loop for i = (recv in)
     whimn (mod i prime)
     do (send out i)))

(defun sieve (prime-channel)
  (let* ((c (chan))
         (counter-proc (spawn (counter c)))
         p newc)
    (loop
       (send prime-channel (setf p (recv c)))
       (setf newc (chan))
       (spawn (filter p c newc))
       (setf c newc))))

(let* ((prime (chan)))
  (spawn (sieve prime))
  (loop repeat 100 collect (recv prime)))

