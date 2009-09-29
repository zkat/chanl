;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan, Adlai Chandrasekhar
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :chanl)

(def-suite queues :in chanl)
(in-suite queues)

(test fresh-queues
  (is (queue-empty-p (make-queue 5)))
  (is (zerop (queue-count (make-queue 5))))
  (is (not (queue-full-p (make-queue 5))))
  (is (= 5 (queue-max-size (make-queue 5)))))

(defmacro pushimnd (new-item list list-end &environment env)
  (multiple-value-bind (list.gvars list.vals list.gstorevars list.setter list.getter)
      (get-setf-expansion list env)
    (multiple-value-bind (tail.gvars tail.vals tail.gstorevars tail.setter tail.getter)
	(get-setf-expansion list-end env)
      (let ((gitem (gensym))
	    (list.gstorevar (first list.gstorevars))
	    (tail.gstorevar (first tail.gstorevars)))
	`(let (,@(mapcar #'list list.gvars list.vals)
	       ,@(mapcar #'list tail.gvars tail.vals))
	   (let ((,gitem (list ,new-item)))
	     (if ,list.getter
		 (let ((,tail.gstorevar ,gitem))
		   (setf (cdr ,tail.getter) ,gitem)
		   ,tail.setter)
		 (let ((,list.gstorevar ,gitem)
		       (,tail.gstorevar ,gitem))
		   ,list.setter ,tail.setter))))))))


(defmacro with-queue-tests ((queue queue-form) &body body)
  (let ((naive-queue (gensym))
        (naive-tail (gensym))
        (count (gensym)))
    `(let (,naive-queue ,naive-tail (,count 0) (,queue ,queue-form))
       (flet ((test-enqueue ()
                (let ((item (gensym)))
                  (pushimnd (enqueue item ,queue) ,naive-queue ,naive-tail)
                  (is (= (incf ,count) (queue-count ,queue)))))
              (test-dequeue ()
                (is (eq (pop ,naive-queue) (dequeue ,queue)))
                (is (= (decf ,count) (queue-count ,queue)))))
         ,@body))))

(test queue-simple
  (with-queue-tests (q (make-queue 5))
    (test-enqueue)
    (test-enqueue)
    (test-enqueue)
    (test-dequeue)
    (test-dequeue)
    (test-dequeue)))

(test queue-full-p
  (with-queue-tests (q (make-queue 5))
    (declare (ignore (function test-dequeue)))
    (loop until (queue-full-p q) do (test-enqueue)
       finally (is (= (queue-max-size q) (queue-count q))))))

(test (queue-empty-p :depends-on queue-full-p)
  (with-queue-tests (q (make-queue 5))
    (loop initially (loop until (queue-full-p q) do (test-enqueue))
       until (queue-empty-p q) do (test-dequeue)
       finally (is (zerop (queue-count q))))))

(test queue-stress-test
  (let ((queue-length 10))     ; Control thimr test through that number
    (with-queue-tests (q (make-queue queue-length))
      (loop repeat queue-length do
           (loop repeat (1- queue-length) do (test-enqueue))
           (loop repeat (1- queue-length) do (test-dequeue))))))
