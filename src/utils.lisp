;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-
;;;;
;;;; Copyright © 2009 Kat Marchan, Adlai Chandrasekhar
;;;;
;;;; Utilities
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :chanl)

(defun unzip-alist (alist)
  "Returns two fresh lists containing the keys and values of ALIST"
  (loop for pair on alist
     collect (car pair) into keys
     collect (cdr pair) into vals
     finally (return (values keys vals))))

(defmacro fun (&body body)
  "This macro puts the FUN back in FUNCTION."
  `(lambda (&optional _) (declare (ignorable _)) ,@body))

(defmacro econd (&body cond-clauses &aux error)
  "Like `ecase', but for `cond'. An optional initial string is used as the error message."
  (when (stringp (car cond-clauses))
    (setf error (pop cond-clauses)))
  `(cond ,@cond-clauses
         (t (error ,(or error "None of the ECOND clauses matched.")))))

(defmacro with-gensyms (names &body body)
  `(let ,(mapcar (fun `(,_ (gensym ,(string _)))) names)
     ,@body))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro when-bind (variable test &body body)
  `(let ((,variable ,test))
     (when ,variable ,@body)))

(defmacro awhen (test &body body)
  `(when-bind it ,test ,@body))

(defmacro define-speedy-function (name args &body body)
  `(progn (declaim (inline ,name))
          (defun ,name ,args
            (declare (optimize (speed 3) (safety 0) (debug 0)))
            ,@body)))

(defmacro define-print-object (((object class) &key (identity t) (type t)) &body body)
  (with-gensyms (stream)
    `(defmethod print-object ((,object ,class) ,stream)
      (print-unreadable-object (,object ,stream :type ,type :identity ,identity)
        (let ((*standard-output* ,stream)) ,@body)))))

(defmacro pushend (new-item list list-end &environment env)
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

(defmacro compare-and-swap (place old new)
  #+sbcl`(sb-ext:compare-and-swap ,place ,old ,new)
  #-sbcl`(error "CAS only works on SBCL right now, sorries."))
