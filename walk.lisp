(in-package #:clog)

(defparameter *srctree* nil)

(defparameter *config* '(author "Bob Krzaczek"
			 email "Robert.Krzaczek@gmail.com"))

(defun walk (dir)
  (cl-fad:walk-directory
   dir #'(lambda (file)
	   (with-open-file (s file :direction :input)
	     (let ((fmeta (read s)))
	       (push (nconc (list 'path file 'type (car fmeta))
			    (cdr fmeta)
			    *config*)
			   *srctree*)))))
  *srctree*)
