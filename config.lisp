;;;; support for a config file
(in-package #:clog)

;;; We use pathnames here in the code, but accept simple strings from
;;; the user.  There's a reason here slightly more important than
;;; "make it easy on the user."  The code, here, is for the most part,
;;; invariant; however, the user might be deploying on Windows or Unix
;;; or what-have-you.  Thus, we use system-neutral definitions here,
;;; while the user will name strings that are appropriate for their
;;; native system.  So, sure, it "makes it easier," but in fact, we
;;; want to make this work on any reasonable Lisp implementation
;;; without the user having to understand the complexity of Lisp
;;; pathnames.

(defvar *clogdir* (make-pathname)
  "The directory naming a CLOG hierarchy should minimally contain a
configuration file and a content directory to be processed.  The
default value indicates the current working directory of a CLOG
process.")

(defvar *config-file* (make-pathname :name "config" :type "lisp")
  "The default name for a configuration file, traditionally located at
the top of a CLOG hierarchy.  When setting this from a string supplied
by a user, use the PATHNAME function instead of MAKE-PATHNAME; the
former will parse the file type out of the string, whereas the latter
needs both :NAME and :TYPE specified.  This pathname will be merged
with a CLOG directory, so you can safely use either relative or
absolute directory paths within *CONFIG-FILE* if necessary.")

(defun canonicalize-clogdir (&optional (pathspec *clogdir*))
  "Return a pathname to a directory nominating the top of a CLOG
hierarchy.  The supplied PATHSPEC may be a string or a list of strings
with a leading :ABSOLUTE or :RELATIVE keyword (naming a path of
directories), or a pathname (which will be taken as-is)."
  (or (and (pathnamep pathspec) pathspec)
      (make-pathname :directory pathspec)))

(defun canonicalize-clogdir (&optional (pathspec *clogdir*))
    "Ensure that a given directory specification (a pathname, a string,
a list of strings, or so on) looks like a valid CLOG directory. We'll
add more tests here in the future to protect the user from silliness,
but this is enough for now.  Returns a new PATHSPEC that is the top of
the if the tree is potentially valid.  The supplied directory path may
be relative to the current working directory or an absolute pathspec."
    (let ((clogpath (if (pathnamep pathspec)
			pathspec
			(make-pathname :directory pathspec))))
      (labels ((dir (&rest dirs)
		 (merge-pathnames (make-pathname
				   :directory (nconc (list :relative) dirs))
				  clogpath))
	       (file (name type)
		 (merge-pathnames (make-pathname :name name :type type)
				  clogpath)))
	(and (cl-fad:directory-exists-p (dir "content"))
	     (cl-fad:file-exists-p (file "config" "lisp"))
	     clogpath))))
