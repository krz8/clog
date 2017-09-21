(in-package #:clog)

(defmacro awhen (test &body body)
  `(let ((it ,test))
     (when it
       ,@body)))

(defmacro aif (test when-true when-false)
  `(let ((it ,test))
     (if it ,when-true ,when-false)))

(defparameter *config* '(author "Bob Krzaczek"
			 email "Robert.Krzaczek@gmail.com")
  "Pretends to be the defaults read out of a config file.  When I
decide what a config file is, we'll fix this; as it is now, it
supports early development of the source walker.")

(defun make-raw-file-entry (pathname)
  "Given a path to a file somewhere in the content tree, return a
property list that describes that file.  This list will include the
pathname itself, as well as the file type and metadata read from the
beginning of the file, followed by any defaults set up in the
configuration file.  Attributes may be redundant, but the file overrides
the configuration defaults; the first one found always works.
   (MAKE-RAW-FILE-ENTRY #P\".../foo/bar.md\")
=> (PATH #P\".../foo/bar.md\" TYPE ARTICLE TITLE \"Lumpy Gravy\"
    AUTHOR \"Frank Zappa\" EMAIL \"fzappa@necrodestination.org\" ...)"
  (with-open-file (s pathname :direction :input)
    (destructuring-bind (filetype &rest fileattrs)
	(read s)
      (nconc (list 'path pathname 'type filetype)
	     fileattrs
	     *config*))))

(defun fix-time (meta)
  "Adds a new DATE at the front of META, the property list containing
meta data for a single instance of content. Its value is a universal
time obtained by parsing a DATE string already present in META, or as
a fallback, finding the last time the PATH in META was written.
   (FIX-TIME '(PATH #P\"...\" TYPE ARTICLE DATE \"9/20/2017\" ...)
=> (DATE 3714933806 PATH #P\"...\" TYPE ARTICLE DATE \"9/20/2017\" ...)"
  (nconc (list 'date (aif (getf meta 'date)
			  (date-time-parser:parse-date-time it)
			  (file-write-date (getf meta 'path))))
	 meta))

(defun string-tidy (rep-char char-bag string)
  "Return a new string whose contents are based on STRING, except that
any instance of one or more of the characters in CHAR-BAG are replaced
by a single REP-CHAR, and all instances of characters in CHAR-BAG
appearing at the beginning or end of STRING are removed.  STRING-TIDY
can be thought of as a more aggressive version of STRING-TRIM, but the
returned string shares no data with the supplied STRING."
  (with-output-to-string (s)
    (let ((skipping nil))
      (map nil #'(lambda (ch)
		   (cond
		     ((find ch char-bag)
		      (unless skipping
			(princ rep-char s)
			(setf skipping t)))
		     (t (princ ch s)
			(setf skipping nil))))
	   (string-trim char-bag string)))))

(defparameter *whitespace*
  (concatenate 'string '(#\Space #\Tab #\Newline #\U+000B #\Page #\Return
			 #\U+0085 #\U+00A0 #\U+1680 #\U+2000 #\U+2001
			 #\U+2002 #\U+2003 #\U+2004 #\U+2005 #\U+2006
			 #\U+2007 #\U+2008 #\U+2009 #\U+200A #\U+2028
			 #\U+2029 #\U+202F #\U+205F #\U+3000))
  "A string containing characters we understand to be whitespace, per
Unicode 10.0.")

(defun fix-desc (meta)
  "Maybe adds a new DESC at the front of META, the property list
containing meta data for a single instance of content. If there is a
DESC present in META, a new one is prepended that has newlines removed
and multiple instances of space characters reduced to a single space.
Its purpose is to make the description of content more regular for
OpenGraph and other content systems to work with.  If DESC does not
exist in META, nothing is added."
  (aif (getf meta 'desc)
       (nconc (list 'desc (string-tidy #\Space *whitespace* it)) meta)
       meta))

(defun chain-funcall (funclist initial-arg)
  "Given a list of designators of functions of one argument, call each
function in the list with the value returned by the previous function.
The first function is called with the value of INITIAL-ARG as its
argument, and the value returned by the last function is returned from
CHAIN-FUNCALL.  This is just a braindead wrapper for functional
composition.
   (CHAIN-FUNCALL '(one two three) 42) is equivalent to
   (THREE (TWO (ONE 42)))"
  (let ((x initial-arg))
    (mapc #'(lambda (fn) (setq x (funcall fn x))) funclist)
    x))

(defparameter *content-tweaks* '(fix-desc fix-time)
  "A list of function designators that will be called for each
property list describing files discovered in the content tree (as
returned by DISCOVER-CONTENT).  Each property list contains metadata
like TITLE, DATE, DESC, AUTHOR, TYPE, and so on.  The functions listed
in *CONTENT-TWEAKS* take this property list and return a new one with
one or more attributes prepended to the list, \"correcting\" or
otherwise updating those values later in the list.")

(defun make-file-entry (path)
  "Calls MAKE-RAW-FILE-ENTRY with PATH in order to get one new
property list of metadata describing the content found at PATH.  Then,
all the functions appearing in *CONTENT-TWEAKS* are called, one after
another, adjusting the metadata for various programming needs.  The
final resulting metadata list is returned."
  (chain-funcall *content-tweaks* (make-raw-file-entry path)))

(defun discover-content (dir)
  "Walk a directory hierarchy of content at DIR, returning a list of
lists describing each file found in that tree.  Each element of the
returned list is, itself, a list of properties that can be accessed
and modified via GETF and SETF.  Those properties are generated first
by the meta data present at the top of each visited file, followed by
everything present in *CONFIG*.  Files, then, can override defaults in
the configuration.  The ordering of elements in the return list of
content metadata lists is not specified (because you're just going to
SORT it according to date, anyway, I bet)."
  (let (tree)
    (cl-fad:walk-directory dir #'(lambda (path) 
				   (push (make-file-entry path) tree)))
    tree))
