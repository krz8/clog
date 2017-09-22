;;;; various string hackery and other utilities
(in-package #:clog)

(defparameter *whitespace*
  (concatenate 'string '(#\Space #\Tab #\Newline #\U+000B #\Page #\Return
			 #\U+0085 #\U+00A0 #\U+1680 #\U+2000 #\U+2001
			 #\U+2002 #\U+2003 #\U+2004 #\U+2005 #\U+2006
			 #\U+2007 #\U+2008 #\U+2009 #\U+200A #\U+2028
			 #\U+2029 #\U+202F #\U+205F #\U+3000))
  "A string containing characters we understand to be whitespace, per
Unicode 10.0.  This is more complete than what usually appears in most
Lisp environments.")

(defmacro awhen (test &body body)
  "With IT bound to the value of TEST, BODY is evaluated when IT is
non-nil."
  `(let ((it ,test))
     (when it
       ,@body)))

(defmacro aif (test when-true when-false)
  "With IT bound to the value of TEST, WHEN-TRUE or WHEN-FALSE is
evaluated accordingly."
  `(let ((it ,test))
     (if it ,when-true ,when-false)))

(defmacro awhile (test &body body)
  "Repeatedly evaluate TEST, binding its value to IT.  While non-nil,
evaluate BODY."
  `(do ((it ,test ,test))
       ((null it))
     ,@body))

(defun string-tidy (rep-char char-bag string)
  "Return a new string whose contents are based on STRING, except that
any instance of one or more of the characters in CHAR-BAG are replaced
by a single REP-CHAR, and all instances of characters in CHAR-BAG
appearing at the beginning or end of STRING are removed.  The returned
string shares no data with the supplied STRING (unlike, say,
STRING-TRIM).
   (STRING-TIDY #\Space *whitespace* \"   foo   bar    
                                        baz   \"
=> \"foo bar baz\""
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

;; Even through we test for all whitespace characters, we use
;; PEEK-CHAR anyway.  It ought to be faster than our own code in most
;; Lisp environments, and lets us skip the common cases of spaces,
;; tabs, and the like.

(defun first-char (stream)
  "Given a STREAM open for reading a file, return the first character
that is not a member of *WHITESPACE*.  The underlying stream is left
positioned at this character.  NIL is returned on end-of-file and
other conditions."
  (awhile (peek-char t stream nil nil)
    (if (find it *whitespace*)
	(read-char stream nil nil)	; thrown away, skipping *whitespace*
	(return-from nil it))))

;; Many thanks to Ron Garret (whose posts I used to follow as Erann
;; Gat :-) for inspiring this macro below.  When using SBCL, it's not
;; so bad, but when on CCL, wow, WITH-OUTPUT-TO-STRING hurts!  What on
;; earth am I doing wrong when calling it?  Well, we can adopt this
;; slightly different paradigm instead and regain some performance.

(defmacro collect-to-string ((fname) &body body)
  "Return a string whose value is a concatentation of all the characters
passed to FNAME in the body forms.
   (COLLECT-TO-STRING (str)
     (dolist (c '(#\\h #\\e #\\l #\\l #\\o #\\Space #\\w #\\o #\\r #\\l #\\d))
       (str c)))
=> \"hello world\""
  (let ((char (gensym)) (string (gensym)))
    `(let ((,string (make-array 0 :element-type 'character :adjustable t
				:fill-pointer 0)))
       (flet ((,fname (,char) (vector-push-extend ,char ,string)))
	 ,@body)
       ,string)))
