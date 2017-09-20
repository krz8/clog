(in-package #:clog)

(defun main ()
  (format t "hello, world~%"))

(defvar *appname*
  #+windows "clog.exe"
  #- (or windows) "clog"
  "The default name of the executable we build, with a proper
extension for certain operating systems.")

(defun build (&optional (app *appname*))
  "Deliver a compiled application APP.  APP takes its default value
from the special *APPNAME*, so you can use a different name either by
calling BUILD with an explicit name or by rebinding APP."
  #+ccl
  (ccl::save-application app :toplevel-function #'main :prepend-kernel t)
  #+sbcl
  (sb-ext:save-lisp-and-die app :toplevel #'main :executable t)
  #- (or ccl sbcl)
  (error "It appears this version of clog isn't yet ported to this Lisp."))
