(in-package #:clog)

(defun main ()
  (format t "hello, world~%"))

(defvar *appname*
  #+windows "clog.exe"
  #- (or windows) "clog"
  "The default name of the executable we build, with a proper
extension for certain operating systems.")

(defun build-and-exit (&optional (app *appname*))
  "Deliver a compiled application APP.  APP takes its default value
from *APPNAME*, so you can use a different name either by calling
BUILD-AND-EXIT with an explicit name or by rebinding the special.

So far, every Lisp engine I've worked with that delivers executables
will exit upon doing so, hence the name of this function.  If you're
adding support for something that doesn't exit, insert a PROGN form
below that ensures it does exit, to remain compatible with the
others."
  #+ccl
  (ccl:save-application app :toplevel-function #'main :prepend-kernel t)
  #+sbcl
  (sb-ext:save-lisp-and-die app :toplevel #'main :executable t)
  #- (or ccl sbcl)
  (error "It appears this version of clog isn't yet ported to this Lisp."))
