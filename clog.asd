(defsystem "clog"
    :description "Exploring the creation of a static site generator."
    :version "0.0.1"
    :author "Bob Krzaczek <Robert.Krzaczek@gmail.com>"

    :depends-on ("iterate" "cl-fad" "cl-date-time-parser")
    :components ((:file "pkg")
		 (:file "util" :depends-on ("pkg"))
		 (:file "walk" :depends-on ("util"))
		 (:file "main" :depends-on ("util"))))
