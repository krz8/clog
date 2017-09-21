(defsystem "clog"
    :description "Exploring the creation of a static site generator."
    :version "0.0.1"
    :author "Bob Krzaczek <Robert.Krzaczek@gmail.com>"

    :depends-on ("iterate" "cl-fad" "cl-date-time-parser")
    :components ((:file "pkg")
		 (:file "walk" :depends-on ("pkg"))
		 (:file "main" :depends-on ("pkg"))))
