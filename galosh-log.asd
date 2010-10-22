(asdf:defsystem #:galosh-log
    :depends-on (#:galosh-lisp
		 #:clsql
		 #:cl-ncurses)
    :components ((:file "qso")
		 (:file "galosh-utils")
		 (:file "galosh-ncurses")
		 (:file "galosh-log"
			:depends-on ("qso" "galosh-utils" "galosh-ncurses"))))