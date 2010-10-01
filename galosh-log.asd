(asdf:defsystem #:galosh-log
    :depends-on (#:clsql
		 #:cl-ncurses
		 #:split-sequence)
    :components ((:file "galosh-lisp")
		 (:file "qso"
			:depends-on ("galosh-lisp"))
		 (:file "galosh-utils")
		 (:file "galosh-ncurses"
			:depends-on ("galosh-lisp"))
		 (:file "galosh-log"
			:depends-on ("qso" "galosh-lisp" "galosh-utils" "galosh-ncurses"))))