(asdf:defsystem #:galosh-log
    :depends-on (#:galosh-lisp
		 #:galosh-qso
		 #:galosh-qrz
		 #:clsql
		 #:cl-ncurses
		 #:alexandria)
    :components ((:file "galosh-utils")
		 (:file "galosh-ncurses")
		 (:file "galosh-adif")
		 (:file "galosh-log"
			:depends-on ("galosh-utils" "galosh-ncurses" "galosh-adif"))))