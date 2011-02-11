(asdf:defsystem #:galosh-log
    :depends-on (#:galosh-lisp
		 #:galosh-qrz
		 #:clsql
		 #:cl-ncurses
		 #:alexandria)
    :components ((:file "qso")
		 (:file "galosh-utils")
		 (:file "galosh-ncurses")
		 (:file "galosh-adif")
		 (:file "galosh-log"
			:depends-on ("qso" "galosh-utils" "galosh-ncurses" "galosh-adif"))))