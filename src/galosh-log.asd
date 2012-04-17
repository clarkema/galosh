(asdf:defsystem #:galosh-log
    :depends-on (#:galosh-lisp
		 #:galosh-qso
		 #:galosh-qrz
		 #:getopt
		 #:clsql
		 #:bordeaux-threads
		 #:drakma
		 #:cl-ncurses
		 #:alexandria)
    :components ((:file "galosh-ncurses")
		 (:file "galosh-adif")
		 (:file "galosh-log"
			:depends-on ("galosh-ncurses" "galosh-adif"))))