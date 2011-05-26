(asdf:defsystem #:galosh-qsl
    :depends-on (#:galosh-lisp
		 #:galosh-qso
		 #:galosh-qrz
		 #:clsql
		 #:cl-ncurses
		 #:alexandria
		 #:screamer)
    :components ((:file "galosh-ncurses")
		 (:file "galosh-qsl"
			:depends-on ("galosh-ncurses"))))