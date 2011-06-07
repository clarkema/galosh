(asdf:defsystem #:galosh-cluster
    :depends-on (#:bordeaux-threads
		 #:galosh-lisp
		 #:clsql
		 #:cl-ncurses
		 #:cl-ppcre
		 #:alexandria
		 #:usocket
		 #:jpl-queues)
    :components ((:file "galosh-utils")
		 (:file "galosh-ncurses")
		 (:file "galosh-cluster"
			:depends-on ("galosh-utils" "galosh-ncurses"))))