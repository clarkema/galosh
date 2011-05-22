(asdf:defsystem #:galosh-export-adif
    :depends-on (#:galosh-lisp
		 #:getopt
		 #:clsql
		 #:galosh-qso)
    :components ((:file "galosh-utils")
		 (:file "galosh-adif"
			:depends-on ("galosh-utils"))
		 (:file "galosh-export-adif"
			:depends-on ("galosh-adif"))))
