(asdf:defsystem #:galosh-export-adif
    :depends-on (#:galosh-lisp
		 #:getopt
		 #:clsql
		 #:galosh-qso)
    :components ((:file "galosh-adif")
		 (:file "galosh-export-adif"
			:depends-on ("galosh-adif"))))
