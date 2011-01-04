(asdf:defsystem #:galosh-export-adif
    :depends-on (#:galosh-lisp
		 #:getopt
		 #:clsql)
    :components ((:file "qso")
		 (:file "galosh-utils")
		 (:file "galosh-adif"
			:depends-on ("qso" "galosh-utils"))
		 (:file "galosh-export-adif"
			:depends-on ("galosh-adif"))))
