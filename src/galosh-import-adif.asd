(asdf:defsystem #:galosh-import-adif
    :depends-on (#:galosh-lisp
		 #:galosh-qso
		 #:clsql
		 #:getopt
		 #:parse-number)
    :components ((:file "galosh-adif")
		 (:file "galosh-import-adif"
			:depends-on ("galosh-adif"))))
