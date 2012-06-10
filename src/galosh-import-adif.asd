(asdf:defsystem #:galosh-import-adif
    :depends-on (#:galosh-lisp
		 #:galosh-qso
		 #:clsql
		 #:getopt)
    :components ((:file "galosh-adif")
		 (:file "galosh-import-adif"
			:depends-on ("galosh-adif"))))
