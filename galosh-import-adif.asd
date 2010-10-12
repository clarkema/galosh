(asdf:defsystem #:galosh-import-adif
    :depends-on (#:galosh-lisp
		 #:galosh-config
		 #:clsql
		 #:getopt)
    :components ((:file "galosh-utils")
		 (:file "qso")
		 (:file "galosh-adif"
			:depends-on ("qso" "galosh-utils"))
		 (:file "galosh-import-adif"
			:depends-on ("galosh-adif"))))
