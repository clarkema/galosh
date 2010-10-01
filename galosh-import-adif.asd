(asdf:defsystem #:galosh-import-adif
    :depends-on (#:clsql
		 #:split-sequence
		 #:getopt)
    :components ((:file "galosh-lisp")
		 (:file "galosh-utils")
		 (:file "qso"
			:depends-on ("galosh-lisp"))
		 (:file "galosh-adif"
			:depends-on ("qso" "galosh-lisp" "galosh-utils"))
		 (:file "galosh-import-adif"
			:depends-on ("qso" "galosh-adif"))))
