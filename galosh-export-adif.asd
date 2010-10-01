(asdf:defsystem #:galosh-export-adif
    :depends-on (#:clsql
		 #:split-sequence)
    :components ((:file "galosh-lisp")
		 (:file "qso"
			:depends-on ("galosh-lisp"))
         (:file "galosh-utils")
		 (:file "galosh-adif"
			:depends-on ("qso" "galosh-lisp" "galosh-utils"))
		 (:file "galosh-export-adif"
			:depends-on ("qso" "galosh-adif"))))
