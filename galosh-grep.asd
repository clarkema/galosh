(asdf:defsystem #:galosh-grep
    :depends-on (#:galosh-lisp
		 #:galosh-config
		 #:clsql)
    :components ((:file "qso")
		 (:file "galosh-grep"
			:depends-on ("qso"))))