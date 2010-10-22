(asdf:defsystem #:galosh-grep
    :depends-on (#:galosh-lisp
		 #:clsql)
    :components ((:file "qso")
		 (:file "galosh-grep"
			:depends-on ("qso"))))