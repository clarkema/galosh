(asdf:defsystem #:galosh-grep
    :depends-on (#:galosh-lisp
		 #:galosh-qso
		 #:clsql)
    :components ((:file "galosh-grep")))
