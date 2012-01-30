(asdf:defsystem #:galosh-audit
    :depends-on (#:galosh-lisp
		 #:galosh-qso
		 #:getopt
		 #:clsql
		 #:alexandria)
    :components ((:file "galosh-audit")))
