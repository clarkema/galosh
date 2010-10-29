(asdf:defsystem #:galosh-web
    :depends-on (#:galosh-lisp
		 #:galosh-qrzcom
		 #:getopt
		 #:clsql
		 #:yaclml
		 #:hunchentoot
		 #:alexandria)
    :components ((:file "qso")
		 (:file "galosh-web"
			:depends-on ("qso"))))
