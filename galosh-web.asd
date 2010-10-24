(asdf:defsystem #:galosh-web
    :depends-on (#:galosh-lisp
		 #:getopt
		 #:clsql
		 #:cl-who
		 #:hunchentoot)
    :components ((:file "qso")
		 (:file "galosh-web"
			:depends-on ("qso"))))
