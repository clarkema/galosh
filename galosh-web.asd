(asdf:defsystem #:galosh-web
    :depends-on (#:galosh-lisp
		 #:galosh-config
		 #:getopt
		 #:clsql
		 #:cl-who
		 #:hunchentoot
		 #:split-sequence)
    :components ((:file "qso")
		 (:file "galosh-web"
			:depends-on ("qso"))))
