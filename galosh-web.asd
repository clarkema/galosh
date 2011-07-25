(asdf:defsystem #:galosh-web
    :depends-on (#:galosh-lisp
		 #:galosh-qrzcom
		 #:getopt
		 #:clsql
		 #:cl-markup
		 #:hunchentoot
		 #:parenscript
		 #:alexandria)
    :components ((:file "qso")
		 (:file "galosh-web"
			:depends-on ("qso"))))
