(asdf:defsystem #:galosh-qrz
    :depends-on (#:galosh-lisp
		 #:galosh-qrzcom
		 #:galosh-grep
		 #:getopt
		 #:clsql)
    :components ((:file "galosh-qrz")))
