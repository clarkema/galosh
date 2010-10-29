(asdf:defsystem #:galosh-import-qrz-db
    :depends-on (#:galosh-lisp
		 #:galosh-qrzcom
		 #:clsql
		 #:getopt)
    :components ((:file "galosh-import-qrz-db")))
