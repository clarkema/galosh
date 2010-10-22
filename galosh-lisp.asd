(asdf:defsystem #:galosh-lisp
    :depends-on (#:clsql
		 #:split-sequence
		 #:py-configparser)
    :components ((:file "galosh-lisp")))
