(asdf:defsystem #:galosh-lisp
    :depends-on (#:clsql
		 #:split-sequence
		 #:py-configparser
		 #:alexandria)
    :components ((:file "galosh-lisp")))
