(asdf:defsystem #:galosh-lisp
    :depends-on (#:clsql
		 #:split-sequence
		 #:py-configparser
		 #:alexandria
		 #:cl-ppcre)
    :components ((:file "galosh-lisp")))
