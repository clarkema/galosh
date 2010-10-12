(asdf:defsystem #:galosh-lisp
    :depends-on (#:clsql
		 #:split-sequence)
    :components ((:file "galosh-lisp")))
