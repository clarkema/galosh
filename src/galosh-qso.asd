(asdf:defsystem #:galosh-qso
    :depends-on (#:galosh-lisp
		 #:clsql)
    :components ((:file "qso")))
