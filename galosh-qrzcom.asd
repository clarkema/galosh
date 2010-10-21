(asdf:defsystem #:galosh-qrzcom
    :depends-on (#:galosh-lisp
		 #:cxml
		 #:drakma
		 #:clsql)
    :components ((:file "galosh-qrzcom")))
