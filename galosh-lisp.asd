(asdf:defsystem #:galosh-lisp
    :depends-on (#:clsql
		 #:split-sequence
		 #:py-configparser
		 #:alexandria
		 #:cl-ppcre
		 ;; For -entity.lisp
		 #:cxml)
    :components ((:file "galosh-lisp-package")
		 (:file "galosh-lisp"
			:depends-on ("galosh-lisp-package"))
		 (:file "galosh-entity"
			:depends-on ("galosh-lisp"))))
