(asdf:defsystem #:galosh-grep
    :depends-on (#:clsql
		 #:split-sequence)
    :components ((:file "galosh-lisp")
		 (:file "qso"
			:depends-on ("galosh-lisp"))
		 (:file "galosh-grep"
			:depends-on ("qso" "galosh-lisp"))))