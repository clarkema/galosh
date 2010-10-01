(asdf:defsystem #:galosh-init-db
    :depends-on (#:clsql)
    :components ((:file "galosh-lisp")
		 (:file "qso"
			:depends-on ("galosh-lisp"))
		 (:file "journal-entry")
		 (:file "galosh-init-db"
			:depends-on ("qso" "journal-entry"))))
