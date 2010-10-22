(asdf:defsystem #:galosh-init-db
    :depends-on (#:galosh-lisp
		 #:clsql)
    :components ((:file "qso")
		 (:file "journal-entry")
		 (:file "galosh-init-db"
			:depends-on ("qso" "journal-entry"))))
