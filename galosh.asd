(asdf:defsystem #:galosh
    :depends-on (#:hunchentoot
		 #:clsql-sqlite3
		 #:yaclml
		 #:cl-ncurses
		 #:split-sequence
		 #:vecto
		 #:drakma
		 #:cxml
		 #:getopt
		 #:py-configparser))
