(asdf:defsystem #:galosh
    :depends-on (#:hunchentoot
		 #:parenscript
		 #:clsql-sqlite3
		 #:cl-markup
		 #:cl-ncurses
		 #:split-sequence
		 #:vecto
		 #:drakma
		 #:cxml
		 #:getopt
		 #:py-configparser
		 #:fiveam
		 #:jpl-queues
		 #:screamer
		 #:st-json))
