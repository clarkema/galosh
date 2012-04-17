(asdf:defsystem #:galosh
    :depends-on (#:hunchentoot
		 #:parenscript
		 #:clsql-sqlite3
		 #:cl-log
		 #:cl-markup
		 #:cl-ncurses
		 #:split-sequence
		 #:vecto
		 #:drakma
		 #:cxml
		 #:getopt
		 #:parse-number
		 #:py-configparser
		 #:fiveam
		 #:jpl-queues
		 #:screamer
		 #:st-json
		 #:swank))
