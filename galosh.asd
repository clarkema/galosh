(asdf:defsystem #:galosh
    :depends-on (#:hunchentoot
		 #:clsql
		 #:cl-who
		 #:cl-ncurses
		 #:split-sequence
		 #:vecto
		 #:drakma
		 #:cxml))