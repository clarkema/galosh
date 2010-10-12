(asdf:defsystem #:galosh-config
    :depends-on (#:galosh-lisp
		 #:py-configparser)
    :components ((:file "galosh-config")))
