(asdf:defsystem #:galosh-web
  :depends-on (#:galosh-lisp
               #:galosh-qso
               #:galosh-qrzcom
               #:getopt
               #:clsql
               #:cl-markup
               #:hunchentoot
               #:parenscript)
  :components ((:file "galosh-web")))
