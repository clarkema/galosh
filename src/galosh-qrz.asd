(asdf:defsystem #:galosh-qrz
  :depends-on (#:galosh-lisp
               #:galosh-qrzcom
               #:galosh-grep
               #:getopt
               #:clsql
               #:alexandria)
  :components ((:file "galosh-qrz")))
