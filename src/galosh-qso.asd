(asdf:defsystem #:galosh-qso
  :depends-on (#:galosh-lisp
               #:galosh-prove
               #:clsql)
  :components ((:file "qso")))
