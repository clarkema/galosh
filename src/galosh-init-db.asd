(asdf:defsystem #:galosh-init-db
  :depends-on (#:galosh-lisp
               #:galosh-qso
               #:clsql)
  :components ((:file "journal-entry")
               (:file "galosh-init-db"
                :depends-on ("journal-entry"))))
