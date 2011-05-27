(defpackage :galosh-lisp
  (:nicknames :gl)
  (:use :cl :clsql :py-configparser :alexandria)
  (:export :*galosh-db*
	   :*galosh-version*
	   :with-safe-io-syntax
	   :mklist
	   :mkstr
	   :symb
	   :mkkeyword
	   :split
	   :split-words
	   :last1
	   :default-rst-for-mode
	   :valid-callsign-char-p
	   :sane-callsign-p
	   :empty-string-p
	   :string-right-pad
	   :n->es
	   :es->n
	   :cats
	   :join
	   :say
	   :keys
	   :default
	   :with-galosh-db
	   :define-galosh-command
	   :get-galosh-dir
	   :get-time-fudge
	   :fatal-get-galosh-dir
	   :missing-galosh-db-error
	   :missing-galosh-dir-error
	   :qrg->band
	   :human-date
	   :has-config-p
	   :get-config
	   :check-required-config
	   :missing-mandatory-configuration-error
	   :terminate
	   :deg->rad
	   :rad->deg
	   :great-circle-heading
	   ;; Entity processing
	   :entity
	   :entity-prefix
	   :entity-name
	   :entity-adif
	   :entity-cq-zone
	   :entity-itu-zone
	   :entity-continent
	   :entity-latitude
	   :entity-longitude
	   :entity-start
	   :entity-end
	   :entity-information-available-p
	   :get-entity
	   :entity-not-found-error
	   :load-entity-information))