(defpackage :galosh-lisp
  (:use :cl :clsql :py-configparser :alexandria :cl-log)
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
	   :lecture
	   :open-in-browser
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
	   :log-date-time
	   :log-date
	   :log-time
	   :drop-last
	   :has-config-p
	   :get-config
	   :check-required-config
	   :missing-mandatory-configuration-error
	   :terminate
	   :deg->rad
	   :rad->deg
	   :great-circle-heading
	   :locator->decdeg
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
	   :load-entity-information
	   :entity-name->adif
	   :entity-adif->name
	   :all-entity-names
	   :given
	   :drop-last
	   :kill-last-word
	   :log-fatal
	   :log-error
	   :log-warn
	   :log-info
	   :log-debug
	   :log-trace))
