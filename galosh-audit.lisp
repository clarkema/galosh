;;;; galosh -- amateur radio utilities.
;;;; Copyright (C) 2012 Michael Clarke, M0PRL
;;;; <mike -at- galosh.org.uk>
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License Version 3, as
;;;; published by the Free Software Foundation.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defpackage :galosh-audit
  (:use :cl :galosh-lisp :alexandria :clsql
	:galosh-qso))
(in-package :galosh-audit)

(clsql:file-enable-sql-reader-syntax)

(defun format-qso (q)
  (format nil "~A ~A ~A ~10A ~8@A ~6A ~3A ~3A"
	  (string-right-pad (length (princ-to-string (galosh-qso::max-qso-id))) (princ-to-string (q-id q)))
	  (q-qso-date q)
	  (q-time-on q)
	  (q-his-call q)
	  (q-qrg q)
	  (q-mode q)
	  (q-rx-rst q)
	  (q-tx-rst q)))

(defun check-bands (commit)
  (with-transaction ()
    (do-query ((qso) [SELECT 'qso :WHERE [null 'band]])
      (if commit
	(progn
	  (setf (q-band qso) (qrg->band (q-qrg qso)))
	  (update-records-from-instance qso)
	  (format t "~A no band: ~A!~%" (format-qso qso) (qrg->band (q-qrg qso))))
	(format t "~A no band: ~A?~%" (format-qso qso) (qrg->band (q-qrg qso)))))))

(defun process-options (argv)
  (multiple-value-bind (leftover options)
      (getopt:getopt argv '(("commit" :none t)))
    (declare (ignore leftover))
    options))

(defun buildapp-init ()
  (load-entity-information))

(define-galosh-command galosh-audit ()
  (let* ((options (process-options argv))
	 (commit (assoc "commit" options)))
    (check-bands commit)))
