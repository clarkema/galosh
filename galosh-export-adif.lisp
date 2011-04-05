;;;; galosh -- amateur radio utilities.
;;;; Copyright (C) 2010, 2011 Michael Clarke, M0PRL
;;;; <clarkema -at- clarkema.org>
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

(defpackage :galosh-export-adif
  (:use :cl :gl :clsql :galosh-qso :galosh-adif))
(in-package :galosh-export-adif)

(clsql:file-enable-sql-reader-syntax)

(defparameter *slot-sets* (make-hash-table :test 'equal))
(setf (gethash "std" *slot-sets*) '(q-band
				    q-hiscall
				    q-his-dxcc
				    q-comment
				    q-qrg
				    q-prop-mode
				    q-his-state
				    q-his-ve-prov
				    q-his-grid
				    q-his-iota
				    q-mode
				    q-qso-date
				    q-tx-rst
				    q-rx-rst
				    q-time-on
				    q-time-off
				    q-operator))

(defun process-options (argv)
  (multiple-value-bind (leftover options)
      (getopt:getopt argv '(("fieldset" :optional)
			    ("list-fieldsets" :none t)))
    options))

(defun print-header ()
  (say "Exported from Galosh -- a suite of amateur radio tools for Linux.")
  (format t "<PROGRAMID:~A>~A <EOR>~%" (length "GALOSH") "GALOSH")
  (format t "<PROGRAMVERSION:~A>~A <EOR>~%" (length *galosh-version*) *galosh-version*)
  (say "<EOH>"))

(define-galosh-command galosh-export-adif ()
  (let* ((options (process-options argv))
	 (set (or (cdr (assoc "fieldset" options)) "std")))
    (if (cdr (assoc "list-fieldsets" options))
	(format t "~{~:A~%~}~&all~%" (keys *slot-sets*))
	(progn
	  (print-header)
	  (mapcar #'(lambda (x)
		      (princ (qso->adif x
					:slots (if (string= set "all")
						   :all
						   (gethash set *slot-sets*)))))
		  (reverse (select 'qso :order-by '(([qso_date] :desc)([time_on] :desc)) :flatp t)))))))
