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

(defpackage :galosh-qso
  (:use :cl :gl :clsql)
  (:export :qso :with-qso-accessors))
(in-package :galosh-qso)

(clsql:file-enable-sql-reader-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun merge-field-defaults (defaults field-spec)
    (if (null defaults)
	(if (or (member :type field-spec) (member :db-type field-spec))
	    field-spec
	    (append field-spec (list :db-type "TEXT")))
	(if (member (caar defaults) field-spec)
	    (merge-field-defaults (cdr defaults) field-spec)
	    (merge-field-defaults (cdr defaults) (append field-spec (car defaults)))))))

(defmacro accessor-name (name)
  `(symb "Q-" ,name))

(defmacro def-qso-class (&rest field-defs)
  (let* ((defaults '((:initform nil)))
	 (accessor-names nil)
	 (fields (loop for field in field-defs collect
		      (cond ((consp field)
			     (let ((name (first field)))
			       (push (accessor-name name) accessor-names)
			       (merge-field-defaults (append
						      (list `(:initarg ,(mkkeyword name))
							    (unless (member :noaccessors field)
							      `(:accessor ,(accessor-name name))))
						      defaults)
						     (remove :noaccessors field))))
			    (t
			     (push (symb "Q-" field) accessor-names)
			     `(,field :db-type "TEXT" :accessor ,(accessor-name field)
				      :initarg ,(mkkeyword field) :initform nil))))))
    `(progn
       (def-view-class qso ()
	 (,@fields)
	  (:base-table "qso"))
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (export (list ,@(loop for name in accessor-names collect `(quote ,name)))))
       (defmacro with-qso-accessors (qso &body body)
	 `(with-accessors ,',(loop for field in accessor-names
				  collect (list field field))
	      ,qso
	    ,@body)))))

(def-qso-class
    (id
     :type integer
     :db-type "INTEGER" ; Required to make sqlite autoincrement work properly.
     :db-kind :key
     :db-constraints :not-null)
    (band :noaccessors)
    (qrg :type integer :initform 0)
    (mode :initform "SSB")
    (tx-rst :type integer :initform 59)
    (rx-rst :type integer :initform 59)
    (tx-pwr :type integer)
    (his-dxcc :type integer)
    (followup :type integer :initform 0)
    qso-date time-on time-off operator hiscall
    prop-mode stx srx name his-state his-ve-prov
    his-iota our-iota comment his-grid our-grid)

(defun max-qso-id ()
  (first (select [max [id]] :from 'qso :flatp t)))

(defgeneric as-string (x))
(defmethod as-string ((q qso))
  (format nil "~a ~a ~a ~10a ~8@a ~6a ~3a ~3a ~6a ~a~%"
	  (string-right-pad (length (princ-to-string (max-qso-id))) (princ-to-string (q-id q)))
	  (q-qso-date q)
	  (q-time-on q)
	  (q-hiscall q)
	  (q-qrg q)
	  (q-mode q)
	  (q-rx-rst q)
	  (q-tx-rst q)
	  (or (q-his-iota q) "")
	  (or (q-comment q) "")))

(defun q-toggle-followup (qso)
  (setf (q-followup qso) (if (eql 1 (q-followup qso)) 0 1)))

(defun q-band (qso)
  (let ((qrg  (slot-value qso 'qrg))
	(band (slot-value qso 'band)))
    (if qrg
	(qrg->band qrg)
	band)))

(defun (setf q-band) (band qso)
  (setf (slot-value qso 'band) band))
