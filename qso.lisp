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
  (:export :qso :with-qso-accessors :q-toggle-followup :as-string :*qso-slot-accessors*))
(in-package :galosh-qso)

(clsql:file-enable-sql-reader-syntax)
(defvar *slot-accessors* ())

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
       (setf *qso-slot-accessors* (list ,@(loop for name in accessor-names collect `(quote ,name))))
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

    ;; Basics
    his-call
    his-operator
    his-name
    my-call
    my-operator
    my-owner
    (qrg :type integer :initform 0)
    (band :noaccessors)
    (mode :initform "SSB")
    qso-date
    qso-date-off
    time-on
    time-off
    (tx-rst :type integer :initform 59)
    (rx-rst :type integer :initform 59)
    comment
    notes
    prop-mode
    (followup :type integer :initform 0)
    (swl :type integer :initform 0)

    ;; Conditions
    (his-power :type integer)
    (my-power  :type integer)
    (a-index   :type integer)
    (k-index   :type integer)
    sfi ; solar flux
    my-rig

    (ant-az :type integer)
    (ant-el :type integer)

    ;; His QTH
    (his-cq-zone :type integer)
    (his-itu-zone :type integer)
    (his-dxcc :type integer)
    his-country
    his-state
    his-county ; cnty
    his-city ; -> qth in adif
    his-iota
    his-iota-island-id
    his-grid
    his-latitude
    his-longitude

    ;; My QTH
    (my-cq-zone :type integer)
    (my-itu-zone :type integer)
    (my-dxcc :type integer)
    my-country
    my-county
    my-state
    my-city
    my-street
    my-postal-code
    my-iota
    my-iota-island-id
    my-grid
    my-latitude
    my-longitude

    ;; Memberships
    his-arrl-section
    my-arrl-section
    his-ten-ten
    my-ten-ten

    ;; Contesting
    contest-id
    contest-check
    contest-class
    contest-his-wpx-prefix
    contest-precedence
    contest-srx
    contest-srx-string
    contest-stx
    contest-stx-string

    ;; QSLing
    eqsl-rdate
    eqsl-sdate
    eqsl-recv
    eqsl-sent
    lotw-rdate
    lotw-sdate
    lotw-rcvd
    lotw-sent
    qsl-rdate
    qsl-sdate
    qsl-rcvd
    qsl-rcvd-via
    qsl-sent
    qsl-sent-via
    qsl-via
    his-qsl-message
    my-qsl-message

    ;; Meteor scatter
    (meteor-max-bursts :type integer)
    meteor-shower-name
    meteor-num-bursts
    meteor-num-pings

    ;; Satellite operating
    satellite-mode
    satellite-name)


(defun max-qso-id ()
  (first (select [max [id]] :from 'qso :flatp t)))

(defgeneric as-string (x))
(defmethod as-string ((q qso))
  (format nil "~a ~a ~a ~10a ~8@a ~6a ~3a ~3a ~6a ~a~%"
	  (string-right-pad (length (princ-to-string (max-qso-id))) (princ-to-string (q-id q)))
	  (q-qso-date q)
	  (q-time-on q)
	  (q-his-call q)
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
