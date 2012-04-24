;;;; galosh -- amateur radio utilities.
;;;; Copyright (C) 2010, 2011, 2012 Michael Clarke, M0PRL
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

(defpackage :galosh-qso
  (:use :cl :galosh-lisp :clsql)
  (:export :qso :with-qso-accessors :q-toggle-followup :as-string :*qso-slot-accessors*
	   :qso-datetime))
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
       (setf *qso-slot-accessors* (list ,@(loop for name in
					     accessor-names collect `(quote ,name))))
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
    his-owner
    his-name
    (his-age :type number)
    his-web
    his-email
    his-public-key
    my-call
    my-operator
    my-name
    my-owner
    (qrg :type integer :initform 0)
    (qrg-rx :type integer :initform 0)
    (band :noaccessors)
    (band-rx :noaccessors)
    (mode :initform "SSB")
    qso-date
    qso-date-off
    time-on
    time-off
    qso-complete
    qso-random
    net
    (tx-rst :initform "59")
    (rx-rst :initform "59")
    comment
    notes
    prop-mode
    his-sig
    his-sig-info
    (followup :type integer :initform 0)
    (swl :type integer :initform 0)

    ;; Conditions
    (his-power :type number)
    (my-power  :type number)
    (a-index   :type float)
    (k-index   :type float)
    sfi ; solar flux
    my-rig
    his-rig
    (distance :type float)

    (ant-az :type float)
    (ant-el :type float)
    ant-path

    ;; His QTH
    (his-cq-zone :type integer)
    (his-itu-zone :type integer)
    (his-dxcc :type integer)
    his-continent
    his-country
    his-state
    his-county ; cnty
    his-city ; -> qth in adif
    his-address;
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
    credit-submitted
    credit-granted

    ;; Meteor scatter
    (meteor-max-bursts :type integer)
    meteor-shower-name
    meteor-num-bursts
    meteor-num-pings

    ;; Satellite operating
    satellite-mode
    satellite-name

    ;; EME
    force-init)


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
    (if (and qrg (not (zerop qrg)))
	(qrg->band qrg)
	band)))

(defun (setf q-band) (band qso)
  (setf (slot-value qso 'band) band))

(defun q-band-rx (qso)
  (let ((qrg (slot-value qso 'qrg-rx))
	(band (slot-value qso 'band-rx)))
    (if (and qrg (not (zerop qrg)))
	(qrg->band qrg)
	band)))

(defun (setf q-band-rx) (band qso)
  (setf (slot-value qso 'band-rx) band))

(defun qso-datetime (qso)
  (let ((date (q-qso-date qso))
	(time (q-time-on qso)))
    (format nil "~A-~A-~AT~A:~A:~A+00:00"
	    (subseq date 0 4)
	    (subseq date 4 6)
	    (subseq date 6 8)
	    (subseq time 0 2)
	    (subseq time 2 4)
	    (subseq time 4 6))))
