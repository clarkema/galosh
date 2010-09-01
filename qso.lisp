;;;; galosh -- amateur radio utilities.
;;;; Copyright (C) 2010 Michael Clarke, M0PRL
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

(require 'clsql)

(defpackage :galosh-qso
  (:use :cl :clsql-user)
  (:export :qso :make-qso
	   :q-qso-date :q-time-on :q-time-off :q-operator :q-hiscall :q-band :q-qrg :q-mode
	   :q-tx-rst :q-rx-rst :q-stx :q-srx :q-name :q-his-iota :q-our-iota :q-comment :q-followup :q-tx-pwr
	   :q-his-state :q-his-ve-prov :as-string :q-his-grid :q-our-grid
	   :q-toggle-followup))
(in-package :galosh-qso)

(def-view-class qso ()
  ((id 
    :type integer
    :db-type "INTEGER" ; Required to make sqlite autoincrement work properly.
    :db-kind :key
    :db-constraints :not-null
    :initarg :id
    :initform nil
;    :initform (sequence-next 'qso-ids)
    :accessor q-id)
   (qso-date
    :db-type "TEXT"
    :accessor q-qso-date)
   (time-on
    :db-type "TEXT"
    :accessor q-time-on)
   (time-off
    :db-type "TEXT"
    :accessor q-time-off)
   (operator
    :db-type "TEXT"
    :accessor q-operator
    :initform nil)
   (hiscall
    :db-type "TEXT"
    :accessor q-hiscall)
   (band
    :db-type "TEXT"
    :accessor q-band)
   (qrg
    :type integer
    :accessor q-qrg
    :initform 0)
   (mode
    :db-type "TEXT"
    :accessor q-mode
    :initform "SSB")
   (tx-rst
    :type integer
    :accessor q-tx-rst)
   (rx-rst 
    :type integer
    :accessor q-rx-rst)
   (stx
    :db-type "TEXT"
    :accessor q-stx)
   (srx
    :db-type "TEXT"
    :accessor q-srx)
   (tx-pwr
    :documentation "Power of this station in Watts"
    :type integer
    :accessor q-tx-pwr)
   (name 
    :documentation "Other station's operator's name"
    :db-type "TEXT"
    :accessor q-name
    :initform nil)
   (his-state
    :documentation "Other station's US state abbreviation"
    :db-type "TEXT"
    :accessor q-his-state
    :initform nil)
   (his-ve-prov
    :documentation "Other station's Candadian province"
    :db-type "TEXT"
    :accessor q-his-ve-prov)
   (his-iota
    :documentation "Other station's IOTA reference"
    :db-type "TEXT"
    :accessor q-his-iota
    :initform nil)
   (our-iota
    :documentation "This station's IOTA reference"
    :db-type "TEXT"
    :accessor q-our-iota)
   (comment
    :db-type "TEXT"
    :accessor q-comment
    :initform nil)
   (followup
    :type integer
    :accessor q-followup
    :initform 0)
   (his-grid
    :documentation "Other station's Maidenhead grid reference"
    :db-type "TEXT"
    :accessor q-his-grid
    :initform nil)
   (our-grid
    :documentation "This station's Maidenhead grid reference"
    :db-type "TEXT"
    :accessor q-our-grid
    :initform nil))
  (:base-table "qso"))

(defgeneric as-string (x))
(defmethod as-string ((q qso))
  (format nil "~a ~a ~a ~10a ~8@a ~6a ~3a ~3a ~6a ~a~%"
	  (q-id q)
	  (q-qso-date q)
	  (q-time-on q)
	  (q-hiscall q)
	  (q-qrg q)
	  (q-mode q)
	  (q-rx-rst q)
	  (q-tx-rst q)
	  (q-his-iota q)
	  (q-comment q)))
 
(defun q-toggle-followup (qso)
  (setf (q-followup qso) (if (eql 1 (q-followup qso)) 0 1)))

(defmacro make-qso ()
  '(make-instance (quote qso)))
