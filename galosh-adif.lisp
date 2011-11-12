;;;; galosh -- amateur radio utilities.
;;;; Copyright (C) 2010, 2011 Michael Clarke, M0PRL
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

(defpackage :galosh-adif
  (:use :cl
	:galosh-lisp
	:galosh-qso)
  (:export :map-over-qsos
	   :adif-error
	   :adif-error-message
	   :adif-error-value
	   :adif-error-line-number
	   :qso->adif
	   :*standard-slots*)
  (:shadow :read-char))
(in-package :galosh-adif)

(defvar *line-number* nil)

;;; ===================================================================
;;; Condition handling and definition
;;; ===================================================================
(define-condition adif-error (error)
  ((message
    :initarg :message
    :accessor adif-error-message
    :initform nil
    :documentation "Text message indicating what went wrong with the validation.")
   (value
    :initarg :value
    :accessor adif-error-value
    :initform nil
    :documentation "The value of the field for which the error is signalled.")
   (line-number
    :initarg :line-number
    :accessor adif-error-line-number
    :initform nil
    :documentation "The number of the line on which the error was encountered.")))

(defmethod print-object ((object adif-error) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~@(~a~) ~s on line ~a"
            (adif-error-message object)
            (adif-error-value object)
            (adif-error-line-number object))))

(defun adif-error (message &key value line-number)
  (error 'adif-error
         :message message
         :value value
         :line-number line-number))

;;; ===================================================================
;;; ADIF tag representation
;;; ===================================================================
(defstruct tag
  name
  length
  type)

(defun list->tag (l)
  "Make a tag structure from a list of the form (\"IOTA\", \"3\", \"2\")"
  (let ((tag (make-tag)))
    (setf (tag-name tag) (intern (first l) "KEYWORD")
	  (tag-length tag) (if (stringp (second l))
			       (parse-integer (second l))
			       nil)
	  (tag-type tag) (third l))
    tag))

;;; ===================================================================
;;; Translations
;;; ===================================================================
(defstruct (translation (:conc-name t-))
  slot-name
  field-name
  (qso->adif #'identity)
  (adif->qso #'identity))

(defparameter *slot-translations*
  (list
   (make-translation :slot-name 'q-id :field-name "app_galosh_id")
   (make-translation :slot-name 'q-his-call :field-name "call")
   (make-translation :slot-name 'q-his-operator :field-name "app_galosh_his_operator")
   (make-translation :slot-name 'q-his-name :field-name "name")
   (make-translation :slot-name 'q-my-call :field-name "station_callsign")
   (make-translation :slot-name 'q-my-operator :field-name "operator")
   (make-translation :slot-name 'q-my-owner :field-name "owner_callsign")
   (make-translation :slot-name 'q-qrg :field-name "freq"
                     :qso->adif #'(lambda (x) (format nil "~,6,f" (float (/ x 1000000)))))
   (make-translation :slot-name 'q-band :field-name "band")
   (make-translation :slot-name 'q-mode :field-name "mode")
   (make-translation :slot-name 'q-qso-date :field-name "qso_date")
   (make-translation :slot-name 'q-qso-date-off :field-name "qso_date_off")
   (make-translation :slot-name 'q-time-on :field-name "time_on")
   (make-translation :slot-name 'q-time-off :field-name "time_off")
   (make-translation :slot-name 'q-tx-rst :field-name "rst_sent")
   (make-translation :slot-name 'q-rx-rst :field-name "rst_rcvd")
   (make-translation :slot-name 'q-comment :field-name "comment")
   (make-translation :slot-name 'q-notes :field-name "notes")
   (make-translation :slot-name 'q-prop-mode :field-name "prop_mode")
   (make-translation :slot-name 'q-followup :field-name "app_galosh_followup"
		     :qso->adif #'(lambda (x) (sqlite->adifbool x)))
   (make-translation :slot-name 'q-swl :field-name "swl"
		     :qso->adif #'(lambda (x) (sqlite->adifbool x)))
   (make-translation :slot-name 'q-his-power :field-name "rx_pwr")
   (make-translation :slot-name 'q-my-power :field-name "tx_pwr")
   (make-translation :slot-name 'q-a-index :field-name "a_index")
   (make-translation :slot-name 'q-k-index :field-name "k_index")
   (make-translation :slot-name 'q-sfi :field-name "sfi")
   (make-translation :slot-name 'q-my-rig :field-name "my_rig")
   (make-translation :slot-name 'q-ant-az :field-name "ant_az")
   (make-translation :slot-name 'q-ant-el :field-name "ant_el")
   ;; His QTH
   (make-translation :slot-name 'q-his-cq-zone :field-name "cqz")
   (make-translation :slot-name 'q-his-itu-zone :field-name "ituz")
   (make-translation :slot-name 'q-his-dxcc :field-name "dxcc")
   (make-translation :slot-name 'q-his-country :field-name "country")
   (make-translation :slot-name 'q-his-state :field-name "state")
   (make-translation :slot-name 'q-his-county :field-name "cnty")
   (make-translation :slot-name 'q-his-city :field-name "qth")
   (make-translation :slot-name 'q-his-iota :field-name "iota")
   (make-translation :slot-name 'q-his-iota-island-id :field-name "iota_island_id")
   (make-translation :slot-name 'q-his-grid :field-name "gridsquare")
   (make-translation :slot-name 'q-his-latitude :field-name "lat")
   (make-translation :slot-name 'q-his-longitude :field-name "lon")
   ;; My QTH
   (make-translation :slot-name 'q-my-cq-zone :field-name "my_cq_zone")
   (make-translation :slot-name 'q-my-itu-zone :field-name "my_itu_zone")
   (make-translation :slot-name 'q-my-dxcc :field-name "app_galosh_my_dxcc")
   (make-translation :slot-name 'q-my-country :field-name "my_country")
   (make-translation :slot-name 'q-my-state :field-name "my_state")
   (make-translation :slot-name 'q-my-county :field-name "my_cnty")
   (make-translation :slot-name 'q-my-city :field-name "my_city")
   (make-translation :slot-name 'q-my-street :field-name "my_street")
   (make-translation :slot-name 'q-my-postal-code :field-name "my_postal_code")
   (make-translation :slot-name 'q-my-iota :field-name "my_iota")
   (make-translation :slot-name 'q-my-iota-island-id :field-name "my_iota_island_id")
   (make-translation :slot-name 'q-my-grid :field-name "my_gridsquare")
   (make-translation :slot-name 'q-my-latitude :field-name "my_lat")
   (make-translation :slot-name 'q-my-longitude :field-name "my_lon")
   ;; Memberships
   (make-translation :slot-name 'q-his-arrl-section :field-name "arrl_sect")
   (make-translation :slot-name 'q-my-arrl-section :field-name "app_galosh_my_arrl_section")
   (make-translation :slot-name 'q-his-ten-ten :field-name "ten_ten")
   (make-translation :slot-name 'q-my-ten-ten :field-name "app_galosh_my_ten_ten")
   ;; Contesting
   (make-translation :slot-name 'q-contest-id :field-name "contest_id")
   (make-translation :slot-name 'q-contest-check :field-name "check")
   (make-translation :slot-name 'q-contest-class :field-name "class")
   (make-translation :slot-name 'q-contest-his-wpx-prefix :field-name "pfx")
   (make-translation :slot-name 'q-contest-precedence :field-name "precedence")
   (make-translation :slot-name 'q-contest-srx :field-name "srx")
   (make-translation :slot-name 'q-contest-srx-string :field-name "srx_string")
   (make-translation :slot-name 'q-contest-stx :field-name "stx")
   (make-translation :slot-name 'q-contest-stx-string :field-name "stx_string")
   ;; QSLing
   (make-translation :slot-name 'q-eqsl-rdate :field-name "eqsl_qslrdate")
   (make-translation :slot-name 'q-eqsl-sdate :field-name "eqsl_qslsdate")
   (make-translation :slot-name 'q-eqsl-recv :field-name "eqsl_qsl_rcvd")
   (make-translation :slot-name 'q-eqsl-sent :field-name "eql_qsl_sent")
   (make-translation :slot-name 'q-lotw-rdate :field-name "lotw_qslrdate")
   (make-translation :slot-name 'q-lotw-sdate :field-name "lotw_qslsdate")
   (make-translation :slot-name 'q-lotw-rcvd :field-name "lotw_qsl_rcvd")
   (make-translation :slot-name 'q-lotw-sent :field-name "lotw_qsl_sent")
   (make-translation :slot-name 'q-qsl-rdate :field-name "qslrdate")
   (make-translation :slot-name 'q-qsl-sdate :field-name "qslsdate")
   (make-translation :slot-name 'q-qsl-rcvd :field-name "qsl_rcvd")
   (make-translation :slot-name 'q-qsl-rcvd-via :field-name "qsl_rcvd_via")
   (make-translation :slot-name 'q-qsl-sent :field-name "qsl_sent")
   (make-translation :slot-name 'q-qsl-sent-via :field-name "qsl_sent_via")
   (make-translation :slot-name 'q-qsl-via :field-name "qsl_via")
   (make-translation :slot-name 'q-his-qsl-message :field-name "app_galosh_his_qsl_message")
   (make-translation :slot-name 'q-my-qsl-message :field-name "qslmsg")
   ;; Meteor scatter
   (make-translation :slot-name 'q-meteor-max-bursts :field-name "max_bursts")
   (make-translation :slot-name 'q-meteor-shower-name :field-name "ms_shower")
   (make-translation :slot-name 'q-meteor-num-bursts :field-name "nr_bursts")
   (make-translation :slot-name 'q-meteor-num-pings :field-name "nr_pings")
   ;; Satellite operating
   (make-translation :slot-name 'q-satellite-mode :field-name "sat_mode")
   (make-translation :slot-name 'q-satellite-name :field-name "sat_name")))

(defparameter *slot-name->adif* (make-hash-table))
(defparameter *adif->slot-name* (make-hash-table))
(mapcar #'(lambda (tr)
	    (setf (gethash (t-slot-name tr) *slot-name->adif*) tr)
	    (setf (gethash (t-field-name tr) *adif->slot-name*) tr))
	*slot-translations*)

(defun private-field-p (slot-name)
  (search "app_galosh" (t-field-name (gethash slot-name *slot-name->adif*))))

(defparameter *standard-slots* (remove-if #'private-field-p *qso-slot-accessors*))

;;; ===================================================================

(defun package-adif (qso slot-name &optional (fs #\Tab))
  (let* ((translator (gethash slot-name *slot-name->adif*))
	 (value (funcall (t-qso->adif translator) (funcall slot-name qso))))
    (when value
      (format nil "<~:@(~a~):~a>~a~a" (t-field-name translator)
	      (length (format nil "~a" value))
	      value
	      fs))))

(defun qso->adif (qso &key slots)
  (if (or (null slots) (eq slots :all))
      (setf slots (keys *slot-name->adif*)))
  (when qso
    (format nil "~{~:a~}<EOR>~%"
	    (remove nil (mapcar #'(lambda (slot-name) (package-adif qso slot-name))
				slots)))))

(defun read-char (stream)
  "Wrap cl:read-char with a version that counts newlines in passing."
  (let ((r (cl:read-char stream)))
    (when (char= r #\Newline)
      (incf *line-number*))
    r))

;; NB: We are using read-char below and not simply relying on peek-char to
;; eat unwanted characters up to a #\< in order to allow our own read-char
;; wrapper to count newlines.
(defun discard-until-tag (stream)
  "Discard everything on a stream until we hit a < character."
  (case (peek-char nil stream nil :eof)
    ((eql :EOF) :EOF)
    ((char= #\<) nil)
    (otherwise (read-char stream)
	       (discard-until-tag stream))))

(defun read-tag (&optional (stream *standard-input*))
  "Read an ADIF tag from a stream."
  (discard-until-tag stream)
  (do* ((c (read-char stream) (read-char stream))
	(buffer "" (concatenate 'string buffer (string c))))
       ((char= c #\>)
	(list->tag (split #\: (string-trim "<>" buffer))))
    nil))

(defun read-value (stream tag)
  "Read an ADIF value from 'stream'"
  (do ((buffer (make-string (tag-length tag)))
       (i 0 (1+ i)))
      ((eql i (tag-length tag))
       buffer)
    (setf (char buffer i) (read-char stream))))

(defun decmhz->hz (string)
  (let* ((f (split #\. string))
	 (mhz (parse-integer (first f)))
	 (khz (parse-integer (format nil "~6,,,,'0F" (second f)))))
    (+ (* 1000000 mhz) khz)))

(defun adifbool->sqlite (string)
  (if (string= string "Y")
      1
      0))

(defun sqlite->adifbool (int)
  (if (= int 1)
      "Y"
      "N"))

(defun read-record (stream)
  (if (eql (discard-until-tag stream) :EOF)
      :EOF
      (let ((qso (make-instance 'qso)))
	(do ((tag (read-tag stream) (read-tag stream)))
	    ((eql (tag-name tag) :EOR)
	     qso)
	  (let ((value (read-value stream tag)))
	    (case (tag-name tag)
	      (:app_galosh_id (setf (q-id qso) (parse-integer value)))
	      (:call (setf (q-his-call qso) value))
	      (:qso_date (setf (q-qso-date qso) value))
	      (:time_on (setf (q-time-on qso) value))
	      (:time_off (setf (q-time-off qso) value))
	      (:mode (setf (q-mode qso) value))
	      (:freq (setf (q-qrg qso) (decmhz->hz value)))
	      (:prop_mode (setf (q-prop-mode qso) value))
	      (:rst_sent (setf (q-tx-rst qso) (parse-integer value)))
	      (:rst_rcvd (setf (q-rx-rst qso) (parse-integer value)))
	      (:name (setf (q-his-name qso) value))
	      (:comment (setf (q-comment qso) value))
	      (:iota (setf (q-his-iota qso) value))
	      (:my_iota (setf (q-my-iota qso) value))
	      (:app_galosh_followup (setf (q-followup qso) (adifbool->sqlite value)))
	      (:tx_pwr (setf (q-my-pwr qso) (parse-integer value)))
	      (:state (setf (q-his-state qso) value))
	      (:swl (setf (q-swl qso) (adifbool->sqlite value)))
	      (:ve_prov (setf (q-his-state qso) value))
	      (:operator (setf (q-my-operator qso) value))
	      (:station_callsign (setf (q-my-call qso) value))
	      (:band (setf (q-band qso) value))
	      (:stx (setf (q-contest-stx qso) value))
	      (:srx (setf (q-contest-srx qso) value))
	      (:notes ())
	      (:my_gridsquare (setf (q-my-grid qso) value))
	      (otherwise (adif-error "unrecognised ADIF field"
				     :value (tag-name tag)
				     :line-number *line-number*))))))))

(defun read-header (stream)
  ; If the first character of the file is #\<, there is no header.
  (if (char= (peek-char nil stream) #\<)
      nil
      (do ((tag (read-tag stream) (read-tag stream)))
	  ((eql (tag-name tag) :EOH) t))))

(defun make-qso-iterator (stream)
  #'(lambda () (let ((r (read-record stream)))
		 (if (eql r :eof)
		     nil
		     r))))

(defun map-over-qsos (func stream)
  (let ((*line-number* 1))
    (read-header stream) ; Discard any header
    (let ((next-qso (make-qso-iterator stream)))
      (macrolet ((next () `(funcall next-qso)))
	(do ((qso (next) (next)))
	    ((null qso) 'done)
	  (funcall func qso))))))
