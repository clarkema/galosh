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

(defpackage :galosh-adif
  (:use :cl
	:gl
	:gu
	:galosh-qso)
  (:export :map-over-qsos
	   :adif-error
	   :adif-error-message
	   :adif-error-value
	   :adif-error-line-number
	   :qso->adif)
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
   (make-translation :slot-name 'q-id :field-name "galosh_id")
   (make-translation :slot-name 'q-band :field-name "band")
   (make-translation :slot-name 'q-hiscall :field-name "call")
   (make-translation :slot-name 'q-comment :field-name "comment")
   (make-translation :slot-name 'q-qrg :field-name "freq"
                     :qso->adif #'(lambda (x) (format nil "~,6,f" (float (/ x 1000000)))))
   (make-translation :slot-name 'q-his-state :field-name "state")
   (make-translation :slot-name 'q-his-ve-prov :field-name "ve_prov")
   (make-translation :slot-name 'q-his-grid :field-name "gridsquare")
   (make-translation :slot-name 'q-our-grid :field-name "galosh_our_grid")
   (make-translation :slot-name 'q-his-iota :field-name "iota")
   (make-translation :slot-name 'q-our-iota :field-name "galosh_our_iota")
   (make-translation :slot-name 'q-mode :field-name "mode")
   (make-translation :slot-name 'q-name :field-name "name")
   (make-translation :slot-name 'q-qso-date :field-name "qso_date")
   (make-translation :slot-name 'q-tx-rst :field-name "rst_sent")
   (make-translation :slot-name 'q-rx-rst :field-name "rst_rcvd")
   (make-translation :slot-name 'q-time-on :field-name "time_on")
   (make-translation :slot-name 'q-time-off :field-name "time_off")
   (make-translation :slot-name 'q-operator :field-name "operator")
   (make-translation :slot-name 'q-followup :field-name "galosh_followup")))

(defparameter *slot-name->adif* (make-hash-table))
(defparameter *adif->slot-name* (make-hash-table))
(mapcar #'(lambda (tr)
	    (setf (gethash (t-slot-name tr) *slot-name->adif*) tr)
	    (setf (gethash (t-field-name tr) *adif->slot-name*) tr))
	*slot-translations*)

;;; ===================================================================

(defun package-adif (qso slot-name)
  (let* ((translator (gethash slot-name *slot-name->adif*))
	 (value (funcall (t-qso->adif translator) (funcall slot-name qso))))
    (when value
      (format nil "<~:@(~a~):~a>~a " (t-field-name translator)
	      (length (format nil "~a" value))
	      value))))

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
  (if (string= string "True")
      1
      0))

(defun read-record (stream)
  (if (eql (discard-until-tag stream) :EOF)
      :EOF
      (let ((qso (make-instance 'qso)))
	(do ((tag (read-tag stream) (read-tag stream)))
	    ((eql (tag-name tag) :EOR)
	     qso)
	  (let ((value (read-value stream tag)))
	    (case (tag-name tag)
	      (:call (setf (q-hiscall qso) value))
	      (:qso_date (setf (q-qso-date qso) value))
	      (:time_on (setf (q-time-on qso) value))
	      (:time_off (setf (q-time-off qso) value))
	      (:mode (setf (q-mode qso) value))
	      (:freq (setf (q-qrg qso) (decmhz->hz value)))
	      (:rst_sent (setf (q-tx-rst qso) (parse-integer value)))
	      (:rst_rcvd (setf (q-rx-rst qso) (parse-integer value)))
	      (:name (setf (q-name qso) value))
	      (:comment (setf (q-comment qso) value))
	      (:iota (setf (q-his-iota qso) value))
	      (:followup (setf (q-followup qso) (adifbool->sqlite value)))
	      (:tx_pwr (setf (q-tx-pwr qso) (parse-integer value)))
	      (:state (setf (q-his-state qso) value))
	      (:ve_prov (setf (q-his-ve-prov qso) value))
	      (:band (setf (q-band qso) value))
	      (:stx (setf (q-stx qso) value))
	      (:srx (setf (q-srx qso) value))
	      (:notes ())
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
