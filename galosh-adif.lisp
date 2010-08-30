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

(load "qso.lisp")
(load "galosh-utils.lisp")

(defpackage :galosh-adif
  (:use :cl
	:galosh-qso
	:galosh-utils)
  (:export :map-over-qsos
	   :adif-error
	   :adif-error-message
	   :adif-error-value
	   :adif-error-line-number)
  (:shadow :read-char))
(in-package :galosh-adif)

(defun line-num (stream)
  (get 'linenum stream))
(defun (setf line-num) (val stream)
  (setf (get 'linenum stream) val))

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

;; Do something more useful than the default printer behaviour
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

(defun read-char (stream)
  "Wrap cl:read-char with a version that counts newlines in passing."
  (let ((r (cl:read-char stream)))
    (if (char= r #\Newline)
	(incf (line-num stream)))
    r))

;;; NB: We are using read-char below and not simply relying on peek-char to
;;; eat unwanted characters up to a #\< in order to allow our own read-char
;;; wrapper to count newlines.
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
      (let ((qso (make-qso)))
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
				     :line-number (line-num stream)))))))))

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
  (setf (line-num stream) 1)
  (read-header stream) ; Discard any header
  (let ((next-qso (make-qso-iterator stream)))
    (macrolet ((next () `(funcall next-qso)))
      (do ((qso (next) (next)))
	  ((null qso) 'done)
	(funcall func qso)))))
