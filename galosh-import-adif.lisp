#!/usr/bin/sbcl --script

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

(require 'getopt)
(require 'clsql)
(load "qso.lisp")
(load "galosh-utils.lisp")

(use-package :galosh-qso)
(use-package :clsql-user)
(use-package :gu)

(defvar *comment-prefix* nil)

(defstruct tag
  name
  length
  type)
	   
(defun list->tag (l)
  "Make a tag structure from a list of the form (\"IOTA\", \"3\", \"2\")"
  (let ((tag (make-tag)))
    (setf (tag-name tag) (intern (first l) "KEYWORD"))
    (setf (tag-length tag) (if (stringp (second l))
			       (parse-integer (second l))
			       nil))
    (setf (tag-type tag) (third l))
    tag))

(defun discard-until-tag (stream)
  "Discard everything on stream 'stream' until we hit a < character."
  (peek-char #\< stream nil :eof))

(defun read-tag (&optional (stream *standard-input*))
  "Read an ADIF tag from 'stream'."
  (do* ((c (read-char stream) (setf c (read-char stream)))
	(buffer "" (setf buffer (concatenate 'string buffer (string c)))))
       ((char= c #\>)
	(list->tag (split #\: (string-trim "<>" buffer))))
    nil))

(defun read-value (stream tag)
  "Read an ADIF value from 'stream'"
  (do ((buffer (make-string (tag-length tag)))
       (i 0 (incf i)))
      ((eql i (tag-length tag))
       buffer)
    (setf (char buffer i) (read-char stream))))
	    

(defun read-field (stream)
  (unless (eql (discard-until-tag stream) :eof)
    (let ((tag (read-tag stream)))
      (cond ((string= (tag-name tag) "EOR")
	     (format t "End of that record. Next.")
	     (read-field stream))
	    (t
	     (format t "TAG: ~s  VALUE ~s ~%" (tag-name tag) (read-value stream tag))
	     (read-field stream))))))

(defun decmhz->hz (string)
  (let* ((f (split-sequence:split-sequence #\. string))
	 (mhz (parse-integer (first f)))
	 (khz (parse-integer (format nil "~6,,,,'0F" (second f)))))
    (+ (* 1000000 mhz) khz)))
	 
(defun adifbool->sqlite (string)
  (if (string= string "True")
      1
      0))

(defun wrap-comment (string)
  (if *comment-prefix*
      (if (> 0 (length string))
	  (format nil "~a~%~%~a" *comment-prefix* string)
	  *comment-prefix*)
      string))

(defun ensure-defaults (qso)
  (if (null (q-comment qso))
      (setf (q-comment qso) *comment-prefix*))
  (if (q-his-iota qso)
      (setf (q-followup qso) 1)))

(defun read-record (stream)
  (if (eql (discard-until-tag stream) :eof)
      :eof
      (let ((qso (make-qso)))
	(setf (q-operator qso) "VP8DMH")
	(do ((tag (read-tag stream)
		  (progn
		    (discard-until-tag stream)
		    (setf tag (read-tag stream)))))
	    ((eql (tag-name tag) :eor)
	     (ensure-defaults qso)
	     (update-records-from-instance qso)
	     (format t "~a" (as-string qso)))
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
	      (:comment (setf (q-comment qso) (wrap-comment value)))
	      (:iota (setf (q-his-iota qso) value))
	      (:followup (setf (q-followup qso) (adifbool->sqlite value)))
	      (:tx_pwr (setf (q-tx-pwr qso) (parse-integer value)))
	      (:state (setf (q-his-state qso) value))
	      (:ve_prov (setf (q-his-ve-prov qso) value))
	      (:band (setf (q-band qso) value))
	      (:stx (setf (q-stx qso) value))
	      (:srx (setf (q-srx qso) value))
	      (:notes ())
	      (otherwise (error (format nil "Unrecognised ADIF field ~s" (tag-name tag))))))))))
	      
(defun read-header (stream)
  ; If the first character of the file is #\<, there is no header.
  (if (char= (peek-char nil stream) #\<)
      nil
      (progn
	(discard-until-tag stream)
	(labels ((get-next-tag () (let ((tag (read-tag stream)))
				    (unless (string= (tag-name tag) "EOH")
				      (discard-until-tag stream)
				      (format t "~s" (tag-name tag))
				      (get-next-tag)))))
	  (get-next-tag)))))
	      
(defun scan-adif-file (stream)
  (read-header stream)
  (do ((record (read-record stream) (setf record (read-record stream))))
      ((eql record :eof) 'done)))

(defun process-options ()
  (multiple-value-bind (leftover options)
      (getopt:getopt *posix-argv* '(("comment-prefix" :required)))
    (if (< (length leftover) 2)
	(error "Error: please specify input file.")
	(concatenate 'list options `((filename . ,(second leftover)))))))

(defun main ()
  (let (options filename)
    (handler-case
	(progn
	  (setf options (process-options)
		filename (cdr (assoc 'filename options))
		*comment-prefix* (cdr (assoc "comment-prefix" options :test #'string=)))
	  (with-open-file (stream filename)
	    (unwind-protect
		 (progn
		   (connect '("log.db") :database-type :sqlite3)
		   (scan-adif-file stream))
	      (disconnect))))
      (file-error () (format t "Error: file `~a' does not exist!~%" filename))
      (simple-error (e) (format t "~a~%" e)))))

(main)
