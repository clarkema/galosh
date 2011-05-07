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

(defpackage :galosh-import-qrz-db
  (:use :cl :gl :clsql-user :galosh-qrzcom)
  (:shadow :read-line))
(in-package :galosh-import-qrz-db)

(clsql:file-enable-sql-reader-syntax)

(defvar *line-number* nil)

(defun read-line (stream)
  "Wrap cl:read-line with a version that counts newlines in passing."
  (let ((r (cl:read-line stream nil nil)))
    (when r
      (incf *line-number*))
    r))

(defun read-record (stream)
  (if (= (mod *line-number* 1000) 0)
      (format t "~a~%" *line-number*))
  (let* ((line (read-line stream))
	 (fields (mapcar #'(lambda (s) (string-trim '(#\Space) s)) (split #\, line))))
    (if (and line (= (length fields) 15))
	(make-qrz-record fields)
	(if line
	    (progn
					;	      (format t "Not enough fields ~s~%" fields)
	      (read-record stream))
	    :eof))))

(defun make-record-iterator (stream)
  #'(lambda () (let ((r (read-record stream)))
		 (if (eql r :eof)
		     nil
		     r))))

(defun map-over-call-records (func stream)
  (setf *line-number* 1)
  (let ((next-record (make-record-iterator stream)))
    (macrolet ((next () `(funcall next-record)))
      (do ((record (next) (next)))
	  ((null record) 'done)
	(funcall func record)))))

(defun import-call-file (path)
  (with-open-file (stream path)
    (with-transaction () 
      (map-over-call-records #'(lambda (r)
				 (update-records-from-instance r))
			     stream))))

(defun import-country-file (path)
  (with-open-file (stream path)
    (with-transaction ()
      (do ((line (cl:read-line stream nil) (cl:read-line stream nil)))
	  ((null line) :done)
	(let* ((record (split #\Tab line))
	       (country (make-instance 'qrz-country 
				       :id (parse-integer (first record) :junk-allowed t) 
				       :name (second record))))
	  (format t "~s~%" record)
	  (update-records-from-instance country))))))

(defun create-indicies ()
  (create-index [amateur-calls] :on [amateur]
		:attributes '([call])
		:unique t))

(defun init-db ()
  (truncate-database)
  (create-view-from-class 'qrz-record)
  (create-view-from-class 'qrz-country))
  
(defun process-options (argv)
  (multiple-value-bind (leftover options)
      (getopt:getopt argv '())
    (if (< (length leftover) 2)
	(error "Error: please specify input file.")
	(concatenate 'list options `((call-file . ,(third leftover))
				     (country-file . ,(fourth leftover)))))))

(defun main (argv)
  (let ((options (process-options argv)))
    (handler-case
	(unwind-protect
	     (progn
	       (connect (list (get-config "qrz.offlinedb")) :database-type :sqlite3)
	       (init-db)
	       (import-call-file (cdr (assoc 'call-file options)))
	       (import-country-file (cdr (assoc 'country-file options)))
	       (create-indicies))
	  (disconnect))
      (file-error () (format t "Error: file `~a' does not exist!~%" (cdr (assoc 'call-file options))))
      (simple-error (e) (format t "~a~%" e)))))
