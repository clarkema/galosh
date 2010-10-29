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

(defpackage :galosh-qrz
  (:use :cl :gl :galosh-qrzcom :clsql :galosh-grep :alexandria))
(in-package :galosh-qrz)

(clsql:file-enable-sql-reader-syntax)

(defvar *qrz-db* nil)
(defvar *country-by-id* #())
(declaim (vector *country-by-id*))

(defun get-country-translations ()
  (with-transaction (:database *qrz-db*)
    (let ((result (make-array (1+ (first (SELECT [max[id]] :FROM 'countries :flatp t :database *qrz-db*)))
			      :initial-element nil)))
      (setf (svref result 0) "Unknown country")
      (do-query ((id name) "SELECT id, name FROM countries" :database *qrz-db*)
	(setf (svref result id) (string-capitalize name)))
      result)))

(defun bulk-offline-qrz-search (call)
  (declare (optimize (speed 3)
		     (compilation-speed 0)))
  (let* ((c (string-upcase call))
	 (r (or (first (select [country_id] 
			       :from [amateur] :where [= 'call c] 
			       :limit 1 :caching nil :flatp t :field-names nil :database *qrz-db*))
		0)))
    (declare (fixnum r))
    (format t "~A:~c~A~%" c #\Tab
	    (if (< r (length *country-by-id*))
		(svref *country-by-id* r)
		(warn (format nil "Invalid country for ~A (~A)" c r))))))

(defun offline-qrz-search (call)
  (let* ((c (string-upcase call))
	 (r (first (select 'qrz-record :where [= 'call c] :limit 1 :caching nil :flatp t :database *qrz-db*))))
    (when r
      (format t "~%~A~%~A~%" (qrz-call r) (make-string (length (qrz-call r)) :initial-element #\=))
      (format t "~A, ~A~%" (string-upcase (qrz-lastname r)) (string-capitalize (qrz-firstname r)))
      (format t "~A~&~A~&~A~&~A~%~%" (string-capitalize (qrz-mailstreet r))
	      (string-capitalize (qrz-mailcity r))
	      (string-capitalize (qrz-mailstate r))
	      (country-name (qrz-country r))))))

(defun online-qrz-search (call)
  (let* ((client (make-instance 'qrzcom-client
				:username (get-config "qrz.user")
				:password (get-config "qrz.password")))
	 (result (details-by-call client call)))
    (format t "~A, ~A~%" (string-upcase (gethash "name" result)) (string-capitalize (gethash "fname" result)))
    (format t "~A~&~A~&~A~&" (string-capitalize (gethash "addr1" result))
	    (string-capitalize (gethash "addr2" result))
	    (string-upcase (gethash "country" result)))))

(defun process-options (argv)
  (multiple-value-bind (leftover options)
      (getopt:getopt argv '(("offline" :none)))
    (if (third leftover)
	(concatenate 'list options `(("sought" . ,(third leftover))))
	options)))

(defmacro with-qrz-db ((db) &body body)
  (with-gensyms (dbfile)
    `(let ((,dbfile ,db))
       (unwind-protect
	    (progn
	      (setf *qrz-db* (connect (list ,dbfile) :database-type :sqlite3 :make-default nil))
	      ,@body)
	 (when *qrz-db*
	   (disconnect :database *qrz-db*)
	   (setf *qrz-db* nil))))))

(define-galosh-command galosh-qrz (:required-configuration '("qrz.user" "qrz.password"))
  (let* ((options (process-options argv))
	 (sought (cdr (assoc "sought" options))))
    (if sought
	(progn
	  (if (assoc "offline" options)
	      (with-qrz-db ((get-config "qrz.offlinedb"))
		(offline-qrz-search sought))
	      (online-qrz-search sought))
	  (fresh-line)
	  (grep sought))
	(with-qrz-db ((get-config "qrz.offlinedb"))
	  (setf *country-by-id* (get-country-translations))
	  (with-transaction (:database *qrz-db*)
	    (do ((sought (read-line *standard-input* nil :eof) (read-line *standard-input* nil :eof)))
		((equal sought :eof) nil)
	      (bulk-offline-qrz-search sought)))))))
