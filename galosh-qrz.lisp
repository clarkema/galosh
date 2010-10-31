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
  (with-qrz-db ((get-config "qrz.offlinedb"))
    (let* ((c (string-upcase call))
	   (r (first (select 'qrz-record :where [= 'call c] :limit 1 :caching nil :flatp t :database *qrz-db*))))
      (when r
	(format t "~A, ~A~%" (string-upcase (qrz-lastname r)) (string-capitalize (qrz-firstname r)))
	(format t "~A~&~A~&~A~&" (string-capitalize (qrz-mailstreet r))
		(string-capitalize (qrz-mailcity r))
		(string-capitalize (qrz-mailstate r)))
	(princ-unless-nil (qrz-country-name r) :fl t)))))

(defun raw-online-qrz-search (call)
  (let* ((client (make-instance 'qrzcom-client
				:username (get-config "qrz.user")
				:password (get-config "qrz.password")))
	 (result (details-by-call client call)))
    (if result
	(loop for k being each hash-key of result using (hash-value v)
	     do (format t "~A:~A~A~%" k #\Tab v)))
    result))

(defun princ-unless-nil (obj &key (fl nil))
  (unless (null obj)
    (princ obj)
    (if fl
	(fresh-line))))

(defun prepend-newline (string)
  (unless (zerop (length string))
      (cats (format nil "~%") string)))

(defun section-qsl (result)
  (flet ((r (key) (gethash key result)))
    (with-output-to-string (*standard-output*)
      (when-let ((qsl-via (r "qslmgr")))
	(format t "QSL Via: ~A~%" qsl-via))
      (princ "QSL Methods: ")
      (princ (join (remove-if #'null (list (if (equal (r "mqsl") "1") "Direct")
					   (if (equal (r "eqsl") "1") "eQSL")
					   (if (equal (r "lotw") "1") "LoTW")))
		   ", "))
      (fresh-line))))

(defun section-qth (result)
  (flet ((r (key) (gethash key result)))
    (with-output-to-string (*standard-output*)
      (when-let ((lat (r "lat"))
		 (lon (r "lon")))
	(format t "Lat/Long: ~A ~A~&" lat lon))
      (when-let ((grid (r "grid")))
	(format t "Grid: ~A~&" grid))
      (when-let ((iota (r "iota")))
	(format t "IOTA: ~A~&" iota)))))

(defun online-qrz-search (call)
  (let* ((client (make-instance 'qrzcom-client
				:username (get-config "qrz.user")
				:password (get-config "qrz.password")))
	 (result (details-by-call client call)))
    (if result
	(flet ((r (key) (gethash key result)))
	  (format t "~A, ~A~%" (string-upcase (r "name")) (string-capitalize (r "fname")))
	  (format t "~A~&~A~&~A~&" (string-capitalize (r "addr1"))
		  (string-capitalize (r "addr2"))
		  (string-upcase (r "country")))
	  (mapcar (lambda (section)
		    (princ-unless-nil (prepend-newline (funcall section result))))
		  (list #'section-qth #'section-qsl))))))

(defun process-options (argv)
  (multiple-value-bind (leftover options)
      (getopt:getopt argv '(("offline" :none) ("raw" :none)))
    (if (third leftover)
	(concatenate 'list options `(("sought" . ,(third leftover))))
	options)))

(define-galosh-command galosh-qrz (:required-configuration '("qrz.user" "qrz.password"))
  (let* ((options (process-options argv))
	 (sought (cdr (assoc "sought" options))))
    (if sought
	(cond ((assoc "offline" options)
	       (offline-qrz-search sought))
	      ((assoc "raw" options)
	       (raw-online-qrz-search sought))
	      (t (online-qrz-search sought)))
	(with-qrz-db ((get-config "qrz.offlinedb"))
	  (setf *country-by-id* (get-country-translations))
	  (with-transaction (:database *qrz-db*)
	    (do ((sought (read-line *standard-input* nil :eof) (read-line *standard-input* nil :eof)))
		((equal sought :eof) nil)
	      (bulk-offline-qrz-search sought)))))))
