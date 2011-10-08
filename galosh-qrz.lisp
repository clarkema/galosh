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

;;;; This file contains both library functions, designed for use by other
;;;; parts of the suite such as the logger and qsl modules; and the code
;;;; of the qrz module itself, implemented as a thin layer on top of the
;;;; library.

(defpackage :galosh-qrz
  (:use :cl :gl :galosh-qrzcom :clsql :galosh-grep :alexandria)
  (:export :has-offlinedb-p :offline-qrz-search :qrz-search))
(in-package :galosh-qrz)

(clsql:file-enable-sql-reader-syntax)

(defvar *qrz-db* nil)
(defvar *country-by-id* #())
(declaim (vector *country-by-id*))

(defparameter *us-state-code->name* (make-hash-table :test 'equal))
(defparameter *us-state-name->code* (make-hash-table :test 'equal))
(mapcar #'(lambda (l)
	    (setf (gethash (first l)  *us-state-code->name*) (second l))
	    (setf (gethash (second l) *us-state-name->code*) (first l)))
	(list '("AL" "Alabama") '("AK" "Alaska") '("AZ" "Arizona")
	      '("AR" "Arkansas") '("CA" "California") '("CO" "Colorado")
	      '("CT" "Conneticut") '("DE" "Delaware") '("DC" "District of Columbia")
	      '("FL" "Florida") '("GA" "Georgia") '("HI" "Hawaii")
	      '("ID" "Idaho") '("IL" "Illinois") '("IN" "Indiana")
	      '("IA" "Iowa") '("KS" "Kansas") '("KY" "Kentucky")
	      '("LA" "Louisiana") '("ME" "Maine") '("MD" "Maryland")
	      '("MA" "Massachusetts") '("MI" "Michigan") '("MN" "Minnesota")
	      '("MS" "Mississippi") '("MO" "Missouri") '("MT" "Montana")
	      '("NE" "Nebraska") '("NV" "Nevada") '("NH" "New Hampshire")
	      '("NJ" "New Jersey") '("NM" "New Mexico") '("NY" "New York")
	      '("NC" "North Carolina") '("ND" "North Dakota") '("OH" "Ohio")
	      '("OK" "Oklahoma") '("OR" "Oregon") '("PA" "Pennsylvania")
	      '("PR" "Puerto Rico") '("RI" "Rhode Island") '("SC" "South Carolina")
	      '("SD" "South Dakota") '("TN" "Tennessee") '("TX" "Texas")
	      '("UT" "Utah") '("VT" "Vermont") '("VA" "Virginia")
	      '("VI" "Virgin Islands") '("WA" "Washington") '("WV" "West Virginia")
	      '("WI" "Wisconsin") '("WY" "Wyoming")))

;;;; ===================================================================
;;;; Utilities
;;;; ===================================================================
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

(defun princ-unless-nil (obj &key (fl nil))
  (unless (null obj)
    (princ obj)
    (if fl
	(fresh-line))))

(defun prepend-newline (string)
  (unless (empty-string-p string)
    (format nil "~%~A" string)))

(defun print-logged-qsos (term)
  (let ((q (grep-his-call term)))
    (when q
      (format t "~%QSOs:~%~A" q))))

(defun get-country-translations ()
  (with-transaction (:database *qrz-db*)
    (let ((result (make-array (1+ (first (SELECT [max[id]] :FROM 'countries :flatp t :database *qrz-db*)))
			      :initial-element nil)))
      (setf (svref result 0) "Unknown country")
      (do-query ((id name) "SELECT id, name FROM countries" :database *qrz-db*)
	(setf (svref result id) (string-capitalize name)))
      result)))

;;;; ===================================================================
;;;; Public interface
;;;; ===================================================================
(defun has-offlinedb-p ()
  (and (has-config-p "qrz.offlinedb") (probe-file (get-config "qrz.offlinedb"))))

(defun offline-qrz-search (call)
  "Search for a callsign in the QRZ.com offline database, if installed.
Returns a list of (\"Lastname, Firstname\" \"Street\" \"City\" \"State\" \"Country\").
Returns nil if there is no offline database."
  (flet ((u (x) (string-upcase x))
	 (c (x) (string-capitalize x)))
    (declare (inline u c))
    (when (has-offlinedb-p)
      (with-qrz-db ((get-config "qrz.offlinedb"))
       (let ((r (first (select 'qrz-record :where [= 'call (u call)] :limit 1
			       :caching nil :flatp t :database *qrz-db*))))
	 (when r
	   (list
	    (format nil "~A, ~A" (u (qrz-lastname r)) (c (qrz-firstname r)))
	    (c (qrz-mailstreet r))
	    (c (qrz-mailcity r))
	    (if (= (qrz-country-id r) 271)
		(format nil "~A (~A)" (gethash (qrz-mailstate r) *us-state-code->name*) (qrz-mailstate r))
		(c (qrz-mailstate r)))
	    (qrz-country-name r))))))))

;;;; ===================================================================
;;;; Online searching
;;;; ===================================================================
(defun section-qsl (call result)
  (declare (ignore call))
  (flet ((r (key) (gethash key result)))
    (with-output-to-string (*standard-output*)
      (when-let ((qsl-via (r "qslmgr")))
	(format t "QSL Via     : ~A~%" qsl-via))
      (princ "QSL Methods : ")
      (princ (join (remove-if #'null (list (if (equal (r "mqsl") "1") "Direct")
					   (if (equal (r "eqsl") "1") "eQSL")
					   (if (equal (r "lotw") "1") "LoTW")))
		   ", "))
      (fresh-line))))

(defun section-entity (call &optional result)
  (declare (ignore result))
  (with-output-to-string (*standard-output*)
    (when-let ((e (get-entity call)))
      (format t "Entity      : ~A~%" (entity-name e)))))

(defun section-qth (call result)
  (declare (ignore call))
  (flet ((r (key) (gethash key result)))
    (with-output-to-string (*standard-output*)
      (when-let ((lat (r "lat"))
		 (lon (r "lon")))
	(format t "Lat/Long    : ~A, ~A~&" lat lon))
      (when-let ((grid (r "grid")))
	(format t "Grid        : ~A~&" grid))
      (when (has-config-p "user.grid")
	(let ((target (r "grid")))
	  (when (and (r "lat") (r "long"))
	    (with-safe-io-syntax
		(setf target (list (read-from-string (r "lat"))
				   (read-from-string (r "long"))))))
	  (when target
	    (multiple-value-bind (sp lp) (great-circle-heading (get-config "user.grid") target)
	      (format t "Heading (SP): ~D (LP): ~D~&" (round sp) (round lp))))))
      (when-let ((iota (r "iota")))
	(format t "IOTA        : ~A~&" iota)))))

(defun online-qrz-search (call)
  (with-output-to-string (*standard-output*)
    (let* ((client (make-instance 'qrzcom-client
				  :username (get-config "qrz.user")
				  :password (get-config "qrz.password")))
	   (result (details-by-call client call)))
      (if result
	  (flet ((r (key) (gethash key result)))
	    (format t "~A, ~A~%" (string-upcase (r "name")) (string-capitalize (r "fname")))
	    (format t "~A~&~A~&" (string-capitalize (r "addr1"))
		    (string-capitalize (r "addr2")))
	    (if (and (r "ccode") (= (parse-integer (r "ccode")) 271))
		(format t "~A (~A)~&" (gethash (r "state") *us-state-code->name*) (r "state"))
		(princ-unless-nil (r "state")))
	    (format t "~&~A~%" (string-upcase (r "country")))
	    (princ-unless-nil (prepend-newline (section-entity call)))
	    (mapcar #'(lambda (section)
			(princ-unless-nil (prepend-newline (funcall section call result))))
		    (list #'section-qth #'section-qsl)))
	  (princ-unless-nil (prepend-newline (section-entity call)))))))

(defun qrz-search (call)
  (when (and (has-config-p "qrz.user") (has-config-p "qrz.password"))
    (let ((client (make-instance 'qrzcom-client
				 :username (get-config "qrz.user")
				 :password (get-config "qrz.password"))))
      (details-by-call client call))))

(defun raw-online-qrz-search (call)
  (let* ((client (make-instance 'qrzcom-client
				:username (get-config "qrz.user")
				:password (get-config "qrz.password")))
	 (result (details-by-call client call)))
    (if result
	(loop for k being each hash-key of result using (hash-value v)
	     do (format t "~A:~A~A~%" k #\Tab v)))
    result))

;;;; ===================================================================
;;;; Program wrapper
;;;; ===================================================================
(defun process-options (argv)
  (multiple-value-bind (leftover options)
      (getopt:getopt argv '(("offline" :none) ("raw" :none)))
    (if (third leftover)
	(concatenate 'list options `(("sought" . ,(third leftover))))
	options)))

(defun buildapp-init ()
  (load-entity-information))

(define-galosh-command galosh-qrz ()
  (let* ((options (process-options argv))
	 (sought (cdr (assoc "sought" options))))
    (if sought
	(cond ((assoc "offline" options)
	       (if (has-offlinedb-p)
		   (progn (princ (join (offline-qrz-search sought) #\Newline))
			  (fresh-line)
			  (princ (prepend-newline (section-entity sought)))
			  (print-logged-qsos sought))
		   (progn (say "Offline use of galosh qrz requires that you download and install the qrz.com")
			  (say "database.  See 'man galosh-qrz' or 'info galosh' for more information."))))
	      ((assoc "raw" options)
	       (raw-online-qrz-search sought))
	      (t (handler-case
		     (check-required-config '("qrz.user" "qrz.password"))
		   (missing-mandatory-configuration-error (e)
		     (format t "~&~A~&" (gl::text e))
		     (terminate)))
		 (princ (online-qrz-search sought))
		 (print-logged-qsos sought)))
	(format *error-output* "Fatal: No call given.~%"))))

