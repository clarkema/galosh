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

(defpackage :galosh-init-db
  (:use :cl :galosh-lisp :clsql :galosh-qso :galosh-journal-entry))
(in-package :galosh-init-db)

(defun prompt-for-permission (dbfile)
  (format t "Database ~a already exists: really reinitialize it? This cannot be undone. [y/N] "
	  dbfile))

(defun init-db (dbfile)
  (unwind-protect
       (progn
	 (connect (list dbfile) :database-type :sqlite3)
	 (truncate-database)
	 (create-view-from-class 'qso)
	 (create-view-from-class 'journal-entry))
    (disconnect)))

(defun main (argv)
  (declare (ignore argv))
  (let ((dbfile (get-config "core.log")))
    (if (probe-file dbfile)
	(if (y-or-n-p (concatenate 'string
				   "Database ~a already exists! Really reinitialize it?~%"
				   "This will cause any log entries in the database to be lost,~%"
				   "and cannot be undone.") dbfile)
	    (progn
	      (format t "Reinitializing ~a.~%" dbfile)
	      (init-db dbfile))
	    (format t "Leaving ~a untouched.~%" dbfile))
	(progn
	  (format t "Creating new ~a.~%" dbfile)
	  (init-db dbfile)))))
