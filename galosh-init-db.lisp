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

(setf *debug-beginner-help-p* nil)

(require :getopt)
(require :clsql)
(load "qso.lisp")

(use-package :galosh-qso)
(use-package :clsql-user)

(defun prompt-for-permission (dbfile)
  (format t "Database ~a already exists: really reinitialize it? This cannot be undone. [y/N] "
	  dbfile))

(defun init-db (dbfile)
  (unwind-protect
       (progn
	 (connect dbfile :database-type :sqlite3)
	 (truncate-database)
	 (create-view-from-class 'qso))
    (disconnect)))

(defun main ()
  (let ((dbfile "log.db"))
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

(main)
