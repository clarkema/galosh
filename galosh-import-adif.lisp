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
(load "galosh-adif.lisp")

(use-package :galosh-qso)
(use-package :clsql-user)
(use-package :galosh-adif)

(defvar *comment-prefix* nil)

(defun ensure-defaults (qso)
  (if (null (q-operator qso))
      (setf (q-operator qso) "VP8DMH"))
  (if (null (q-comment qso))
      (setf (q-comment qso) *comment-prefix*))
  (if (q-his-iota qso)
      (setf (q-followup qso) 1)))

(defun wrap-comment (string)
  (if *comment-prefix*
      (if (> 0 (length string))
	  (format nil "~a~%~%~a" *comment-prefix* string)
	  *comment-prefix*)
      string))

(defun process-options ()
  (multiple-value-bind (leftover options)
      (getopt:getopt *posix-argv* '(("comment-prefix" :required)))
    (if (< (length leftover) 2)
	(error "Error: please specify input file.")
	(concatenate 'list options `((filename . ,(second leftover)))))))

(defun process-file (path)
  (with-open-file (stream path)
    (unwind-protect
	 (progn
	   (connect '("log.db") :database-type :sqlite3)
	   (map-over-qsos #'(lambda (q)
			      (ensure-defaults q)
			      (update-records-from-instance q)
			      (format t "~a" (as-string q)))
			  stream))
      (disconnect))))

(defun main ()
  (let (options filename)
    (handler-case
	(progn
	  (setf options (process-options)
		filename (cdr (assoc 'filename options))
		*comment-prefix* (cdr (assoc "comment-prefix" options :test #'string=)))
	  (process-file filename))
      (file-error () (format t "Error: file `~a' does not exist!~%" filename))
      (simple-error (e) (format t "~a~%" e))
      (adif-error (e) (format t "Parse error: ~a ~a on line ~s.~%"
			      (adif-error-message e)
			      (adif-error-value e)
			      (adif-error-line-number e))))))

(main)
