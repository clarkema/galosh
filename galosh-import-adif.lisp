;;;; galosh -- amateur radio utilities.
;;;; Copyright (C) 2010, 2011 Michael Clarke, M0PRL
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

(defpackage :galosh-import-adif
  (:use :cl :gl :clsql :galosh-qso :galosh-adif))
(in-package :galosh-import-adif)

(defvar *comment-prefix* nil)

(defun ensure-defaults (qso)
  (default (q-my-call qso) (get-config "user.call"))
  (default (q-comment qso) *comment-prefix*)
  (default (q-band qso) (qrg->band (q-qrg qso))))

(defun wrap-comment (string)
  (if *comment-prefix*
      (if (> 0 (length string))
	  (format nil "~a~%~%~a" *comment-prefix* string)
	  *comment-prefix*)
      string))

(defun process-options (argv)
  (multiple-value-bind (leftover options)
      (getopt:getopt argv '(("comment-prefix" :required)))
    (if (< (length leftover) 2)
	(error "Error: please specify input file.")
	(concatenate 'list options `((filename . ,(third leftover)))))))

(defun process-file (path)
  (with-open-file (stream path)
    (map-over-qsos #'(lambda (q)
		       (ensure-defaults q)
		       (update-records-from-instance q)
		       (format t "~a" (as-string q)))
		   stream)))

(define-galosh-command galosh-import-adif (:required-configuration '("user.call"))
  (let (options filename)
    (handler-case
	(progn
	  (setf options (process-options argv)
		filename (cdr (assoc 'filename options))
		*comment-prefix* (cdr (assoc "comment-prefix" options :test #'string=)))
	  (process-file filename))
      (file-error () (format t "Error: file `~a' does not exist!~%" filename))
      (simple-error (e) (format t "~a~%" e))
      (adif-error (e) (format t "Parse error: ~a ~a on line ~s.~%"
			      (adif-error-message e)
			      (adif-error-value e)
			      (adif-error-line-number e))))))
