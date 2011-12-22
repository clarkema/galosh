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

(defpackage :galosh-export-adif
  (:use :cl :galosh-lisp :clsql :galosh-qso :galosh-adif :alexandria))
(in-package :galosh-export-adif)

(clsql:file-enable-sql-reader-syntax)

(defparameter *slot-sets* (make-hash-table :test 'equal))

(define-condition match-syntax-error (error)
  ((message
    :initarg :message
    :accessor match-syntax-error-message
    :initform nil
    :documentation "Human-readable error message.")))

(defmethod print-object ((object match-syntax-error) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (match-syntax-error-message object) stream)))

(defun match-syntax-error (message)
  (error 'match-syntax-error
	 :message message))

(defun add-fieldset (name fields)
  (setf (gethash name *slot-sets*) fields))

(add-fieldset "std" galosh-adif:*standard-slots*)

(defun process-options (argv)
  (multiple-value-bind (leftover options)
      (getopt:getopt argv '(("fieldset" :optional)
			    ("list-fieldsets" :none t)
			    ("match" :optional)))
    (declare (ignore leftover))
    options))

(defun print-header ()
  (say "Exported from Galosh.")
  (format t "<PROGRAMID:~A>~A~%" (length "GALOSH") "GALOSH")
  (format t "<PROGRAMVERSION:~A>~A~%" (length *galosh-version*) *galosh-version*)
  (say "<EOH>"))

(defun compile-query-form (form)
  (case (first form)
    (like `(sql-operation 'or
			  ,@(mapcar (lambda (f) `(sql-operation 'like (sql-expression :attribute ',(second form)) ,f))
				    (cddr form))))
    (= `(sql-operation 'or
		       ,@(mapcar (lambda (f) `(sql-operation '= (sql-expression :attribute ',(second form)) ,f))
				 (cddr form))))
    (or
     `(sql-operation 'or ,@(mapcar (lambda (f) (compile-query-form f)) (cdr form))))
    (and
     `(sql-operation 'and ,@(mapcar (lambda (f) (compile-query-form f)) (cdr form))))
    (not
     `(sql-operation 'not ,@(mapcar (lambda (f) (compile-query-form f)) (cdr form))))
    (:no-match-options
     nil)
    (otherwise
     (match-syntax-error (format nil "Unknown operator: ~A" (first form))))))

(defun get-option (name options &key (default nil default-p))
  (if-let ((option (cdr (assoc name options))))
	  option
	  (if default-p default nil)))

(defmacro generate-query (form)
  `(select 'qso
	   :where ,(compile-query-form form)
	   :order-by '(([qso_date] :desc)([time_on] :desc))
	   :flatp t))

(defun get-qsos-matching (terms)
  (reverse
   (eval `(generate-query ,(let ((*package* (find-package :galosh-export-adif)))
				(read-from-string terms))))))

(define-galosh-command galosh-export-adif ()
  (let* ((options (process-options argv))
	 (set (get-option "fieldset" options :default "std")))

    (if (get-option "list-fieldsets" options)
	(format t "~{~:A~%~}~&all~%" (keys *slot-sets*))

	(handler-case
	    (let ((matching-qsos (get-qsos-matching (get-option "match" options	:default "(:no-match-options)"))))
	      (print-header)
	      (mapcar #'(lambda (x)
			  (princ (qso->adif x :slots (if (string= set "all")
							 :all
							 (gethash set *slot-sets*))))) matching-qsos))
	  (match-syntax-error (e) (format *error-output* "~A~%"
					  (match-syntax-error-message e)))
	  (sql-database-data-error (e) (if (= (sql-error-error-id e) 1)
					   (format *error-output* "Unknown column: ~A~%"
						   (last1 (split-words (sql-error-database-message e)))) ;; Should only occur for invalid columns
					   (format *error-output* "~A~%"
						   (sql-error-database-message e)))))))) ;; Could be other stuff?
