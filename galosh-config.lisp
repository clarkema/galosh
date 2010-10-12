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

(defpackage :galosh-config
  (:use :cl :gl :py-configparser)
  (:export :get-config
	   :missing-mandatory-configuration-error))
(in-package :galosh-config)

(define-condition missing-mandatory-configuration-error (error)
  ((text :initarg :text :reader text)))

(defvar *config* nil)
(defparameter *required* (list "user.call"
			       "user.name"
			       "user.email"))

(defun set-defaults (config)
  (with-input-from-string (s (join
			      (list "[core]"
				    (cats "log = " (namestring (merge-pathnames "log.db" (fatal-get-galosh-dir)))))
			      #\Newline))
    (setf config (read-stream config s))))

(defun check-required (config)
  (dolist (ropt *required*)
    (destructuring-bind (section option) (split #\. ropt)
      (unless (has-option-p *config* section option)
	(error 'missing-mandatory-configuration-error :text
	       "Please update your configuration to provide a value for `~a'." ropt)))))

(defun get-config (name)
  (unless *config* (init))
  (destructuring-bind (section option) (split #\. name)
    (coerce (get-option *config* section option) 'simple-string)))

(defun init ()
  (setf *config* (set-defaults (make-config)))
  (read-files *config* (list
			(make-pathname :directory (fatal-get-galosh-dir) :name "config")))
  (check-required *config*))

(defun main (argv)
  (let ((sought (third argv)))
    (if sought
	(say (get-config sought))
	(progn
	  (init)
	  (dolist (section (sections *config*))
	    (dolist (option (options *config* section))
	      (format t "~a.~a = ~a~%" section option (get-option *config* section option))))))))