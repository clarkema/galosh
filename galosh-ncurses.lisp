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

;;;; Modifications to cl-ncurses to make it a bit more LISP-y
;;;; Requires galosh-lisp.

(in-package :cl-ncurses)

(defconstant +ctrl-w+ (code-char 23))
(defconstant +resize+ (code-char 410))

(defmacro with-attr (attr &body body)
  (alexandria:with-gensyms (attribute)
  `(let ((,attribute ,attr))
     (unwind-protect
	(progn
	  (attron ,attribute)
	  ,@body)
     (attroff ,attribute)))))

(defmacro with-color (color &body body)
  `(with-attr (color-pair ,color)
     ,@body))

(defun read-value (&key prompt ucase-p capitalize-p value-required-p integer-p (buffer ""))
  (labels ((optional-ucase (c) (if ucase-p
				   (string-upcase (string c))
				   (string c)))
	   (optional-capitalize (s) (if capitalize-p (string-capitalize s) s))
	   (optional-integer (s) (if integer-p (parse-integer s :junk-allowed t) s))
	   (r (buffer)
	     (funcall prompt buffer)
	     (let ((c (code-char (getch))))
	       (gl:given c #'equal
		 (#\Newline
		  (if (gl:empty-string-p buffer)
		      (if value-required-p
			  (r buffer)
			  nil)
		      (optional-integer (optional-capitalize buffer))))
		 (#\Tab (r (gl:mkstr buffer #\Space)))
		 (#\Rubout (r (gl:drop-last buffer)))
		 (+ctrl-w+ (r (gl:kill-last-word buffer)))
		 (t (r (gl:mkstr buffer (optional-ucase c))))))))
    (r buffer)))

(export '(+ctrl-w+ +resize+ with-attr with-color read-value))
