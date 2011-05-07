;;;; galosh -- amateur radio utilities.
;;;; Copyright (C) 2010 Michael Clarke, M0PRL
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

(export '(with-attr with-color))
