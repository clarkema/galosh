;;;; galosh -- amateur radio utilities.
;;;; Copyright (C) 2012 Michael Clarke, M0PRL
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

(defpackage #:galosh-bootstrap
  (:use #:cl)
  (:export #:boot
	   #:*main-package-name*))
(in-package #:galosh-bootstrap)

(defvar *main-package-name*)

(defun find-main (package)
  (symbol-function
   (find-symbol "MAIN"
		(find-package (string-upcase package)))))

(defun boot ()
  (ql-impl-util:call-with-quiet-compilation
   (lambda ()
     (let ((*standard-output* (make-broadcast-stream))
	   (*error-output* (make-broadcast-stream)))
       (ql:quickload *main-package-name*))
     (funcall (find-main *main-package-name*) sb-ext:*posix-argv*))))