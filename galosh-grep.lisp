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

(defpackage :galosh-grep
  (:use :cl :gl :clsql :galosh-qso)
  (:export :grep))
(in-package :galosh-grep)

(clsql:file-enable-sql-reader-syntax)

(defun grep (term)
  (let ((liked-term (cats "%" term "%")))
    (do-query ((qso) [SELECT 'qso :WHERE [or
	       [like 'hiscall liked-term]
	       [like 'comment liked-term]
	       [like 'his_iota liked-term]]]
	       :database *galosh-db*)
      (princ (as-string qso)))))

(define-galosh-command galosh-grep ()
  (dolist (sought (cddr argv))
    (grep sought)))
