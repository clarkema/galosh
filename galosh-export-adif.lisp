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

(defpackage :galosh-export-adif
  (:use :cl :gl :galosh-config :clsql :galosh-qso :galosh-adif))
(in-package :galosh-export-adif)

(clsql:file-enable-sql-reader-syntax)

(defun main (args)
  (declare (ignore args))
  (with-galosh-db (get-config "core.log")
    (mapcar #'(lambda (x) (princ (qso->adif x)))
	    (reverse (select 'qso :order-by '(([qso_date] :desc)([time_on] :desc)) :flatp t)))))
