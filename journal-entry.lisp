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

(defpackage :galosh-journal-entry
  (:use :cl :galosh-lisp :clsql)
  (:export :journal-entry :j-id :j-date :j-text))
(in-package :galosh-journal-entry)

(def-view-class journal-entry ()
  ((id
    :type integer
    :db-type "INTEGER" ; Required to make sqlite autoincrement work properly.
    :db-kind :key
    :db-constraints :not-null
    :initarg :id
    :initform nil
    :accessor j-id)
   (journal-date
    :db-type "TEXT"
    :db-constraints :not-null
    :accessor j-date
    :initarg :date)
   (text
    :db-type "TEXT"
    :accessor j-text
    :initarg :text
    :initform nil))
  (:base-table "journal-entries"))
