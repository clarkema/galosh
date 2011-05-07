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

(defpackage :galosh-utils
  (:nicknames :gu)
  (:use :cl)
  (:export :string-empty-p :kill-last-word))

(in-package :galosh-utils)

(defun string-empty-p (str)
  (not (> (length str) 0)))

(defun kill-last-word (str)
  (let ((index (position #\Space (string-right-trim '(#\Space #\Tab #\Newline) str) :from-end t)))
    (if index
	(values (subseq str 0 (+ index 1)) (subseq str (+ index 1)))
	(values "" nil))))
