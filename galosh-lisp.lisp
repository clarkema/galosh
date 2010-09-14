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

(defpackage :galosh-lisp
  (:nicknames :gl)
  (:use :cl)
  (:export :default-rst-for-mode
	   :valid-callsign-char-p
	   :sane-callsign-p
	   :empty-string-p
	   :string-right-pad
	   :keys
	   :default))

(in-package :galosh-lisp)

(defparameter *non-tone-modes* '("SSB" "ESSB" "AM"))
(defparameter *default-tone-mode-rst* 599)
(defparameter *default-non-tone-mode-rst* 59)

(defun empty-string-p (str)
  (not (> (length str) 0)))

(defun string-right-pad (target-width str &key (padding-element #\Space))
  (concatenate 'string str (make-string (- target-width (length str)) :initial-element padding-element)))

(defmacro default (place value)
  `(if (null ,place)
       (setf ,place ,value)))

(defun keys (hash)
  (loop for k being the hash-keys of hash collect k))

(defun default-rst-for-mode (mode)
  (if (member mode *non-tone-modes* :test #'string=)
      *default-non-tone-mode-rst*
      *default-tone-mode-rst*))

(defun valid-callsign-char-p (char)
  (or (alphanumericp char) (char= char #\/)))
(proclaim '(inline valid-callsign-char-p))

(defun sane-callsign-p (call)
  (if (and (not (empty-string-p call)) (every #'valid-callsign-char-p call))
      call
      nil))
