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

(require 'split-sequence)
(defpackage :galosh-lisp
  (:nicknames :gl)
  (:use :cl)
  (:export :split
	   :split-words
	   :default-rst-for-mode
	   :valid-callsign-char-p
	   :sane-callsign-p
	   :empty-string-p
	   :string-right-pad
	   :cats
	   :join
	   :say
	   :keys
	   :default
	   :with-gensyms
	   :with-galosh-db
	   :get-galosh-dir
	   :fatal-get-galosh-dir
	   :missing-galosh-db-error
	   :missing-galosh-dir-error
	   :qrg->band))

(in-package :galosh-lisp)

(defparameter *non-tone-modes* '("SSB" "ESSB" "AM"))
(defparameter *default-tone-mode-rst* 599)
(defparameter *default-non-tone-mode-rst* 59)

(defmacro split (sep seq)
  `(split-sequence:split-sequence ,sep ,seq))

(defmacro split-words (seq)
  `(split-sequence:split-sequence #\Space
				  (string-trim '(#\Space #\Tab #\Newline) ,seq)
				  :remove-empty-subseqs t))

(defun empty-string-p (str)
  (not (> (length str) 0)))

(defun string-right-pad (target-width str &key (padding-element #\Space))
  (concatenate 'string str (make-string (- target-width (length str)) :initial-element padding-element)))

(defmacro cats (&rest strings)
  `(concatenate 'string ,@strings))

(defun join (strings &optional (separator ""))
  (let ((sep (string separator)))
    (with-output-to-string (out)
      (loop for (string . more?) on strings
	 do (write-string string out)
	 when more? do (write-string sep out)))))

(defun say (obj &optional (stream *standard-output*))
  (princ obj stream)
  (fresh-line stream))

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

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
		     `(,s (gensym)))
		 syms)
     ,@body))

(define-condition missing-galosh-db-error (error)
  ((text :initarg :text :reader text)))

(defmacro with-galosh-db (db &body body)
  (with-gensyms (dbfile)
    `(let ((,dbfile ,db))
       (if (probe-file ,dbfile)
	   (unwind-protect
		(progn
		  (connect (list ,dbfile) :database-type :sqlite3)
		  ,@body)
	     (disconnect))
	   (error 'missing-galosh-db-error :text
		  (format nil "Could not find database `~a'." ,dbfile))))))

(define-condition missing-galosh-dir-error (error)
  ((text :initarg :text :reader text)))

(defun get-galosh-dir ()
  (let ((gdir (sb-ext:posix-getenv "GALOSH_DIR")))
    (if gdir
	gdir
	(error 'missing-galosh-dir-error :text
	       "GALOSH_DIR is not defined."))))


(defun fatal-get-galosh-dir ()
  (handler-case
      (get-galosh-dir)
    (missing-galosh-dir-error ()
      (format t "GALOSH_DIR is not defined.~%" )
      (sb-ext:quit))))

(defun qrg->band (qrg)
  (labels ((between (lower upper)
	     (and (>= qrg lower) (<= qrg upper))))
    (cond ((< qrg 1800000) "DC")
          ((between 1800000 2000000) "160m")
	  ((between 3500000 4000000) "80m")
	  ((between 5330500 5403500) "60m")
	  ((between 7000000 7400000) "40m")
	  ((between 10100000 10150000) "30m")
	  ((between 14000000 14350000) "20m")
	  ((between 18068000 18168000) "17m")
	  ((between 21000000 21450000) "15m")
	  ((between 24890000 24990000) "12m")
	  ((between 28000000 29700000) "10m")
	  ((between 50000000 54000000) "50m")
	  ((between 144000000 148000000) "2m")
	  ((between 219000000 225000000) "1.25m")
	  ((between 420000000 450000000) "70cm")
	  ((between 902000000 928000000) "33cm")
	  ((between 1240000000 1300000000) "23cm")
	  ((> qrg 1300000000) "Daylight")
	  (t "Unknown band"))))
