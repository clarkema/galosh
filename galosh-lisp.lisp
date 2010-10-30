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
  (:use :cl :clsql :py-configparser :alexandria)
  (:export :*galosh-db*
	   :split
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
	   :with-galosh-db
	   :define-galosh-command
	   :get-galosh-dir
	   :fatal-get-galosh-dir
	   :missing-galosh-db-error
	   :missing-galosh-dir-error
	   :qrg->band
	   :human-date
	   :get-config
	   :check-required-config
	   :missing-mandatory-configuration-error))

(in-package :galosh-lisp)

(defvar *galosh-db* nil)

(defparameter *non-tone-modes* '("SSB" "ESSB" "AM"))
(defparameter *default-tone-mode-rst* 599)
(defparameter *default-non-tone-mode-rst* 59)

(defparameter *short-month-names*
  '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sept" "Oct" "Nov" "Dec"))

(defmacro split (sep seq)
  `(split-sequence:split-sequence ,sep ,seq))

(defun split-words (seq &key (first nil))
  (let ((options (reverse (list :remove-empty-subseqs t))))
    (when first
      (setf options (nconc (reverse (list :count first)) options)))
    (apply #'split-sequence:split-sequence  #\Space
	   (string-trim '(#\Space #\Tab #\Newline) seq)
	   (nreverse options))))

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

(define-condition missing-galosh-db-error (error)
  ((text :initarg :text :reader text)))

(defmacro with-galosh-db ((db &key (make-default t)) &body body)
  (with-gensyms (dbfile default-p)
    `(let ((,dbfile ,db)
	   (,default-p ,make-default))
       (if *galosh-db*
	   (progn
	     (if ,default-p
		 (setf *default-database* *galosh-db*))
	     ,@body)
	   (if (probe-file ,dbfile)
	       (unwind-protect
		    (progn
		      (setf *galosh-db* (connect (list ,dbfile) :database-type :sqlite3 :make-default ,default-p
						 :if-exists :old))
		      ,@body)
		 (disconnect :database *galosh-db*)
		 (setf *galosh-db* nil))
	       (error 'missing-galosh-db-error :text
		      (format nil "Could not find database `~a'." ,dbfile)))))))


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

(defun human-date (date)
  (if (= (length date) 8)
      (progn
	(format nil "~A-~A-~A"
		(subseq date 6 8)
		(nth (- (parse-integer date :start 4 :end 6) 1) *short-month-names*)
		(subseq date 0 4)))))

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

(define-condition missing-mandatory-configuration-error (error)
  ((text :initarg :text :reader text)))

(defvar *config* nil)

(defun set-defaults (config)
  (with-input-from-string (s (join
			      (list "[core]"
				    (cats "log = " (namestring (merge-pathnames "log.db" (fatal-get-galosh-dir)))))
			      #\Newline))
    (setf config (read-stream config s))))

(defun check-required-config (variables)
  (dolist (ropt variables)
    (destructuring-bind (section option) (split #\. ropt)
      (unless (and (has-section-p *config* section)
		   (has-option-p *config* section option))
	(error 'missing-mandatory-configuration-error :text
	       (format nil "Please update your configuration to provide a value for `~a'." ropt))))))

(defun get-config (name)
  (unless *config* (init-config))
  (destructuring-bind (section option) (split #\. name)
    (coerce (get-option *config* section option) 'simple-string)))

(defun init-config ()
  (let ((global-config (merge-pathnames (make-pathname :directory '(:relative ".galosh") :name "config") (user-homedir-pathname)))
	(repository-config (make-pathname :directory (fatal-get-galosh-dir) :name "config")))
    (setf *config* (set-defaults (make-config)))
    (read-files *config* (list global-config repository-config))))

(defmacro define-galosh-command (name (&key (required-configuration nil)) &body body)
  (with-gensyms (req-config)
    `(defun ,(intern "MAIN" (symbol-name name)) (,(intern "ARGV" (symbol-name name)))
       (let ((,req-config ,required-configuration))
	 (init-config)
	 (handler-case
	     (check-required-config ,req-config)
	   (missing-mandatory-configuration-error (e)
	     (format t "~&~A~&" (text e))
	     (sb-ext:quit)))
	 (with-galosh-db ((get-config "core.log"))
	   ,@body)))))
