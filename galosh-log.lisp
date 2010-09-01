#!/usr/bin/sbcl --script

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

(require 'cl-ncurses)
(require 'clsql)

(load "qso.lisp")
(load "galosh-utils.lisp")
(load "galosh-lisp.lisp")

(defpackage :galosh-logger 
  (:use :cl :gl :clsql-user
	:galosh-qso :gu :cl-ncurses))
(in-package :galosh-logger)

(clsql:enable-sql-reader-syntax) 

(defvar *qrg*     14260000)
(defvar *mode*    "SSB")
(defvar *history* ())
(defparameter *history-size* 15)

(defun log-date-time ()
  (multiple-value-bind
        (second minute hour date month year)
      (decode-universal-time (get-universal-time ) 0)
    (values
     (format nil "~2,'0d~2,'0d~2,'0d" year month date)
     (format nil "~2,'0d~2,'02,'0d~2,'0d" hour minute second))))

(defun log-date ()
  (multiple-value-bind (date) (log-date-time)
    date))

(defun log-time ()
  (multiple-value-bind (date time) (log-date-time)
    (declare (ignore date))
    time))

(defun print-buffer (buffer &optional (prompt ""))
  (mvprintw (- *LINES* 1) 0 (format nil "~a~a~%" prompt buffer))
  (refresh))

(defun print-history ()
  (let ((q (reverse (select 'qso 
			    :order-by '(([qso_date] :desc)([time_on] :desc)) 
			    :limit *history-size* 
			    :caching nil
			    :flatp t))))
    (dotimes (i *history-size*)
      (mvprintw (- *LINES* (- (1+ *history-size*) i)) 0 (format nil "~a~%" (as-string (elt q i))))))
  (refresh))

(defun ensure-valid-rst (val)
  (cond ((stringp val) (parse-integer val))
	((integerp val) val)
	(t (default-rst-for-mode *mode*))))

(defun print-qso (q)
  (with-qso-accessors q
    (mvprintw 1 0 (format nil "~a ~a ~a ~a ~a ~a~%"
			  q-qso-date q-time-on q-hiscall
			  q-qrg      q-rx-rst  q-tx-rst))
    (mvprintw 2 4 (format nil "IOTA: ~a MODE: ~a GRID: ~a~%" q-his-iota q-mode q-his-grid))
    (mvprintw 3 4 (format nil "Name: ~a~%" q-name))
    (mvprintw 4 4 (format nil "Comment: ~a~%" q-comment))
    (mvprintw 5 4 (format nil "Follow up? ~a~%" q-followup))
    (refresh)))

(defun display-status-bar ()
  (mvprintw 0 0 (format nil "QRG: ~10a Mode: ~a~%" *qrg* *mode*)))

(defun prompt (p)
  (mvprintw (1- *LINES*) 0 (format nil "~a~%" p))
  (refresh))

(defun drop-last (str)
  (if (> (length str) 1)
      (subseq str 0 (- (length str) 1))
      ""))

(defun read-value (&key prompt ucase-p value-required-p (buffer ""))
  (default buffer "")
  (labels ((optional-ucase (c) (if ucase-p
				   (string-upcase (string c))
				   (string c)))
	   (r (buffer)
	     (progn (print-buffer buffer prompt)
		    (let* ((raw-code (getch))
			   (c (code-char raw-code)))
		      (cond ((eql c #\Newline)
			     (if (string-empty-p buffer)
				 (if value-required-p
				     (r buffer)
				     nil)
				 buffer))
			    ((eql c #\Tab)
			     (r (concatenate 'string buffer (string #\Space))))
			    ((eql c #\Rubout)
			     (r (drop-last buffer)))
			    ((eql raw-code 23) ; ^W
			     (r (kill-last-word buffer)))
			    (t
			     (let ((b (concatenate 'string buffer (optional-ucase c))))
			       (r b))))))))
    (r buffer)))

(defmacro case-with-char (char value &body body)
  (with-gensyms (c val)
    (let ((acc '(cond)))
      (dolist (form body)
	(case (car form)
	  ((:func) 
	   (setf acc (append acc `(((char= ,c ,(second form))
				    (funcall ,(third form))
				    (option-mode-loop ,val))))))
	  ((:exit)
	   (setf acc (append acc `(((char= ,c ,(second form)) ,(third form))))))
	  ((:default) 
	   (setf acc (append acc `((t ,(second form))))))
	  (otherwise
	   (let ((accessor-name (second form))
		 (read-value-options (fourth form)))
	     (setf acc (append acc `(((char= ,c ,(first form))
				      (setf (,accessor-name ,val) (read-value :prompt ,(third form)
									      :buffer (,accessor-name ,val)
									      ,@read-value-options))
				      (option-mode-loop ,val)))))))))
      (append `(let ((,c ,char)
		     (,val ,value)))
	      (list acc)))))

    
(defun option-mode-loop (qso)
  (print-qso qso)
  (prompt "> ")
  (case-with-char (code-char (getch)) qso
		  (#\g q-his-grid "Grid: ")
		  (#\i q-his-iota "IOTA: " (:ucase-p t))
		  (#\c q-comment  "Comment: ")
		  (#\C q-hiscall  "Call: " (:ucase-p t :value-required-p t))
		  (#\n q-name     "Name: ")
		  (:func #\f #'(lambda () (q-toggle-followup qso)))
		  (:exit #\Newline qso)
		  (:exit #\Esc :cancel)
		  (:default (option-mode-loop qso))))

(defun display-qso-and-prompt-for-options (q)
  (let ((qso (option-mode-loop q)))
    (when (eq (class-of qso) (find-class 'qso))
      (setf (q-time-off qso) (log-time))
      (update-records-from-instance qso)
      (print-history)
      qso)))

(defun process-entry (buffer)
  (destructuring-bind (call &optional rx-rst tx-rst) (split-words buffer)
    (if (sane-callsign-p call)
	(let ((q (make-instance 'qso
				:operator "VP8DMH"
				:hiscall  call
				:qso-date (log-date)
				:time-on  (log-time)
				:mode *mode*
				:qrg *qrg*
				:tx-rst (ensure-valid-rst tx-rst)
				:rx-rst (ensure-valid-rst rx-rst))))
	  (display-qso-and-prompt-for-options q)))))

(defun galosh-set (string)
  (let* ((tokens (split-words string))
	 (place (second tokens))
	 (value (third tokens)))
    (cond ((string-equal place "qrg") (setf *qrg* (parse-integer value :junk-allowed t)))
	  ((string-equal place "mode") (setf *mode* (string-upcase value))))))

(defun process-option (string)
  (let ((verb (first (split-words string))))
    (cond
      ((string-equal verb "set") (galosh-set string))
      ((string-equal verb "q") nil)
      (t t))))

(defun event-loop (buffer)
  (display-status-bar)
  (print-buffer buffer)
  (refresh)
  (let* ((raw-code (getch))
	 (c (code-char raw-code)))
    (cond ((eql c #\:)
	   (if (process-option (read-value :prompt ":"))
	       (event-loop "")))
          ((eql c #\Newline)
	   (process-entry buffer)
	   (event-loop ""))
	  ((eql c #\Tab)
	   (event-loop (concatenate 'string buffer (string #\Space))))
          ((eql c #\Rubout)
	   (event-loop (drop-last buffer)))
	  ((eql raw-code 23) ; ^W
	   (event-loop (kill-last-word buffer)))
          (t
	   (let ((b (concatenate 'string buffer (string-upcase (string c)))))
	     (event-loop b))))))

(defun start-interface ()
  (initscr)
  ;(raw) ; Get everything, including ^C, ^Z, etc
  (cbreak) ; Mainly cooked, but don't get ^C, ^Z, etc.
  (noecho)
  (start-color)
  (print-history)
  (event-loop ""))

(defun main ()
  (unwind-protect
       (progn
	 (connect '("log.db") :database-type :sqlite3)
	 (start-interface))
    (progn
      (endwin)
      (disconnect))))

(main)
