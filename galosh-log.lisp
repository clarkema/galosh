;;;; galosh -- amateur radio utilities.
;;;; Copyright (C) 2010, 2011 Michael Clarke, M0PRL
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

(defpackage :galosh-log
  (:use :cl :gl :clsql
	:galosh-qso :gu :cl-ncurses :alexandria))
(in-package :galosh-log)

(clsql:enable-sql-reader-syntax) 

(defvar *operator* nil)
(defvar *qrg*     14260000)
(defvar *mode*    "SSB")
(defvar *iota*    nil)

(defconstant +inv-green+ 1)

;;; ===================================================================
;;; Utilities
;;; ===================================================================
(defun log-date-time ()
  (multiple-value-bind
        (second minute hour date month year)
      (decode-universal-time (+ (get-universal-time ) (get-time-fudge)) 0)
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

(defun ensure-valid-rst (val mode)
  (cond ((stringp val)
	 (let ((rst (princ-to-string (default-rst-for-mode mode))))
	   (if (= (length val) 1)
	       (setf (subseq rst 1 2) val)
	       (setf (subseq rst 0 (length val)) val))
	   (parse-integer rst)))
	((integerp val) val)
	(t (default-rst-for-mode mode))))

(defun drop-last (str)
  (if (> (length str) 1)
      (subseq str 0 (- (length str) 1))
      ""))

;;; ===================================================================
;;; Display functions
;;; ===================================================================
(defun print-buffer (buffer &optional (prompt ""))
  (mvprintw (- *LINES* 1) 0 (format nil "~a~a~%" prompt buffer))
  (refresh))

(defun print-history ()
  (let* ((history-size (- *LINES* 9)) ; 9 = 5 for qso, 3 status bars, 1 entry line
	 (q (reverse (select 'qso
			     :order-by '(([qso_date] :desc)([time_on] :desc))
			     :limit history-size
			     :caching nil
			     :flatp t)))
	 (q-length (length q)))
    (dotimes (i q-length)
      (mvprintw (- *LINES* (- (+ history-size 2) i)) 0 (format nil "~a~%" (as-string (elt q i))))))
  (refresh))

(defun print-qso (q)
  (with-qso-accessors q
    (mvprintw 1 0 (string-right-pad 40 (format nil "~a ~a ~a ~a ~a ~a"
					       (subseq q-qso-date 2) (subseq q-time-on 0 4) q-his-call
					       q-qrg      q-rx-rst  q-tx-rst)))
    (mvprintw 2 0 (string-right-pad 40 (format nil "    IOTA: ~6@A  MODE: ~A"
					       (n->es q-his-iota)
					       (n->es q-mode))))
    (mvprintw 3 0 (string-right-pad 40 (format nil "    Name: ~a" (n->es q-his-name))))
    (mvprintw 4 0 (string-right-pad 40 (format nil "    Comment: ~a" (n->es q-comment))))
    (mvprintw 5 0 (string-right-pad 40 (format nil "    Follow up? ~A  GRID: ~6@A" q-followup (n->es q-his-grid))))
    (with-color +inv-green+
      (mvprintw 6 0 (string-right-pad *COLS* " g:grid i:iota c:comment C:call n:name f:followup")))
    (refresh)))

(defun print-qrz-info (call)
  (when (galosh-qrz:has-offlinedb-p)
    (let ((i 1))
      (dolist (line (galosh-qrz:offline-qrz-search call))
	(mvprintw i 40 (format nil "~A~%" line))
	(setf i (+ i 1))))
    (refresh)))

(defun display-title-bar ()
  (with-color +inv-green+
    (mvprintw 0 0 (string-right-pad *COLS* (format nil " QRG: ~10a Mode: ~a IOTA: ~a" *qrg* *mode* *iota*)))))

(defun display-status-bar ()
  (with-color +inv-green+
    (mvprintw (- *LINES* 2) 0
	      (string-right-pad *COLS*
				(format nil "---Galosh Logger: ~A" *default-database*)))))

(defun prompt (p)
  (mvprintw (1- *LINES*) 0 (format nil "~a~%" p))
  (refresh))

(defun read-value (&key prompt ucase-p capitalize-p value-required-p (buffer ""))
  (default buffer "")
  (labels ((optional-ucase (c) (if ucase-p
				   (string-upcase (string c))
				   (string c)))
	   (optional-capitalize (s) (if capitalize-p (string-capitalize s) s))
	   (r (buffer)
	     (progn (print-buffer buffer prompt)
		    (let* ((raw-code (getch))
			   (c (code-char raw-code)))
		      (cond ((eql c #\Newline)
			     (if (string-empty-p buffer)
				 (if value-required-p
				     (r buffer)
				     nil)
				 (optional-capitalize buffer)))
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
		  (#\C q-his-call "Call: " (:ucase-p t :value-required-p t))
		  (#\n q-his-name "Name: " (:capitalize-p t))
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

(defun run-full-call-handlers (call)
  (print-qrz-info call))

(defun process-entry (buffer)
  (destructuring-bind (call &optional rx-rst tx-rst) (split-words buffer :first 3)
    (if (sane-callsign-p call)
	(let ((q (make-instance 'qso
				:my-call *operator*
				:his-call call
				:qso-date (log-date)
				:time-on  (log-time)
				:mode *mode*
				:qrg *qrg*
				:my-iota *iota*
				:tx-rst (ensure-valid-rst tx-rst *mode*)
				:rx-rst (ensure-valid-rst rx-rst *mode*))))
	  (run-full-call-handlers call)
	  (print-qrz-info call)
	  (display-qso-and-prompt-for-options q)))))

(defun process-command (string)
  (let ((verb (first (split-words string))))
    (cond
      ((string-equal verb "d") (cmd-delete-qso string))
      ((string-equal verb "set") (cmd-set string))
      ((string-equal verb "q") nil)
      (t t))))

(defun cmd-delete-qso (string)
  (let* ((tokens (split-words string))
	 (id (second tokens)))
    (with-open-file (s (get-config "log.attic")
		       :direction :output
		       :if-exists :append
		       :if-does-not-exist :create)
      (princ (galosh-adif:qso->adif (first (select 'qso :where [= 'id id] :flatp t :caching nil))) s))
    (delete-records :from [qso] :where [= 'id id])
    (print-history))
  t)

(defun cmd-set (string)
  (let* ((tokens (split-words string))
	 (place (second tokens))
	 (value (third tokens)))
    (cond ((string-equal place "qrg") (setf *qrg* (parse-integer value :junk-allowed t)))
	  ((string-equal place "mode") (setf *mode* (string-upcase value)))
	  ((string-equal place "iota") (setf *iota* (string-upcase value))))))

(defun event-loop (buffer)
  (display-title-bar)
  (display-status-bar)
  (print-buffer buffer)
  (refresh)
  (let* ((raw-code (getch))
	 (c (code-char raw-code)))
    (cond ((equal raw-code 410)
	   (display-title-bar)
	   (print-history)
	   (display-status-bar)
	   (print-buffer buffer)
	   (refresh)
	   (event-loop buffer))
	  ((and (eql c #\:) (= (length buffer) 0))
	   (if (process-command (read-value :prompt ":"))
	       (event-loop "")))
          ((eql c #\Newline)
	   (unless (string-empty-p buffer)
	     (process-entry buffer))
	   (event-loop ""))
	  ((member c '(#\Tab #\Space))
	   (when (= (length (split-words buffer)) 1)
	     (run-full-call-handlers buffer))
	   (event-loop (cats buffer (string #\Space))))
          ((eql c #\Rubout)
	   (event-loop (drop-last buffer)))
	  ((eql raw-code 23) ; ^W
	   (event-loop (kill-last-word buffer)))
          (t
	   (let ((b (cats buffer (string-upcase (string c)))))
	     (event-loop b))))))

(defun write-state (path)
  (with-open-file (s path
		     :direction :output
		     :if-does-not-exist :create
		     :if-exists :supersede)
    (with-standard-io-syntax
      (print `(setf *qrg*  ,*qrg*)  s)
      (print `(setf *mode* ,*mode*) s)
      (print `(setf *iota* ,*iota*) s))))

(defun read-state (path)
  (when (probe-file path)
    (load path)))

(defun start-interface ()
  (initscr)
  ;(raw) ; Get everything, including ^C, ^Z, etc
  (cbreak) ; Mainly cooked, but don't get ^C, ^Z, etc.
  (noecho)
  (when (has-colors)
    (start-color)
    (init-pair +inv-green+ COLOR_BLACK COLOR_GREEN))
  (print-history)
  (event-loop ""))

(define-galosh-command galosh-log (:required-configuration '("user.call"))
  (let ((state-file (make-pathname :directory (fatal-get-galosh-dir) :name "galosh-log" :type "state"))
	(*operator* (get-config "user.call")))
    (read-state state-file)
    (unwind-protect
	 (start-interface)
      (endwin)
      (write-state state-file))))
