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
	:galosh-qso :cl-ncurses :alexandria))
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

(defun ensure-valid-rst (val mode)
  (cond ((stringp val)
	 (let ((rst (princ-to-string (default-rst-for-mode mode))))
	   (if (= (length val) 1)
	       (setf (subseq rst 1 2) val)
	       (setf (subseq rst 0 (length val)) val))
	   (parse-integer rst)))
	((integerp val) val)
	(t (default-rst-for-mode mode))))

;;; ===================================================================
;;; Display functions
;;; ===================================================================
(defun print-skeleton ()
  (display-title-bar)
  (display-status-bar)
  (display-help-bar)
  (refresh))

(defun print-buffer (buffer &optional (prompt ""))
  (mvprintw (- *LINES* 1) 0 (format nil "~a~a~%" prompt buffer))
  (refresh))

(defun print-history ()
  (let* ((history-size (- *LINES* 9)) ; 9 = 5 for qso, 3 status bars, 1 entry line
	 (history-start 7)
	 (q (reverse (select 'qso
			     :order-by '(([qso_date] :desc)([time_on] :desc))
			     :limit history-size
			     :caching nil
			     :flatp t)))
	 (q-length (length q)))
    (dotimes (i history-size)
      (mvprintw (+ history-start i) 0 (format nil "~a~%" (if (< i q-length) (as-string (elt q i)) "")))))
  (refresh))

(defun print-qso (q)
  (with-qso-accessors q
    (mvprintw 1 0 (string-right-pad 40 (format nil "~a ~a ~a ~a ~a ~a"
					       (subseq q-qso-date 2) (subseq q-time-on 0 4) q-his-call
					       q-qrg      q-rx-rst  q-tx-rst)))
    (mvprintw 2 0 (string-right-pad 40 (format nil "    IOTA: ~6@A  Mode: ~A"
					       (n->es q-his-iota)
					       (n->es q-mode))))
    (mvprintw 3 0 (string-right-pad 40 (format nil "    Name: ~a" (n->es q-his-name))))
    (mvprintw 4 0 (string-right-pad 40 (format nil "    Comment: ~a" (n->es q-comment))))
    (mvprintw 5 0 (string-right-pad 40 (format nil "    Follow up? ~A  Grid: ~6@A" q-followup (n->es q-his-grid))))
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

(defun display-help-bar ()
  (with-color +inv-green+
    (mvprintw 6 0 (string-right-pad *COLS* " g:grid i:iota c:comment C:call n:name f:followup"))))

(defun prompt (p)
  (mvprintw (1- *LINES*) 0 (format nil "~a~%" p))
  (refresh))

(defun read-value (&key prompt ucase-p capitalize-p value-required-p integer-p (buffer ""))
  (default buffer "")
  (labels ((optional-ucase (c) (if ucase-p
				   (string-upcase (string c))
				   (string c)))
	   (optional-capitalize (s) (if capitalize-p (string-capitalize s) s))
	   (optional-integer (s) (if integer-p (parse-integer s :junk-allowed t) s))
	   (r (buffer)
	     (print-buffer buffer prompt)
	     (let ((c (code-char (getch))))
	       (cond ((eql c #\Newline)
		      (if (empty-string-p buffer)
			  (if value-required-p
			      (r buffer)
			      nil)
			  (optional-integer (optional-capitalize buffer))))
		     ((eql c #\Tab)
		      (r (concatenate 'string buffer (string #\Space))))
		     ((eql c #\Rubout)
		      (r (drop-last buffer)))
		     ((eql c (code-char 23)) ; ^W
		      (r (kill-last-word buffer)))
		     (t
		      (let ((b (mkstr buffer (optional-ucase c))))
			(r b)))))))
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
		  (:func +resize+ #'(lambda ()
				      (print-history)
				      (print-skeleton)))
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
  (given (first (split-words string)) #'string-equal
    ("d"   (cmd-delete-qso string))
    ("set" (cmd-set string))
    ("q"    nil)
    (t t)))

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
  (destructuring-bind (set place value) (split-words string :first 3)
    (declare (ignore set))
    (given place #'string-equal
      ("qrg"  (setf *qrg* (parse-integer value :junk-allowed t)))
      ("mode" (setf *mode* (string-upcase value)))
      ("iota" (setf *iota* (string-upcase value))))))

(defun event-loop (buffer)
  (print-skeleton)
  (print-buffer buffer)
  (refresh)
  (let* ((raw-code (getch))
	 (c (code-char raw-code)))
    (cond ((eql c +resize+)
	   (print-skeleton)
	   (print-history)
	   (print-buffer buffer)
	   (refresh)
	   (event-loop buffer))
	  ((and (eql c #\:) (= (length buffer) 0))
	   (if (process-command (read-value :prompt ":"))
	       (event-loop "")))
          ((eql c #\Newline)
	   (unless (empty-string-p buffer)
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

(defun complete-missing-entities ()
  (with-transaction ()
    (do-query ((qso) [SELECT 'qso :WHERE [null 'his_dxcc]])
      (let ((entity (get-entity (q-his-call qso) (qso-datetime qso) :error-p nil)))
	(setf (q-his-dxcc qso) (entity-adif entity))
	(update-records-from-instance qso)
	(format t "Updated ~A to ~A (~A)~%" (as-string qso) (entity-adif entity) (entity-name entity))))))

(defun process-options (argv)
  (multiple-value-bind (leftover options)
      (getopt:getopt argv '(("complete-missing-entities" :none t)))
    (declare (ignore leftover))
    options))

(defun get-option (name options &key (default nil default-p))
  (if-let ((option (cdr (assoc name options))))
	  option
	  (if default-p default nil)))

(define-galosh-command galosh-log (:required-configuration '("user.call"))
  (let ((options (process-options argv))
	(state-file (make-pathname :directory (fatal-get-galosh-dir) :name "galosh-log" :type "state"))
	(*operator* (get-config "user.call")))
    (cond ((get-option "complete-missing-entities" options)
	   (complete-missing-entities))
	  (t
	   (read-state state-file)
	   (unwind-protect
		(start-interface)
	     (endwin)
	     (write-state state-file))))))