;;;; galosh -- amateur radio utilities.
;;;; Copyright (C) 2011 Michael Clarke, M0PRL
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

(screamer:define-screamer-package :galosh-qsl
  (:use :cl :gl :clsql
	:galosh-qso :cl-ncurses :alexandria :st-json))
(in-package :galosh-qsl)

(clsql:enable-sql-reader-syntax) 

(defconstant +inv-green+ 1)
(defvar *tagged-qsos* nil)

(defun clear-partial-area ()
  (dotimes (i (- *LINES* 1))
    (mvprintw i 0 (format nil "~%"))))

(defun print-qso-details (q qrz-details)
  (labels ((qrz (field)
	     (if qrz-details
		 (gethash field qrz-details)
		 "")))
    (declare (inline qrz))
    (clear-partial-area)
    (mvprintw 1  1 (format nil "His call:    ~A ~A~%"   (string-right-pad 21 (q-his-call q)) (if (member q *tagged-qsos*) "t" "")))
    (mvprintw 2  1 (format nil "My call:     ~A~%"      (q-my-call q)))
    (mvprintw 4  1 (format nil "QSO date:    ~A ~A~%"   (human-date (q-qso-date q)) (q-time-on q)))
    (mvprintw 6  1 (format nil "Frequency:   ~A (~A)~%" (q-qrg q) (q-band q)))
    (mvprintw 7  1 (format nil "Mode:        ~A~%"      (q-mode q)))
    (mvprintw 8  1 (format nil "Tx / Rx RST: ~A ~A~%"   (q-tx-rst q) (q-rx-rst q)))
    
    (mvprintw 10 1 (format nil "             ~10A ~14A ~14A~%"   "Mine" "His"  "QRZ"))
    (mvprintw 11 1 (format nil "CQ Zone:     ~10A ~14A ~14A~%"   (n->es (q-my-cq-zone q))  (n->es (q-his-cq-zone q))  (n->es (qrz "cqzone"))))
    (mvprintw 12 1 (format nil "ITU Zone:    ~10A ~14A ~14A~%"   (n->es (q-my-itu-zone q)) (n->es (q-his-itu-zone q)) (n->es (qrz "ituzone"))))
    (mvprintw 13 1 (format nil "DXCC:        ~10A ~14A ~14A~%"   (n->es (q-my-dxcc q))     (n->es (q-his-dxcc q))     (n->es (qrz "dxcc"))))
    (mvprintw 14 1 (format nil "IOTA:        ~10A ~14A ~14A~%"   (n->es (q-my-iota q))     (n->es (q-his-iota q))     (n->es (qrz "iota"))))
    (mvprintw 15 1 (format nil "Country:     ~10A ~14A ~14A~%"   (n->es (q-my-country q))  (n->es (q-his-country q))  (n->es (qrz "country"))))
    (mvprintw 16 1 (format nil "State:       ~10A ~14A ~14A~%"   (n->es (q-my-state q))    (n->es (q-his-state q))    (n->es (qrz "state"))))
    (mvprintw 17 1 (format nil "County:      ~10A ~14A ~14A~%"   (n->es (q-my-county q))   (n->es (q-his-county q))   (n->es (qrz "county"))))
    (mvprintw 18 1 (format nil "Grid:        ~10A ~14A ~14A~%"   (n->es (q-my-grid q))     (n->es (q-his-grid q))     (n->es (qrz "grid"))))

    (mvprintw 20 1 (format nil "QSL Rx / Tx: ~A ~A~%"   (q-qsl-rcvd q) (q-qsl-sent q)))

    (mvprintw 22 1 "Comment:")
    (mvprintw 23 1 (format nil "~A~%" (n->es (q-comment q))))
    (refresh)))

(defun qso-for-selection (q)
  (format nil "~A ~A ~A ~A ~A ~A~%"
	  (human-date (q-qso-date q))
	  (q-my-call q)
	  (q-his-call q)
	  (q-qrg q)
	  (q-mode q)
	  (q-qsl-sent q)))

(defun print-all-qsos (qsos)
  (if (> (length qsos) *LINES*)
      (fail)
      (let ((i 0))
	(dolist (qso qsos)
	  (mvprintw i 0 (format nil "~A  ~A ~A" (if (member qso *tagged-qsos*) "t" " ") (+ i 1) (qso-for-selection qso)))
	  (setf i (+ i 1))))))

(defun print-some-qsos (qsos)
  (if (> (length qsos) *LINES*)
      (mvprintw 1 0 (format nil "Just some then ~A" *LINES*))
      (mvprintw 1 0 (format nil "Foooo ~A" *LINES*))))

(defun get-qsos-for-call (call)
  (select 'qso :caching nil :flatp t :where [= 'his_call call]
		      :order-by '(([qso_date] :desc) ([time_on] :desc))))
  
(defun print-qsos (qsos)
  (one-value (either (print-all-qsos qsos)
		     (print-some-qsos qsos))))

(defun print-calls (calls)
  (clear-partial-area)
  (let* ((max-length (+ (loop for call in calls maximizing (length call)) 1))
	 (cols (floor (/ *COLS* max-length)))
	 (r 0)
	 (c 0))
    (dolist (call calls)
      (mvprintw r c (format nil "~A~%" (string-right-pad max-length call)))
      (setf c (+ c max-length))
      (when (> c *COLS*)
	(setf c 0
	      r (+ r 1)))))
  (refresh))

(defun find-partial-calls (part)
  (let ((matches (select 'his_call
			 :from 'qso :distinct t :caching nil :flatp t
			 :where [like 'his_call (cats "%" part "%")])))
    matches))
    
(defun print-buffer (buffer &optional (prompt ""))
  (mvprintw (- *LINES* 1) 0 (format nil "~a~a~%" prompt buffer))
  (refresh))

(defun mark-qsl-received (qso)
  (setf (q-qsl-rcvd qso) "Y"
	(q-qsl-rdate qso) (log-date))
  (update-records-from-instance qso)
  qso)

(defun mark-qsl-queued (qso)
  (setf (q-qsl-sent qso) "Q")
  (update-records-from-instance qso)
  qso)

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
				    (again ,val))))))
	  ((:exit)
	   (setf acc (append acc `(((char= ,c ,(second form)) ,(third form))))))
	  ((:default) 
	   (setf acc (append acc `((t ,(second form))))))
	  (otherwise
	   (let ((accessor-name (second form))
		 (read-value-options (fourth form)))
	     (setf acc (append acc `(((char= ,c ,(first form))
				      (setf (,accessor-name ,val) (read-value :prompt ,(third form)
									      :buffer (mkstr (n->es (,accessor-name ,val)))
									      ,@read-value-options))
				      (update-records-from-instance ,val)
				      (again ,val)))))))))
      (append `(let ((,c ,char)
		     (,val ,value)))
	      (list acc)))))
  
(defun merge-qso-qrz-details (qso qrz-details)
  (labels ((qrz (field)
	     (if qrz-details
		 (gethash field qrz-details)
		 "")))
    (declare (inline qrz))
    (if (and (qrz "cqzone") (not (equal (qrz "cqzone") "0")))
	(setf (q-his-cq-zone qso) (parse-integer (qrz "cqzone") :junk-allowed t)))
    (if (and (qrz "ituzone") (not (equal (qrz "ituzone") "0")))
	(setf (q-his-itu-zone qso) (parse-integer (qrz "ituzone") :junk-allowed t)))
    (setf (q-his-dxcc qso)     (parse-integer (qrz "dxcc") :junk-allowed t)
	  (q-his-iota qso)     (qrz "iota")
	  (q-his-country qso)  (qrz "country")
	  (q-his-state qso)    (qrz "state")
	  (q-his-county qso)   (qrz "county")
	  (q-his-grid qso)     (qrz "grid"))
    (update-records-from-instance qso)
    qso))

(defun prompt (p)
  (mvprintw (1- *LINES*) 0 (format nil "~a~%" p))
  (refresh))

(defun create-qsl (qso-list)
  (let ((full-card ()))
    (dolist (qso (mklist qso-list))
      (let ((info (make-hash-table :test 'equal)))
	(setf (gethash "his-call" info) (q-his-call qso)
	      (gethash "my-call" info)  (q-my-call qso)
	      (gethash "qso-date" info) (human-date (q-qso-date qso))
	      (gethash "time-on" info)  (subseq (q-time-on qso) 0 4)
	      (gethash "qrg" info)      (format nil "~,3f" (float (/ (q-qrg qso) 1000000)))
	      (gethash "mode" info)     (q-mode qso)
	      (gethash "tx-rst" info)   (q-tx-rst qso)
	      (gethash "my-grid" info)  (n->es (q-my-grid qso))
	      (gethash "my-itu-zone" info)   (n->es (q-my-itu-zone qso))
	      (gethash "my-cq-zone" info)    (n->es (q-my-cq-zone qso))
	      (gethash "my-address" info)    (if (fboundp 'qsl-address)
						 (qsl-address qso)
						 ""))
	(push info full-card)))
    (let ((worker-handle (sb-ext:run-program "galosh" (list "create-qsl")
					      :wait nil
					      :search (sb-ext:posix-getenv "PATH")
					      :input :stream)))
      (write-json full-card (sb-ext:process-input worker-handle))
      (close (sb-ext:process-input worker-handle)))))

(defun toggle-qso-tag (qso)
  (if (member qso *tagged-qsos*)
      (setf *tagged-qsos* (remove qso *tagged-qsos*))
      (setf *tagged-qsos* (append *tagged-qsos* (mklist qso)))))

(defun manage-qso-event-loop (qso)
  (open-in-browser (q-his-call qso))
  (let ((qrz-details (galosh-qrz:qrz-search (q-his-call qso))))
    (labels ((again (q)
	       (print-qso-details qso qrz-details)
	       (prompt "> ")
	       (case-with-char (code-char (getch)) qso
			       (#\c q-his-cq-zone "CQ Zone: " (:integer-p t))
			       (#\i q-his-itu-zone "ITU Zone: " (:integer-p t))
			       (#\I q-his-iota "IOTA: ")
			       (#\D q-his-dxcc "DXCC: " (:integer-p t))
			       (#\G q-his-grid "Grid: ")
			       (#\o q-his-country "Country: ")
			       (:func #\r #'(lambda () (mark-qsl-received q)))
			       (:func #\m #'(lambda () (toggle-qso-tag q)))
			       (:func #\t #'(lambda () (mark-qsl-queued q)))
			       (:func #\v #'(lambda () (merge-qso-qrz-details q qrz-details)))
			       (:func #\z #'(lambda () (open-in-browser (q-his-call q))))
			       (:func #\p #'(lambda () (create-qsl q)))
			       (:func #\a #'(lambda ()
					      (merge-qso-qrz-details q qrz-details)
					      (mark-qsl-received q)
					      (mark-qsl-queued q)))
			       (#\A q-his-call "Call: ")
			       (:exit #\Esc :cancel)
			       (:default (again q)))))
      (again qso))))

(defun select-qso-event-loop (buffer qso-list)
  (print-qsos qso-list)
  (print-buffer buffer)
  (let* ((raw-code (getch))
	 (c (code-char raw-code)))
    (cond ((eql c #\Newline)
	   (if (empty-string-p buffer)
	       (select-qso-event-loop buffer qso-list)
	       (let ((qso-num (parse-integer buffer)))
		 (when (and (> qso-num 0) (<= qso-num (length qso-list)))
		   (manage-qso-event-loop (nth (- qso-num 1) qso-list))
		   (setf buffer "")
		   (clear-partial-area))
		 (select-qso-event-loop buffer qso-list))))
	  ((eql c #\p)
	   (create-qsl (sort (copy-seq *tagged-qsos*) #'> :key #'(lambda (x) (parse-integer (q-qso-date x)))))
	   (select-qso-event-loop buffer qso-list))
	  ((eql c #\Rubout)
	   (select-qso-event-loop (drop-last buffer) qso-list))
	  ((member c '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0))
	   (let ((b (cats buffer (string c))))
	     (select-qso-event-loop b qso-list)))
	  ((eql c #\Esc)
	   (setf *tagged-qsos* nil)
	   :cancel)
	  (t
	   (select-qso-event-loop buffer qso-list)))))

(defun process-command (string)
  (given (first (split-words string)) #'string-equal
    ("q" nil)
    (t t)))

(defun select-call-event-loop (buffer)
  (print-buffer buffer)
  (let* ((raw-code (getch))
	 (c (code-char raw-code)))
    (cond ((eql c #\Newline)
	   (unless (empty-string-p buffer)
	     (let ((qsos (get-qsos-for-call buffer)))
	       (if (= (length qsos) 1)
		   (manage-qso-event-loop (first qsos))
		   (select-qso-event-loop "" qsos))))
	   (clear-partial-area)
	   (select-call-event-loop ""))
	  ((eql c #\Rubout)
	   (print-calls (find-partial-calls (drop-last buffer)))
	   (select-call-event-loop (drop-last buffer)))
	  ((and (eql c #\:) (zerop (length buffer)))
	   (if (process-command (read-value :prompt ":"))
	       (select-call-event-loop "")))
	  ((eql c #\Esc)
	   (select-call-event-loop buffer))
	  (t
	   (let ((b (cats buffer (string-upcase (string c)))))
	     (print-calls (find-partial-calls b))
	     (select-call-event-loop b))))))

(defun process-options (argv)
  (multiple-value-bind (leftover options)
      (getopt:getopt argv '(("mark-queued-as-sent" :none t)
			    ("match" :optional)))
    (declare (ignore leftover))
    options))

(defun start-interface ()
  (initscr)
  ;(raw) ; Get everything, including ^C, ^Z, etc
  (cbreak) ; Mainly cooked, but don't get ^C, ^Z, etc.
  (noecho)
  (when (has-colors)
    (start-color)
    (init-pair +inv-green+ COLOR_BLACK COLOR_GREEN))
  (select-call-event-loop ""))

(defun get-option (name options &key (default nil default-p))
  (if-let ((option (cdr (assoc name options))))
	  option
	  (if default-p default nil)))

(defun mark-queued-as-sent ()
  (update-records [qso]
		  :av-pairs `((qsl_sent "Y")
			      (qsl_sdate ,(log-date)))
		  :where [= [qsl_sent] "Q"]))

(define-galosh-command galosh-qsl ()
  (let* ((options (process-options argv)))
    (if (get-option "mark-queued-as-sent" options)
	(mark-queued-as-sent)
	(unwind-protect
	     (start-interface)
	  (endwin)))))
