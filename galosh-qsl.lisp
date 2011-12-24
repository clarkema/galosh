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
  (:use :cl :galosh-lisp :clsql
	:galosh-qso :cl-ncurses :alexandria :st-json))
(in-package :galosh-qsl)

(clsql:enable-sql-reader-syntax) 

(defconstant +inv-green+ 1)
(defconstant +inv-red+   2)
(defvar *tagged-qsos* nil)
(defvar *route* "D")

(define-galosh-command galosh-qsl ()
  (multiple-value-bind (options leftovers) (process-options argv)
    (handler-case
	(progn
	  (process-route-option (get-option "route" options))
	  (given (third leftovers) #'string-equal
	    ("mark-queued-as-sent" (mark-queued-as-sent))
	    ("waiting" (show-waiting))
	    (t (unwind-protect
		    (start-interface)
		 (endwin)))))
      (invalid-qsl-route (e) (say e *error-output*)))))

;;;
;;; Option handling
;;;

(defun process-options (argv)
  (multiple-value-bind (leftover options)
      (getopt:getopt argv '(("route" :optional)
			    ("match" :optional)))
    (values options leftover)))

(defun get-option (name options &key (default nil default-p))
  (if-let ((option (cdr (assoc name options))))
	  option
	  (if default-p default nil)))

(defun process-route-option (route)
  (when route
    (setf *route* (given route #'string-equal
		    ('("B" "buro" "bureau")            "B")
		    ('("D" "direct")                   "D")
		    ('("E" "electronic" "eqsl" "lotw") "E")
		    ('("M" "manager")                  "M")
		    (t
		     ;; If the route has been supplied, and it isn't handled by the
		     ;; above cases, it must be invalid.  Whinge.
		     (log-error (make-condition 'invalid-qsl-route :route route) "~A"))))))

;;;
;;; Minor subcommands
;;;

(defun mark-queued-as-sent ()
  (update-records [qso]
		  :av-pairs `((qsl_sent "Y")
			      (qsl_sdate ,(log-date)))
		  :where [= [qsl_sent] "Q"]))

(defun show-waiting ()
  (let ((date-col 15))
    (format t "~&Call~vTRequest sent~%" date-col)
    (do-query ((q) [select 'qso
	       :caching nil
	       :where [and [or [= 'qsl_sent "Y"] [= 'qsl_sent "Q"]] [null 'qsl_rcvd]]
	       :order-by [qsl_sdate]])
      (format t "~&~A~vT~A~%" (q-his-call q) date-col (or (q-qsl-sdate q) "queued")))))

;;;
;;; Painting routines
;;;

(defun start-interface ()
  (initscr)
  ;(raw) ; Get everything, including ^C, ^Z, etc
  (cbreak) ; Mainly cooked, but don't get ^C, ^Z, etc.
  (noecho)
  (when (has-colors)
    (start-color)
    (init-pair +inv-green+ COLOR_BLACK COLOR_GREEN)
    (init-pair +inv-red+   COLOR_BLACK COLOR_RED))
  (select-call-event-loop ""))

(defun paint-skeleton ()
  (with-color +inv-green+
    ;; Title bar
    (mvprintw 0 0 (string-right-pad *COLS* (format nil " Route: ~A" *route*)))
    ;; Status bar
    (mvprintw (- *LINES* 2) 0 (string-right-pad *COLS* (format nil "---Galosh QSL")))))

(defun paint-warning (str)
  (with-color +inv-red+
    (mvprintw (- *LINES* 2) 0 (string-right-pad *COLS* (format nil "!! ~A" str)))
    (mvprintw (- *LINES* 1) 0 (format nil "~%")))
  (refresh)
  (getch)
  nil)

(defun print-buffer (buffer &optional (prompt ""))
  (mvprintw (- *LINES* 1) 0 (format nil "~a~a~%" prompt buffer))
  (refresh))

(defun clear-partial-area ()
  (loop for i from 1 to (- *LINES* 3) do
       (mvprintw i 0 (format nil "~%"))))

(defun prompt (p)
  (mvprintw (1- *LINES*) 0 (format nil "~a~%" p))
  (refresh))

(defun paint-help-screen ()
  (with-color +inv-green+
    (mvprintw 0 0 (string-right-pad *COLS* "i:Exit")))
  (getch))

;;;
;;; Call selection event loop
;;;

(defun select-call-event-loop (buffer)
  (paint-skeleton)
  (print-calls (find-partial-calls buffer))
  (print-buffer buffer)
  (let ((c (code-char (getch))))
    (given c #'char=
      (#\Newline (unless (empty-string-p buffer)
		   (let ((qsos (get-qsos-for-call buffer)))
		     (if (= (length qsos) 1)
			 (manage-qso-event-loop (first qsos))
			 (select-qso-event-loop qsos))))
		 (clear-partial-area)
		 (select-call-event-loop ""))
      (#\Rubout (print-calls (find-partial-calls (drop-last buffer)))
		(select-call-event-loop (drop-last buffer)))
      (#\: (if (empty-string-p buffer)
	       (process-command (read-value :prompt #'(lambda (b) (print-buffer b ":")))))
	   (select-call-event-loop buffer))
      (#\Esc (select-call-event-loop (kill-last-word buffer)))
      (+ctrl-w+ (select-call-event-loop (kill-last-word buffer)))
      (+resize+ (select-call-event-loop buffer))
      (t (select-call-event-loop (mkstr buffer (string-upcase (string c))))))))

(defun print-calls (calls)
  (clear-partial-area)
  (let* ((max-length (+ (loop for call in calls maximizing (length call)) 1))
	 (cols (floor (/ *COLS* max-length)))
	 (r 1)
	 (c 0))
    (dolist (call calls)
      (mvprintw r c (format nil "~A~%" (string-right-pad max-length call)))
      (setf c (+ c max-length))
      (when (> (+ c max-length) *COLS*)
	(setf c 0
	      r (+ r 1))
	(when (>= r (- *LINES* 2))
	  (return-from print-calls)))))
  (refresh))

(defun find-partial-calls (part)
  (select 'his_call
	  :from 'qso :distinct t :caching nil :flatp t
	  :where [like 'his_call (cats "%" part "%")]))

(defun get-qsos-for-call (call)
  (select 'qso
	  :caching nil :flatp t :where [= 'his_call call]
	  :order-by '(([qso_date] :desc) ([time_on] :desc))))

;;;
;;; QSO selection event loop
;;;

(defun select-qso-event-loop (qso-list)
  (labels ((again (buffer)
	     (paint-skeleton)
	     (clear-partial-area)
	     (print-qsos qso-list)
	     (print-buffer buffer)
	     (let ((c (code-char (getch))))
	       (given c #'char=
		 (#\Newline
		   (if (empty-string-p buffer)
		       (again buffer)
		       (let ((qso-num (parse-integer buffer)))
			 (if (and (> qso-num 0) (<= qso-num (length qso-list)))
			     (progn
			       (manage-qso-event-loop (nth (- qso-num 1) qso-list))
			       (again ""))
			     (again buffer)))))
		 (#\p
		   (create-qsl (sort (copy-seq *tagged-qsos*) #'> :key #'(lambda (x) (parse-integer (q-qso-date x)))))
		   (again buffer))
		 (#\Rubout
		   (again (drop-last buffer)))
		 ('(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)
		   (again (mkstr buffer c)))
		 (#\Esc
		   (setf *tagged-qsos* nil)
		   :cancel)
		 (#\:
		   (when (empty-string-p buffer)
		     (process-command (read-value :prompt #'(lambda (b) (print-buffer b ":")))))
		   (again buffer))
		 ;; Let the default case handle +resize+
		 (t (again buffer))))))
    (again "")))

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
      (let ((i 1))
	(dolist (qso qsos)
	  (mvprintw i 0 (format nil "~A  ~A ~A" (if (member qso *tagged-qsos*) "t" " ")
				i (qso-for-selection qso)))
	  (setf i (+ i 1))))))

(defun print-some-qsos (qsos)
  (if (> (length qsos) *LINES*)
      (mvprintw 1 0 (format nil "Just some then ~A" *LINES*))
      (mvprintw 1 0 (format nil "Foooo ~A" *LINES*))))

  
(defun print-qsos (qsos)
  (one-value (either (print-all-qsos qsos)
		     (print-some-qsos qsos))))


;;;
;;; QSO management event loop
;;;

;; Note that edit really belongs as a macrolet within manage-qso-event-loop.
;; It can't be defined there because screamer can't cope with macrolet forms,
;; but if you try and use it anywhere else there will be all kinds of errors.
(defmacro edit (field prompt &rest options)
  `(progn (setf (,field q) (read-value :prompt #'(lambda (b) (print-buffer b ,prompt))
				       :buffer (mkstr (,field q))
				       ,@options))
	  (again q)))
(defun manage-qso-event-loop (qso)
  (log-trace "> qsl::manage-qso-event-loop")
  (open-in-browser (q-his-call qso))
  (let ((qrz-details (galosh-qrz:qrz-search (q-his-call qso))))
    (labels ((again (q)
	       (paint-skeleton)
	       ;; This update is in many cases superfluous and wasteful, but it's
	       ;; here to ensure that what we print with (print-qso-details...)
	       ;; is _always_ the same as what is in the database.
	       (update-records-from-instance q)
	       (print-qso-details q qrz-details)
	       (prompt "> ")
	       (given (code-char (getch)) #'char=
		 ((gethash "edit-cq-zone" *qso-event-loop-keys*) (edit q-his-cq-zone "CQ Zone: " :integer-p t))
		 (#\i (edit q-his-itu-zone "ITU Zone: " :integer-p t))
		 (#\I (edit q-his-iota "IOTA: "))
		 (#\D (edit q-his-dxcc "DXCC: " :integer-p t))
		 (#\G (edit q-his-grid "Grid: "))
		 (#\o (edit q-his-country "Country: "))
		 (#\r (again (mark-qsl-received q)))
		 (#\m (again (toggle-qso-tag q)))
		 (#\t (again (mark-qsl-queued q)))
		 (#\v (again (merge-qso-qrz-details q qrz-details)))
		 (#\z (open-in-browser (q-his-call q))
		      (again q))
		 (#\p (create-qsl q)
		      (again q))
		 (#\a (again (progn (merge-qso-qrz-details q qrz-details)
				    (mark-qsl-received q)
				    (mark-qsl-queued q))))
		 (#\A (edit q-his-call "Call: "))
		 (#\? (paint-help-screen)
		      (again q))
		 (#\Esc :cancel)
		 (#\: (process-command (read-value :prompt #'(lambda (b) (print-buffer b ":"))))
		      (again q))
		 (t (again q)))))
      (again qso))))

(defun mark-qsl-received (qso)
  (setf (q-qsl-rcvd qso) "Y"
	(q-qsl-rcvd-via qso) *route*
	(q-qsl-rdate qso) (log-date))
  (update-records-from-instance qso)
  qso)

(defun mark-qsl-queued (qso)
  (setf (q-qsl-sent qso) "Q"
	(q-qsl-sent-via qso) *route*)
  (update-records-from-instance qso)
  qso)

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

    (mvprintw 10 1 (format nil "             ~10A ~14A ~14A~%"   "Mine" "His" (if qrz-details "QRZ" "")))
    (mvprintw 11 1 (format nil "CQ Zone:     ~10A ~14A ~14A~%"   (n->es (q-my-cq-zone q))  (n->es (q-his-cq-zone q))  (n->es (qrz "cqzone"))))
    (mvprintw 12 1 (format nil "ITU Zone:    ~10A ~14A ~14A~%"   (n->es (q-my-itu-zone q)) (n->es (q-his-itu-zone q)) (n->es (qrz "ituzone"))))
    (mvprintw 13 1 (format nil "DXCC:        ~10A ~14A ~14A~%"   (n->es (q-my-dxcc q))     (n->es (q-his-dxcc q))     (n->es (qrz "dxcc"))))
    (mvprintw 14 1 (format nil "IOTA:        ~10A ~14A ~14A~%"   (n->es (q-my-iota q))     (n->es (q-his-iota q))     (n->es (qrz "iota"))))
    (mvprintw 15 1 (format nil "Country:     ~10A ~14A ~14A~%"   (n->es (q-my-country q))  (n->es (q-his-country q))  (n->es (qrz "country"))))
    (mvprintw 16 1 (format nil "State:       ~10A ~14A ~14A~%"   (n->es (q-my-state q))    (n->es (q-his-state q))    (n->es (qrz "state"))))
    (mvprintw 17 1 (format nil "County:      ~10A ~14A ~14A~%"   (n->es (q-my-county q))   (n->es (q-his-county q))   (n->es (qrz "county"))))
    (mvprintw 18 1 (format nil "Grid:        ~10A ~14A ~14A~%"   (n->es (q-my-grid q))     (n->es (q-his-grid q))     (n->es (qrz "grid"))))

    (mvprintw 20 1 (format nil "QSL Rx / Tx: ~A~A ~A~A~%"
			   (n->es (q-qsl-rcvd q)) (n->es (q-qsl-rcvd-via q))
			   (n->es (q-qsl-sent q)) (n->es (q-qsl-sent-via q))))

    (mvprintw 22 1 (format nil "Comment: ~A~%" (n->es (q-comment q))))
    (refresh)))

(defun create-qsl (qso-list)
  (when-let ((qsos (mklist qso-list)))
    (let ((ref (car qsos))
	  (full-card ())
	  (meta (make-hash-table :test 'equal)))
      (setf (gethash "my-call" meta)     (q-my-call ref)
	    (gethash "to-call" meta)     (q-his-call ref)
	    (gethash "my-address" meta)  (if (fboundp 'qsl-address)
					     (qsl-address ref)
					     "")
	    (gethash "my-itu-zone" meta) (n->es (q-my-itu-zone ref))
	    (gethash "my-cq-zone" meta)  (n->es (q-my-cq-zone ref))
	    (gethash "my-grid" meta)     (n->es (q-my-grid ref)))
      (dolist (qso qsos)
	(let ((info (make-hash-table :test 'equal)))
	  (setf (gethash "his-call" info) (q-his-call qso)
		(gethash "qso-date" info) (human-date (q-qso-date qso))
		(gethash "time-on" info)  (subseq (q-time-on qso) 0 4)
		(gethash "qrg" info)      (format nil "~,3f" (float (/ (q-qrg qso) 1000000)))
		(gethash "mode" info)     (q-mode qso)
		(gethash "tx-rst" info)   (q-tx-rst qso))
	  (push info full-card)))
      (push meta full-card)
      (let ((worker-handle (sb-ext:run-program "galosh" (list "create-qsl")
					       :wait nil
					       :search (sb-ext:posix-getenv "PATH")
					       :input :stream)))
	(write-json full-card (sb-ext:process-input worker-handle))
	(close (sb-ext:process-input worker-handle))))))

(defun toggle-qso-tag (qso)
  (if (member qso *tagged-qsos*)
      (setf *tagged-qsos* (remove qso *tagged-qsos*))
      (setf *tagged-qsos* (append *tagged-qsos* (mklist qso))))
  qso)

(defvar *qso-event-loop-keys* (make-hash-table :test #'equal))
(setf (gethash "edit-cq-zone" *qso-event-loop-keys*) #\c)

;;;
;;; Command handling
;;;

(defun process-command (string)
  (given (first (split-words string)) #'string-equal
    ("set" (cmd-set string) t)
    ("q" (terminate))
    (t t)))

(defun cmd-set (string)
  (destructuring-bind (set place value) (split-words string :first 3)
    (declare (ignore set))
    (given place #'string-equal
      ("route" (handler-case
		 (process-route-option value)
	       (invalid-qsl-route (e) (paint-warning e)))))))

;;;
;;; Conditions
;;;

(define-condition invalid-qsl-route (error)
  ((route
    :initarg :route
    :initform ""
    :reader invalid-qsl-route-route))
  (:report (lambda (condition stream)
	     (format stream "Invalid QSL route '~A' (must be one of 'B', 'D', 'E' or 'M')"
		     (invalid-qsl-route-route condition)))))
