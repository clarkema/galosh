;;;; galosh -- amateur radio utilities.
;;;; Copyright (C) 2011, 2012, 2013 Michael Clarke, M0PRL
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

(screamer:define-screamer-package #:galosh-qsl
  (:use #:cl #:galosh-lisp #:clsql
	#:galosh-qso #:cl-ncurses #:alexandria #:st-json))
(in-package #:galosh-qsl)

(clsql:enable-sql-reader-syntax) 

(defconstant +inv-green+ 1)
(defconstant +inv-red+   2)
(defvar *route* "D")
(declaim (ftype (function)
		local-tagged-list
		global-tagged-list))

(define-galosh-command galosh-qsl ()
  (multiple-value-bind (options leftovers) (process-options argv)
    (handler-case
	(progn
	  (process-route-option (get-option "route" options))
	  (given (third leftovers) #'string-equal
	    ("mark-sent" (mark-sent *route*))
	    ("show-waiting" (show-waiting))
	    ("show-queue" (show-queue *route*))
	    (t (unwind-protect
		    (progn (setf (symbol-function 'local-tagged-list) (make-tag-list)
				 (symbol-function 'global-tagged-list) (make-tag-list))
			   (start-interface))
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
		     ;; If the route has been supplied, and it isn't
		     ;; handled by the above cases, it must be
		     ;; invalid.
		     (log-error
			 (make-condition 'invalid-qsl-route
					 :route route) "~A"))))))

;;;
;;; Minor subcommands
;;;

(defun mark-sent (route)
  (update-records [qso]
		  :av-pairs `((qsl_sent "Y")
			      (qsl_sdate ,(log-date)))
		  :where [and [= [qsl_sent] "Q"]
		              [= [qsl_sent_via] route]]))

(defun show-queue (route)
  (do-query ((q) [select 'qso
	     :caching nil
	     :where [and [= 'qsl_sent "Q"] [= 'qsl_sent_via route]]
	     :order-by [qsl-sdate]])
    (format t "~&~A~%" (q-his-call q))))

(defun show-waiting ()
  (let ((date-col 15))
    (format t "~&Call~vTRequest sent~%" date-col)
    (do-query ((q) [select 'qso
	       :caching nil
	       :where [and [or [= 'qsl_sent "Y"] [= 'qsl_sent "Q"]]
	                   [null 'qsl_rcvd]]
	       :order-by [qsl_sdate]])
      (format t "~&~A~vT~A~%" (q-his-call q)
	      date-col (or (q-qsl-sdate q) "queued")))))

;;;
;;; Data structures
;;;

(defun make-tag-list ()
  (let (tagged-qsos)
    (dlambda
     (:toggle (qso)
	      (if (member qso tagged-qsos)
		  (setf tagged-qsos (remove qso tagged-qsos))
		  (setf tagged-qsos (cons qso tagged-qsos)))
	      qso)
     (:clear () (setf tagged-qsos ()))
     (t () (sort (copy-seq tagged-qsos) #'>
		 :key (lambda (x) (parse-integer (q-qso-date x))))))))

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
      (#\: (when (empty-string-p buffer)
	     (process-command
	      (read-value :prompt #'(lambda (b) (print-buffer b ":")))))
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

(defvar *select-qso-event-loop-keys*
  (alist-hash-table
   '((#\p . :print-qsl-card)
     (#\P . :preview-qsl-card)
     (#\s . :print-swl-card)
     (#\S . :preview-swl-card)
     (#\M . :clear-locally-tagged))))

(defun make-qso-selector ()
  (dlambda
   (:print-qsl-card ()
     (create-qsl (local-tagged-list)))
   (:preview-qsl-card ()
     (create-qsl (local-tagged-list) :preview t))
   (:print-swl-card ()
     (let ((swl-call (read-value :prompt (lambda (b) (print-buffer b "SWL: "))
				 :value-required-p t
				 :ucase-p t)))
       (create-qsl (global-tagged-list) :swl swl-call)))
   (:preview-swl-card ()
     (let ((swl-call (read-value :prompt (lambda (b) (print-buffer b "SWL: "))
				 :value-required-p t
				 :ucase-p t)))
       (create-qsl (global-tagged-list) :preview t :swl swl-call)))
   (:clear-locally-tagged ()
     (local-tagged-list :clear))))

(defun make-buffer ()
  (let ((buffer ""))
    (lambda (&optional x)
      (cond
	((null x)
	 buffer)
	((symbolp x)
	 (case x
	   (:clear (setf buffer ""))))
	(t (given x #'char=
	     (#\Rubout
	      (setf buffer (drop-last buffer))
	      t)
	     ('(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)
	       (setf buffer (mkstr buffer x))
	       t)))))))

(defun select-qso-event-loop (qso-list)
  (let ((cancel (gensym)))
    (cbind ((buffer (make-buffer))
	    (selector (make-qso-selector)))
      (until= cancel
	(progn
	  (paint-skeleton)
	  (clear-partial-area)
	  (print-qsos qso-list)
	  (print-buffer (buffer))
	  (let ((cc (code-char (getch))))
	    (given cc #'char=
	      (#\Esc cancel)
	      (#\Newline
	       (unless (empty-string-p (buffer))
		 (let ((qso-num (parse-integer (buffer))))
		   (when (and (> qso-num 0) (<= qso-num (length qso-list)))
		     (manage-qso-event-loop (nth (- qso-num 1) qso-list))
		     (buffer :clear)))))
	      (#\:
	       (when (empty-string-p (buffer))
		 (process-command
		  (read-value :prompt (lambda (b) (print-buffer b ":"))))))
	      ;; Let the default case handle +resize+
	      (t
	       (unless (buffer cc)
		 (when-let ((message (gethash cc *select-qso-event-loop-keys*)))
		   (selector message))))))))
      (local-tagged-list :clear))))


(defun qso-for-selection (q)
  (format nil "~A ~A ~A ~A ~A ~A ~A~%"
	  (human-date (q-qso-date q))
	  (q-my-call q)
	  (q-his-call q)
	  (q-qrg q)
	  (subseq (q-time-on q) 0 4)
	  (q-mode q)
	  (q-qsl-sent q)))

(defun print-all-qsos (qsos)
  (if (> (length qsos) *LINES*)
      (fail)
      (let ((i 1))
	(dolist (qso qsos)
	  (mvprintw i 0 (format nil "~A ~A ~A"
				(if (member qso (local-tagged-list)) "*" "-")
				i
				(qso-for-selection qso)))
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

(defvar *qso-event-loop-keys*
  (alist-hash-table
   '((#\c . :edit-his-cq-zone)
     (#\i . :edit-his-itu-zone)
     (#\I . :edit-his-iota)
     (#\D . :edit-his-dxcc)
     (#\G . :edit-his-grid)
     (#\o . :edit-his-country)
     (#\r . :mark-qsl-received)
     (#\m . :toggle-local-tag)
     (#\M . :toggle-global-tag)
     (#\* . :clear-tag-lists)
     (#\t . :mark-qsl-queued)
     (#\z . :open-his-call-in-browser)
     (#\p . :print-qsl-card)
     (#\P . :preview-qsl-card)
     (#\s . :print-swl-card)
     (#\S . :preview-swl-card)
     (#\A . :edit-his-call)
     (#\v . :merge-qrz-details)
     (#\a . :all))))

(defun manage-qso-event-loop (qso)
  (log-trace "> qsl::manage-qso-event-loop")
  ;; (open-in-browser (q-his-call qso))
  (let ((cancel (gensym)))
    (cbind ((manager (make-qso-manager qso)))
      (until= cancel
	(progn
	  (paint-skeleton)
	  ;; This update is in many cases superfluous and wasteful,
	  ;; but it's here to ensure that what we print with
	  ;; (print-qso-details...)  is _always_ the same as what is
	  ;; in the database.
	  (update-records-from-instance qso)
	  (manager :print-qso-details)
	  (prompt "> ")
	  (let ((cc (code-char (getch))))
	    (given cc #'char=
	      (#\Esc cancel)
	      (#\: (process-command
		    (read-value :prompt (lambda (b) (print-buffer b ":")))))
	      (t (when-let ((message (gethash cc *qso-event-loop-keys*)))
		   (manager message))))))))))

;; Note that edit really belongs as a macrolet within manage-qso-event-loop.
;; It can't be defined there because screamer can't cope with macrolet forms,
;; but if you try and use it anywhere else there will be all kinds of errors.
(defmacro edit-field (field prompt &rest options)
  `(setf (,field qso) (read-value :prompt (lambda (b) (print-buffer b ,prompt))
				  :buffer (mkstr (,field qso)))))
(defun make-qso-manager (qso)
  (let ((qrz-details (galosh-qrz:qrz-search (q-his-call qso))))
    (dalambda
     (:print-qso-details () (print-qso-details qso qrz-details))
     (:all ()
	   (self :merge-qrz-details)
	   (self :mark-qsl-received)
	   (self :mark-qsl-queued))
     (:edit-his-call ()
       (edit-field q-his-call "Call: "))
     (:edit-his-iota ()
       (edit-field q-his-iota "IOTA: "))
     (:edit-his-dxcc ()
       (edit-field q-his-dxcc "DXCC: " :integer-p t))
     (:edit-his-itu-zone ()
       (edit-field q-his-itu-zone "ITU Zone: " :integer-p t))
     (:edit-his-cq-zone ()
       (edit-field q-his-cq-zone "CQ Zone: " :integer-p t))
     (:edit-his-grid ()
       (edit-field q-his-grid "Grid: "))
     (:edit-his-country ()
       (edit-field q-his-country "Country: "))
     (:merge-qrz-details ()
       (merge-qso-qrz-details qso qrz-details))
     (:print-qsl-card ()
       (create-qsl (cons qso (local-tagged-list))))
     (:preview-qsl-card ()
       (create-qsl (cons qso (local-tagged-list)) :preview t))
     (:print-swl-card ()
       (let ((swl-call (read-value :prompt (lambda (b) (print-buffer b "SWL: "))
				   :value-required-p t
				   :ucase-p t)))
	 (create-qsl (cons qso (global-tagged-list)) :swl swl-call)))
     (:preview-swl-card ()
       (let ((swl-call (read-value :prompt (lambda (b) (print-buffer b "SWL: "))
				   :value-required-p t
				   :ucase-p t)))
	 (create-qsl (cons qso (global-tagged-list)) :preview t :swl swl-call)))
     (:toggle-local-tag ()
       (local-tagged-list :toggle qso))
     (:toggle-global-tag ()
       (global-tagged-list :toggle qso))
     (:clear-tag-lists ()
       (local-tagged-list :clear)
       (global-tagged-list :clear))
     (:open-his-call-in-browser ()
       (open-in-browser (q-his-call qso)))
     (:mark-qsl-queued ()
       (setf (q-qsl-sent qso) "Q"
	     (q-qsl-sent-via qso) *route*)
       (update-records-from-instance qso))
     (:mark-qsl-received ()
       (setf (q-qsl-rcvd qso) "Y"
	     (q-qsl-rcvd-via qso) *route*
	     (q-qsl-rdate qso) (log-date))
       (update-records-from-instance qso)))))

(defun merge-qso-qrz-details (qso qrz-details)
  (labels ((qrz (field)
             (if qrz-details
                 (gethash field qrz-details)
                 "")))
    (declare (inline qrz))
    (if (and (qrz "cqzone") (not (equal (qrz "cqzone") "0")))
        (setf (q-his-cq-zone qso)
              (parse-integer (qrz "cqzone") :junk-allowed t)))
    (if (and (qrz "ituzone") (not (equal (qrz "ituzone") "0")))
        (setf (q-his-itu-zone qso)
              (parse-integer (qrz "ituzone") :junk-allowed t)))
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
    (mvprintw 1  1 (format nil "His call:    ~A ~A~%"
			   (string-right-pad 21 (q-his-call q))
			   (cond ((member q (global-tagged-list)) "T")
				 ((member q (local-tagged-list)) "t")
				 (t ""))))
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

(defun create-qsl (qso-list &key preview swl)
  (when-let ((qsos (remove-duplicates (mklist qso-list))))
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
      (if swl
	  (setf (gethash "swl" meta) "true"
		(gethash "to-call" meta) swl))
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
      (let ((worker-handle
	     (sb-ext:run-program "galosh" (if preview
					      (list "create-qsl" "--preview")
					      (list "create-qsl"))
				 :wait nil
				 :search (sb-ext:posix-getenv "PATH")
				 :input :stream)))
	(write-json full-card (sb-ext:process-input worker-handle))
	(close (sb-ext:process-input worker-handle))))))

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
