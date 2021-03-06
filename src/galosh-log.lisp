;;;; galosh -- amateur radio utilities.
;;;; Copyright (C) 2010, 2011, 2012 Michael Clarke, M0PRL
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
  (:use :cl :galosh-lisp :clsql
	:galosh-qso :cl-ncurses :alexandria :drakma))
(in-package :galosh-log)

(clsql:enable-sql-reader-syntax) 

(defvar *operator* nil)
(defvar *iota*     nil)

(defconstant +inv-green+ 1)

(defun parse-qrg (qrg)
  (let ((input (if (integerp qrg) qrg (parse-float qrg))))
    (check-type input (real 0 *) "a positive real number")
    (floor (* input 1000))))

(let ((qrg-tx 14260000)
      (qrg-rx 14260000)
      (qrg-lock (bordeaux-threads:make-lock)))
  (defun qrg ()
    (bordeaux-threads:with-lock-held (qrg-lock)
      qrg-tx))
  (defun qrg-tx ()
    (bordeaux-threads:with-lock-held (qrg-lock)
      qrg-tx))
  (defun qrg-rx ()
    (bordeaux-threads:with-lock-held (qrg-lock)
      qrg-rx))
  (defun (setf qrg) (value)
    (bordeaux-threads:with-lock-held (qrg-lock)
      (ignore-errors
	(setf qrg-rx (parse-qrg value)
	      qrg-tx qrg-rx))))
  (defun (setf qrg-rx) (value)
    (bordeaux-threads:with-lock-held (qrg-lock)
      (ignore-errors
	(setf qrg-rx (parse-qrg value)))))
  (defun (setf qrg-tx) (value)
    (bordeaux-threads:with-lock-held (qrg-lock)
      (ignore-errors
	(setf qrg-tx (parse-qrg value))))))

(let ((mode "SSB")
      (mode-lock (bordeaux-threads:make-lock)))
  (defun mode ()
    (bordeaux-threads:with-lock-held (mode-lock)
      mode))
  (defun (setf mode) (value)
    (bordeaux-threads:with-lock-held (mode-lock)
      (setf mode value))))

;;; ===================================================================
;;; Utilities
;;; ===================================================================
(defun ensure-valid-rst (val mode)
  (let ((phone-modes '("SSB" "ESSB" "AM"))
	(tone-modes '("CW")))
    (cond ((stringp val)
	   (cond ((member mode phone-modes :test #'string=)
		  (let ((rst (copy-seq "59")))
		    (if (= (length val) 1)
			(setf (subseq rst 1 2) val)
			(setf (subseq rst 0 (length val)) val))
		    rst))
		 ((member mode tone-modes :test #'string=)
		  (let ((rst (copy-seq "599")))
		    (if (= (length val) 1)
			(setf (subseq rst 1 2) val)
			(setf (subseq rst 0 (length val)) val))
		    rst))
		 (t val)))
	  ((integerp val) (princ-to-string val))
	  (t (princ-to-string (default-rst-for-mode mode))))))

;;; ===================================================================
;;; Painting functions
;;; ===================================================================
(defun paint-skeleton ()
  (macrolet ((bar (y x &body body)
	       `(mvprintw ,y ,x (string-right-pad *COLS* ,@body))))
    (with-color +inv-green+
      ;; Title bar
      (bar 0 0 (format nil " TX: ~10A RX: ~10A Mode: ~a IOTA: ~a"
		       (qrg-tx) (qrg-rx) (mode) *iota*))
      ;; Status bar
      (bar (- *LINES* 2) 0 (format nil "---Galosh Logger: ~A" *default-database*))
      ;; Help bar
      (bar 6 0 " g:grid i:iota c:comment C:call n:name f:followup")))
  (refresh))

(defun prompt (prompt &optional (buffer ""))
  (mvprintw (- *LINES* 1) 0 (format nil "~A~A~%" prompt buffer))
  (refresh))

(defun paint-history ()
  (let* ((history-size (- *LINES* 9)) ; 9 = 5 for qso, 3 status bars, 1 entry line
	 (history-start 7)
	 (q (reverse (select 'qso
			     :order-by '(([qso_date] :desc)([time_on] :desc)([id] :desc))
			     :limit history-size
			     :caching nil
			     :flatp t)))
	 (q-length (length q)))
    (dotimes (i history-size)
      (mvprintw (+ history-start i) 0
		(format nil "~A~%" (if (< i q-length) (as-string (elt q i)) "")))))
  (refresh))

(defun paint-qso (q)
  (macrolet ((row (y &rest fmt-args)
	       `(mvprintw ,y 0 (string-right-pad 40 (format nil ,@fmt-args)))))
    (with-qso-accessors q
      (row 1 "~A ~A ~A ~A ~A ~A"
	   (subseq q-qso-date 2) (subseq q-time-on 0 4) q-his-call
	   q-qrg q-rx-rst q-tx-rst)
      (row 2 "    IOTA: ~6@A  Mode: ~A" (n->es q-his-iota) (n->es q-mode))
      (row 3 "    Name: ~A" (n->es q-his-name))
      (row 4 "    Comment: ~A" (n->es q-comment))
      (row 5 "    Follow up? ~A  Grid: ~6@A" q-followup (n->es q-his-grid))
      (refresh))))

(defun paint-qrz-info (call)
  (when (galosh-qrz:has-offlinedb-p)
    (let ((result (galosh-qrz:offline-qrz-search call))
	  (i 1))
      (dotimes (i 5)
	(mvprintw (1+ i) 40 (string-truncate (format nil "~A~%" (n->es (nth i result))) (- *COLS* 40)))))
    (refresh)))

(defun option-mode-loop (qso)
  ;; Note multiple evaluation of 'field' in macro below -- no problem
  ;; as long as it's only passed the name of a QSO accessor.
  (macrolet ((edit (field prompt &rest options)
	       `(progn
		  (setf (,field qso) (read-value :prompt #'(lambda (b) (prompt ,prompt b))
						 :buffer (mkstr (,field qso))
						 ,@options))
		  (option-mode-loop qso))))
    (paint-qso qso)
    (prompt "> ")
    (given (code-char (getch)) #'char=
      (#\c (edit q-comment "Comment: "))
      (#\C (edit q-his-call "Call: " :ucase-p t :value-required-p t))
      (#\g (edit q-his-grid "Grid: "))
      (#\i (edit q-his-iota "IOTA: " :ucase-p t))
      (#\n (edit q-his-name "Name: " :capitalize-p t))
      (#\f (q-toggle-followup qso)
	   (option-mode-loop qso))
      (#\Newline qso)
      (#\Esc :cancel)
      (+resize+
       (paint-history)
       (paint-skeleton)
       (option-mode-loop qso))
      (t (option-mode-loop qso)))))

(defun display-qso-and-prompt-for-options (q)
  (let ((qso (option-mode-loop q)))
    (when (eq (class-of qso) (find-class 'qso))
      (setf (q-time-off qso) (log-time))
      (update-records-from-instance qso)
      (paint-history)
      qso)))

(defun run-full-call-handlers (call)
  (paint-qrz-info call))

(defun process-entry (buffer)
  (destructuring-bind (call &optional rx-rst tx-rst) (split-words buffer :first 3)
    (if (sane-callsign-p call)
	(let ((q (make-instance 'qso
				:my-call *operator*
				:his-call call
				:qso-date (log-date)
				:time-on  (log-time)
				:mode (mode)
				:qrg (qrg-tx)
				:qrg-rx (qrg-rx)
				:my-iota *iota*
				:my-grid (get-config "user.grid" :default nil)
				:tx-rst (ensure-valid-rst tx-rst (mode))
				:rx-rst (ensure-valid-rst rx-rst (mode)))))
	  (run-full-call-handlers call)
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
    (paint-history))
  t)

(defun cmd-set (string)
  (let ((words (split-words string :first 3)))
    (if (>= (length words) 3)
	(destructuring-bind (place value) (cdr words)
	  (given place #'string-equal
		 ("qrg"  (setf (qrg) value))
		 ("qrg-tx" (setf (qrg-tx) value))
		 ("qrg-rx" (setf (qrg-rx) value))
		 ("mode" (setf (mode) (string-upcase value)))
		 ("iota" (setf *iota* (string-upcase value)))))))
  t)

(defun event-loop (buffer &optional (pos 0))
  (paint-skeleton)
  (prompt "" buffer)
  (move (- *LINES* 1) pos)
  (refresh)
  (let* ((d (getch))
	 (c (code-char d)))
    (cond ((eql c +resize+)
	   (paint-history)
	   (event-loop buffer pos))
	  ((and (eql c #\:) (empty-string-p buffer))
	   (if (process-command (read-value :prompt #'(lambda (b) (prompt ":" b))))
	       (event-loop "" 0)))
          ((eql c #\Newline)
	   (unless (empty-string-p buffer)
	     (process-entry buffer))
	   (event-loop "" 0))
	  ((member c '(#\Tab #\Space))
	   (when (= (length (split-words buffer)) 1)
	     (run-full-call-handlers buffer))
	   (event-loop (mkstr (subseq buffer 0 pos) #\Space (subseq buffer pos)) (incf pos)))
	  ((eql c #\Esc)
	   (event-loop buffer pos))
          ((eql c #\Rubout)
	   (let ((before (subseq buffer 0 pos))
		 (after (subseq buffer pos)))
	     (event-loop (mkstr (drop-last before) after) (if (> pos 0) (decf pos) 0))))
	  ((eql c +ctrl-a+)
	   (event-loop buffer 0))
	  ((eql c +ctrl-e+)
	   (event-loop buffer (length buffer)))
	  ((eql c +ctrl-w+)
	   (let ((before (subseq buffer 0 pos))
		 (after (subseq buffer pos)))
	     (multiple-value-bind (rest removed) (kill-last-word before)
	      (event-loop (mkstr rest after) (if removed (- pos (length removed)) 0)))))
          (t
	   (let ((b (mkstr (subseq buffer 0 pos) (string-upcase (string c)) (subseq buffer pos))))
	     (event-loop b (incf pos)))))))

(defun write-state (path)
  (with-open-file (s path
		     :direction :output
		     :if-does-not-exist :create
		     :if-exists :supersede)
    (with-standard-io-syntax
      (print `(setf (qrg-tx) ,(float (/ (qrg-tx) 1000))) s)
      (print `(setf (qrg-rx) ,(float (/ (qrg-rx) 1000))) s)
      (print `(setf (mode)   ,(mode)) s)
      (print `(setf *iota*   ,*iota*) s))))

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
  (paint-history)
  (bordeaux-threads:make-thread #'rig-poller)
  (event-loop ""))

;;; Try and start the rig polling thread.  If it fails (most likely because
;;; we aren't running or can't find the rig server) let the thread silently
;;; die.  The (ignore-errors) wrapper simply stops an ugly error message
;;; from appearing when the user quits -log.
(defun rig-poller ()
  (ignore-errors
    (multiple-value-bind (body status) (http-request "http://localhost:8001/")
      ;; Checking for change prevents an unneccesary ncurses update every
      ;; half-second when nothing has happened.
      (when (not (= body (qrg)))
	(setf (qrg) body)
	(paint-skeleton)
	(refresh)))
    (sleep 0.5)
    (rig-poller)))

(defun process-options (argv)
  (multiple-value-bind (leftover options)
      (getopt:getopt argv '(("complete-missing-entities" :none t)))
    (declare (ignore leftover))
    options))

(defun get-option (name options &key (default nil default-p))
  (if-let ((option (cdr (assoc name options))))
	  option
	  (if default-p default nil)))

(define-galosh-command galosh-log (:require-config '("user.call"))
  (let ((options (process-options argv))
	(state-file (make-pathname :directory (fatal-get-galosh-dir)
                               :name "galosh-log"
                               :type "state"))
	(*operator* (get-config "user.call")))
    (declare (ignore options))
    (read-state state-file)
    (unwind-protect
	 (start-interface)
      (endwin)
      (write-state state-file))))
