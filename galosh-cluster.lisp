;;;; galosh -- amateur radio utilities.
;;;; Copyright (C) 2010, 2011 Michael Clarke, M0PRL
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

(defpackage :galosh-cluster
  (:use :cl :gl :clsql
	:gu :cl-ncurses :alexandria :usocket))
(in-package :galosh-cluster)

(setf *print-circle* t)
(defvar *history* ())
(defvar *history-head* nil)
(defvar *client* nil)
(defparameter *history-size* 100)

(defconstant +inv-green+ 1)

(defun init-history-buffer (size)
  (setf *history* (make-list size :initial-element "")
	(cdr (nthcdr (- *history-size* 1) *history*)) *history*
	*history-head* *history*))

(defun history-push (string)
  (setf (car *history-head*) string
	*history-head* (cdr *history-head*)))
  
;;; ===================================================================
;;; Cluster client
;;; ===================================================================

(defclass cluster-client ()
  ((host :initarg :host :initform nil)
   (port :initarg :port)
   (username :initarg :username)
   (socket :initform nil)
   (stream :initform nil)
   (write-lock :initform (bordeaux-threads:make-lock))))

(defun cluster-connect (client)
  (let* ((sb-impl::*default-external-format* :ascii))
    (setf (slot-value client 'socket) (usocket:socket-connect (slot-value client 'host)
							    (parse-integer (slot-value client 'port))))
    (setf (slot-value client 'stream) (usocket:socket-stream (slot-value client 'socket)))
    client))
	 
(defun start-cluster-client (address port)
  (setf *client* (cluster-connect (make-instance 'cluster-client :host address :port port :username (get-config "cluster.user"))))
  (if (cluster-login *client*)
      (process-cluster-input *client*)
      (say "Login failed")))

(defun cluster-login (client)
  (let ((s (slot-value client 'stream)))
    (labels ((r (buffer) 
	       (when (cl-ppcre:scan "login: $" buffer)
		 (cluster-write client (format nil "~A~%" (get-config "cluster.user")))
		 (return-from cluster-login t))
	       (handler-bind ((sb-int:stream-decoding-error
			       #'(lambda (c) (invoke-restart 'attempt-resync))))
		 (let ((c (read-char s nil :EOF)))
		   (cond 
		     ((eq c :EOF) nil)
		     ((eql c #\Newline)
		      (history-push buffer)
		      (display-history)
		      (r ""))
		     (t
		      (r (cats buffer (string c)))))))))
      (r ""))))

(defun process-cluster-input (client)
  (let ((s (slot-value client 'stream)))
    (handler-bind ((sb-int:stream-decoding-error
		    #'(lambda (c)
			(invoke-restart 'sb-int:attempt-resync))))
      (labels ((r (buffer)
		 (let ((c (read-char s nil :EOF)))
		   (cond ((eq c :EOF)
			  (printw "EOF")
			  (refresh)
			  nil)
			 ((eql c #\Newline)
			  (history-push (parse-spot buffer))
			  (display-history)
			  (r ""))
			 ((eql c #\Return)
			  (r buffer))
			 (t
			  (r (format nil "~a~a" buffer (string c))))))))
	(r "")))))

(defun cluster-write (client string)
  (bordeaux-threads:with-lock-held ((slot-value client 'write-lock))
    (princ string (slot-value client 'stream))
    (force-output (slot-value client 'stream))))

;;; ===================================================================
;;; Spot
;;; ===================================================================
(defgeneric format-object (object))
(defmethod format-object ((object spot))
  (format nil " ~A ~A ~A at ~A ~A~%"
	  (spot-spotter object)
	  (spot-qrg object)
	  (spot-spotted object)
	  (spot-time object)
	  (spot-comment object)))
(defmethod format-object ((object t))
  (format nil "~A~%" object))


(defstruct spot
  spotter
  spotted
  qrg
  time
  comment
  qrz)

; Line format (taken from DXSpider's DXCommandmode.pm)
; return sprintf "DX de %-7.7s%11.1f  %-12.12s %-s $t$loc", "$_[4]:", $_[0], $_[1], $comment;
(defun parse-spot (string)
  (cl-ppcre:register-groups-bind (spotter-call spotted-qrg spotted-call comment time)
				 ("^DX de (.{7})(.{11})  (.{12}) (.+?) (\\d{4}Z)" string)
				 (return-from parse-spot (make-spot :spotted spotted-call
								    :qrg spotted-qrg
								    :spotter spotter-call
								    :time time
								    :comment comment)))
  string)

(defun display-title-bar ()
  (with-color +inv-green+
    (mvprintw 0 0 (string-right-pad *COLS* " Galosh Cluster"))))

(defun display-history ()
  (let ((display-lines (- *LINES* 3)))
    (dotimes (i display-lines)
      (mvprintw (+ 1 i) 0 (format-object (elt *history-head*
					      (+ (- *history-size* display-lines) i))))))
  (refresh))
;;;
;;; User side
;;;  

(defun repaint-all ()
  (display-title-bar)
  (display-history))

(defun ncurses-main-loop ()
  (let* ((raw-code (getch))
	 (c (code-char raw-code)))
    (cond ((= raw-code 410)
	   (repaint-all)
	   (ncurses-main-loop))
	  ((eql c #\q)
	   (cluster-write *client* (format nil "q~%")))
	  (t
	   (ncurses-main-loop)))))

(defun write-state (path)
  (with-open-file (s path
		     :direction :output
		     :if-does-not-exist :create
		     :if-exists :supersede)
    (with-standard-io-syntax
      ())))
;      (print `(setf *qrg*  ,*qrg*)  s)
;      (print `(setf *mode* ,*mode*) s)
;      (print `(setf *iota* ,*iota*) s))))

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
  (display-title-bar)
  (refresh))

(defun join-all ()
  (dolist (thread (sb-thread:list-all-threads))
    (unless (equal sb-thread:*current-thread* thread)
      (sb-thread:join-thread thread))))

(define-galosh-command galosh-cluster (:required-configuration '("cluster.host" "cluster.port" "cluster.user"))
  (let ((state-file (make-pathname :directory (fatal-get-galosh-dir) :name "galosh-cluster" :type "state")))
    (read-state state-file)
    (unwind-protect
	 (progn
	   (init-history-buffer *history-size*)
	   (start-interface)
	   (bordeaux-threads:make-thread #'(lambda ()
					     (start-cluster-client (get-config "cluster.host") (get-config "cluster.port"))))
	   (ncurses-main-loop))
      (join-all))
      (endwin)
      (write-state state-file)))
