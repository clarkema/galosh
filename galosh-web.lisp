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

(defpackage :galosh-web
  (:use :cl :gl :clsql :cl-who :galosh-qso :hunchentoot))
(in-package :galosh-web)

(clsql:locally-enable-sql-reader-syntax)

(defmacro with-html (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue nil :indent t)
     ,@body))

(defmacro standard-page ((&key title) &body body)
  `(progn
     (setf (reply-external-format*) (flex:make-external-format :utf-8 :eol-style :lf))
     (with-html
	 (str "<!DOCTYPE html>")
       (:html :xmlns "http://www.w3.org/1999/xhtml"
	      :xml\:lang "en"
	      :lang "en"
	      (:head
	       (:meta :charset "utf-8")
	       (:link :rel "stylesheet" :href "http://clarkema.org/main.css")
	       (:link :rel "stylesheet" :href "http://clarkema.org/log.css")
	       (:title ,title))
	      (:body
	       (:div :id "header" "Mike Clarke, VP8DMH")
	       (:div :id "menu"
		     (:ul
		      (:li (:a :href "http://clarkema.org/" "Home"))
		      (:li (:a :href "http://clarkema.org/blog/newest.html" "Blog Home"))
		      (:li (:a :href "http://clarkema.org/photos/index.html" "Photos"))
		      (:li (:a :href "http://clarkema.org/vp8dmh/index.html" "Radio"))
		      (:li :style "float: right" (:a :href "http://clarkema.org/contact.html" "Contact"))))
	       (:div :id "header_img")
	       (:div :id "content"
	       ,@body))))))

(defstruct (column
	     (:constructor make-column (label value-function &optional (alignment "right"))))
  label
  value-function
  alignment)

(defun format-time (time)
  (if (= (length time) 6)
      (progn
	(format nil "~A:~A:~A"
		(subseq time 0 2)
		(subseq time 2 4)
		(subseq time 4 6)))
      time))

(defparameter *detail-columns* (list (make-column "Id"        #'q-id)
				     (make-column "Operator"  #'q-operator)
				     (make-column "Date"      #'(lambda (q) (human-date (q-qso-date q))))
				     (make-column "Time On"   #'(lambda (q) (format-time (q-time-on q))))
				     (make-column "Time Off"  #'(lambda (q) (format-time (q-time-off q))))
				     (make-column "QRG (kHz)" #'(lambda (q) (/ (q-qrg q) 1000)))
				     (make-column "Band"      #'(lambda (q) (qrg->band (q-qrg q))))
				     (make-column "Mode"      #'q-mode)
				     (make-column "RxRST"     #'q-rx-rst)
				     (make-column "TxRST"     #'q-tx-rst)
				     (make-column "Comments"  #'q-comment "left")))
(defparameter *limited-columns* (list (make-column "Operator" #'q-operator)
				      (make-column "Date"     #'(lambda (q) (human-date (q-qso-date q))))
				      (make-column "Band"     #'q-band)
				      (make-column "Mode"     #'q-mode)))

(defmacro with-row-counter (&body body)
  `(let ((row-counter 0))
     ,@body))

(defun log-result (call)
  (let* ((c (string-upcase call))
	 (cols *limited-columns*)
	 (qsos (select 'qso :where [= 'hiscall c] :flatp t :caching nil)))
    (if qsos
	(with-html
	    (:h2 (fmt "QSO~p with ~a" (length qsos) c))
	  (:table
	   (:tr
	    (dolist (col cols)
	      (htm
	       (:th (str (column-label col))))))
	   (with-row-counter
	       (dolist (qso qsos)
		 (htm
		  (:tr :class (if (evenp (incf row-counter)) "even" "odd")
		       (dolist (col cols)
			 (htm
			  (:td :class (column-alignment col)
			       (str (funcall (column-value-function col) qso)))))))))))
	(with-html
	    (:h2 (str "Not found"))
	  (:p (fmt "Sorry, ~a was not found in the log." c))))))

(define-easy-handler (root :uri "/")
    (call)
  (let ((total-qsos (first (select [count[*]] :from 'qso :flatp t)))
	(unique-calls (first (select [count[distinct 'hiscall]] :from 'qso :flatp t))))
    (standard-page (:title call)
		   (:h1 "Online Logs")
		   (:p (fmt "The ~a log contains ~:d QSO~:p, with ~:d unique station~:p. You can search for yours here." (get-config "user.call") total-qsos unique-calls))
		   (:form :style "float: right"
			  (:label "Callsign search:")
			  (:input :name "call"
				  :placeholder "Callsign"
				  :autofocus "autofocus"
				  :type "search"))
		   (when call
		     (str (log-result call))))))

(defun start-server ()
  (setf *message-log-pathname* "errors"
	*default-content-type* "text/html; charset=utf-8"
	*dispatch-table* (list
;			  (create-regex-dispatcher "^/[a-z\\d]+\\d+[a-z]+$" #'(lambda () (log-details :cols *limited-columns*)))
			  'dispatch-easy-handlers
			  'default-dispatcher))
  (start (make-instance 'acceptor :port 8080)))

(defun sigint-handler (sig code context)
  (declare (ignore sig code context))
  (sb-ext:quit))

(define-galosh-command galosh-web (:required-configuration '("user.call"))
  (let ((server (start-server)))
    (sb-sys:enable-interrupt sb-unix:sigint #'sigint-handler)
    (dolist (thread (sb-thread:list-all-threads))
      (unless (equal sb-thread:*current-thread* thread)
	(sb-thread:join-thread thread)))))
