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
  (:use :cl :gl :clsql :yaclml :galosh-qso :hunchentoot :galosh-qrzcom :alexandria))
(in-package :galosh-web)

(clsql:locally-enable-sql-reader-syntax)

(defmacro with-html (&body body)
  `(with-yaclml-output-to-string
       ,@body))

(defmacro standard-page ((&key (title "")) &body body)
  `(progn
     (setf (reply-external-format*) (flex:make-external-format :utf-8 :eol-style :lf))
     (with-html ()
       (<:ai (format nil "<!DOCTYPE html>~%"))
       (<:html :lang "en"
	       (<:head
		(<:meta :charset "utf-8")
		(<:link :rel "stylesheet" :href "http://clarkema.org/main.css")
		(<:link :rel "stylesheet" :href "http://clarkema.org/log.css")
		(<:title ,title))
	       (<:body
		(<:div :id "header" "Mike Clarke, VP8DMH")
		(<:div :id "menu"
		       (<:ul
			(<:li (<:a :href "http://clarkema.org/" "Home"))
			(<:li (<:a :href "http://clarkema.org/blog/newest.html" "Blog Home"))
			(<:li (<:a :href "http://clarkema.org/photos/index.html" "Photos"))
			(<:li (<:a :href "http://clarkema.org/vp8dmh/index.html" "Radio"))
			(<:li :style "float: right" (<:a :href "http://clarkema.org/contact.html" "Contact"))))
		(<:div :id "header_img")
		(<:div :id "content"
		       ,@body))))))

(defstruct (column
	     (:constructor make-column (label value-function &optional (alignment "right"))))
  label
  value-function
  alignment)

(defmacro fmt (control &rest args)
  (if args
      `(<:ah (format nil ,control ,@args))
      `(<:ah ,control)))

(defmacro fmts (control &rest args)
  (with-gensyms (c)
    `(let ((,c ,control))
       (if ,c
	   (fmt ,c ,@args)
	   (<:ai "&nbsp;")))))

(defun fmt-yes-or-no (arg)
  (if (string-equal arg "1")
      (<:ah "Yes")
      (<:ah "No")))

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
    (with-html
	(if qsos
	    (progn
	      (<:h2 (fmt "QSO~p with ~a" (length qsos) c))
	      (<:table
	       (<:tr
		(dolist (col cols)
		  (<:th (fmt (column-label col)))))
	       (with-row-counter
		   (dolist (qso qsos)
		     (<:tr :class (if (evenp (incf row-counter)) "even" "odd")
			   (dolist (col cols)
			     (<:td :class (column-alignment col)
				   (fmt (funcall (column-value-function col) qso)))))))))
	    (progn
	      (<:h2 "Not found")
	      (<:p (fmt "Sorry, ~a was not found in the log." c)))))))

(defun qrz-info (call)
  (let* ((client (make-instance 'qrzcom-client
				:username (get-config "qrz.user")
				:password (get-config "qrz.password")))
	 (result (details-by-call client call)))
    (flet ((r (key) (gethash key result)))
      (with-html
	  (<:div :id "qrz-info"
		 (<:h2 (<:a :href (format nil "http://www.qrz.com/db/~A" call) "QRZ.com information"))
		 (when-let ((image (gethash "image" result)))
		   (<:div :style "float: right"
			  (<:a :href image
			       (<:img :src image :width "150"))))
		 (<:div :id "qrz-details"
			(<:dl
			 (<:dt "E-mail") (<:dd (if-let ((email (r "email")))
						       (<:a :href (format nil "mailto:~A" email)
							    (fmt email))
						       (<:ai "&nbsp;")))
			 (<:dt "Website") (<:dd (if-let ((url (r "url")))
							(<:a :href url (fmt url))
							(<:ai "&nbsp;"))))
			(<:h3 "Mailing address") (<:p (fmt (string-capitalize (r "addr1"))) (<:br)
						      (fmt (string-capitalize (r "addr2"))) (<:br)
						      (fmt (string-upcase (r "zip"))) (<:br)
						      (fmt (r "state")) (<:br)
						      (fmt (r "country")))
			(<:h3 "QTH") (<:dl
				      (<:dt "Lat") (<:dd (fmt (r "lat")))
				      (<:dt "Long") (<:dd (fmt (r "lon")))
				      (<:dt "Grid") (<:dd (fmt (r "grid"))))
			(<:h3 "Awards") (<:dl
					 (<:dt "DXCC") (<:dd (fmts (r "dxcc")))
					 (<:dt "IOTA") (<:dd (fmts (r "iota")))
					 (<:dt "CQ Zone") (<:dd (fmts (r "cqzone")))
					 (<:dt "ITU Zone") (<:dd (fmts (r "ituzone"))))
			(<:h3 "QSL") (<:dl
				      (<:dt "Via") (<:dd (fmts (r "qslmgr")))
				      (<:dt "Direct?") (<:dd (fmt-yes-or-no (r "mqsl")))
				      (<:dt "eQSL?") (<:dd (fmt-yes-or-no (r "eqsl")))
				      (<:dt "LoTW?") (<:dd (fmt-yes-or-no (r "lotw"))))))))))

(defun qso-details (qso)
  (with-qso-accessors qso
    (with-html ()
      (<:h2 (fmt "QSO with ~A on ~A" q-hiscall (human-date q-qso-date)))
      (<:dl
       (<:dt "Band") (<:dd (fmt q-band))
       (<:dt "QRG")  (<:dd (fmt "~:D" q-qrg))
       (<:dt "RxRST") (<:dd (fmt q-rx-rst))
       (<:dt "TxRST") (<:dd (fmt q-tx-rst))
       (<:dt "His IOTA") (<:dd (fmts q-his-iota))
       (<:dt "My IOTA") (<:dd (fmts q-our-iota))
       (<:dt "Comment") (<:dd (fmts q-comment))))))

(define-easy-handler (review :uri "/review") ()
;  (let ((qso (first (select 'qso :order-by [id] :limit 1 :flatp t :caching nil))))
  (let ((qso (first (select 'qso :where [= 'hiscall "G0VGS"] :limit 1 :flatp t :caching nil))))
    (standard-page (:title (format nil "~A log review" (get-config "user.call")))
		   (<:ai (qso-details qso))
		   (<:ai (qrz-info (q-hiscall qso))))))

(define-easy-handler (root :uri "/") (call)
  (let ((total-qsos (first (select [count[*]] :from 'qso :flatp t)))
	(unique-calls (first (select [count[distinct 'hiscall]] :from 'qso :flatp t))))
    (standard-page (:title call)
		   (<:h1 "Online Logs")
		   (<:p (fmt "The ~a log contains ~:d QSO~:p, with ~:d unique station~:p. You can search for yours here." (get-config "user.call") total-qsos unique-calls))
		   (<:form :style "float: right"
			  (<:label "Callsign search:")
			  (<:input :name "call"
		;		   :placeholder "Callsign"
		;		   :autofocus "autofocus"
				   :type "search"))
		   (when call
		     (<:ai (log-result call))))))

(defun start-server (&key (port 8080))
  (setf *message-log-pathname* "errors"
	*default-content-type* "text/html; charset=utf-8"
	*dispatch-table* (list
;			  (create-regex-dispatcher "^/[a-z\\d]+\\d+[a-z]+$" #'(lambda () (log-details :cols *limited-columns*)))
			  'dispatch-easy-handlers
			  'default-dispatcher))
  (start (make-instance 'acceptor :port port)))

(define-galosh-command galosh-web (:required-configuration '("user.call"))
  (let* ((port (parse-integer (third argv)))
	 (server (start-server :port port)))
    (dolist (thread (sb-thread:list-all-threads))
      (unless (equal sb-thread:*current-thread* thread)
	(sb-thread:join-thread thread)))))
