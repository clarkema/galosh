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


(defpackage :galosh-web
  (:use :cl :galosh-lisp :clsql :cl-markup :galosh-qso :hunchentoot :galosh-qrzcom :alexandria
       :parenscript)
  (:shadowing-import-from :galosh-lisp #:join #:default)
  (:shadowing-import-from :clsql #:get-time)
  (:shadowing-import-from :alexandria #:switch))
(in-package :galosh-web)

(clsql:locally-enable-sql-reader-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *auto-escape* nil))

(defvar *header* "")
(defvar *footer* "")

(defmacro with-html (&body body)
  `(markup
    ,@body))

(defmacro standard-page ((&key (title "")) &body body)
  `(progn
     (setf (reply-external-format*) (flex:make-external-format :utf-8 :eol-style :lf))
     (mkstr
      *header*
      ,@body
      *footer*)))

(defstruct (column
	     (:constructor make-column (label value-function &optional (alignment "right"))))
  label
  value-function
  alignment)

(defmacro fmt (control &rest args)
  (if args
      `(format nil ,control ,@args)
      `,control))

(defmacro fmts (control &rest args)
  (with-gensyms (c)
    `(let ((,c ,control))
       (if ,c
	   (fmt ,c ,@args)
	   (raw "&nbsp;")))))

(defun fmt-yes-or-no (arg)
  (if (string-equal arg "1")
      "Yes"
      "No"))

(defun format-time (time)
  (if (= (length time) 6)
      (progn
	(format nil "~A:~A:~A"
		(subseq time 0 2)
		(subseq time 2 4)
		(subseq time 4 6)))
      time))

(defparameter *detail-columns* (list (make-column "Id"        #'q-id)
				     (make-column "Operator"  #'q-my-call)
				     (make-column "Date"      #'(lambda (q) (human-date (q-qso-date q))))
				     (make-column "Time On"   #'(lambda (q) (format-time (q-time-on q))))
				     (make-column "Time Off"  #'(lambda (q) (format-time (q-time-off q))))
				     (make-column "QRG (kHz)" #'(lambda (q) (/ (q-qrg q) 1000)))
				     (make-column "Band"      #'(lambda (q) (qrg->band (q-qrg q))))
				     (make-column "Mode"      #'q-mode)
				     (make-column "RxRST"     #'q-rx-rst)
				     (make-column "TxRST"     #'q-tx-rst)
				     (make-column "Comments"  #'q-comment "left")))
(defparameter *limited-columns* (list (make-column "Operator" #'q-my-call)
				      (make-column "Date"     #'(lambda (q) (human-date (q-qso-date q))))
				      (make-column "Band"     #'q-band)
				      (make-column "Mode"     #'q-mode)
				      (make-column "QSL?"     #'q-qsl-sent)
				      (make-column "My Grid"  #'q-my-grid)))

(defmacro with-row-counter (&body body)
  `(let ((row-counter 0))
     ,@body))

(defun log-result (call)
  (let* ((c (string-upcase call))
	 (cols *limited-columns*)
	 (qsos (select 'qso :where [= 'his_call c] :flatp t :caching nil)))
    (if qsos
	(markup
	 (:h2 (fmt "QSO~p with ~a" (length qsos) c))
	 (:table
	  (:tr
	   (loop for col in cols collecting
		(markup (:th (column-label col))))
	   (with-row-counter
	       (loop for qso in qsos collecting
		 (markup (:tr :class (if (evenp (incf row-counter)) "even" "odd")
			      (loop for col in cols collecting
				(markup (:td :class (column-alignment col)
					     (funcall (column-value-function col) qso)))))))))))
	(markup (:h2 "Not found")
		(:p (fmt "Sorry, ~a was not found in the log." c))))))

(defun qrz-info (call)
  (let* ((client (make-instance 'qrzcom-client
				:username (get-config "qrz.user")
				:password (get-config "qrz.password")))
	 (result (details-by-call client call)))
    (flet ((r (key) (gethash key result)))
      (markup (:div :id "qrz-info"
		    (:h2 (:a :href (format nil "http://www.qrz.com/db/~A" call) "QRZ.com information"))
		    (when-let ((image (gethash "image" result)))
		      (raw (markup (:div :style "float: right"
				     (:a :href image
					 (:img :src image :width "150"))))))
		    (:div :id "qrz-details"
			  (:dl
			   (:dt "E-mail") (:dd (if-let ((email (r "email")))
						       (raw (markup (:a :href (format nil "mailto:~A" email)
								    (fmt email))))
						       (raw "&nbsp;")))
			   (:dt "Website") (:dd (if-let ((url (r "url")))
							(raw (markup (:a :href url (fmt url))))
							(raw "&nbsp;"))))
			  (:h3 "Mailing address") (:p (fmt (string-capitalize (r "addr1"))) (:br)
						      (fmt (string-capitalize (r "addr2"))) (:br)
						      (fmt (string-upcase (r "zip"))) (:br)
						      (fmt (r "state")) (:br)
						      (fmt (r "country")))
			  (:h3 "QTH") (:dl
				       (:dt "Lat") (:dd (fmt (r "lat")))
				       (:dt "Long") (:dd (fmt (r "lon")))
				       (:dt "Grid") (:dd (fmt (r "grid"))))
			  (:h3 "Awards") (:dl
					  (:dt "DXCC") (:dd (fmts (r "dxcc")))
					  (:dt "IOTA") (:dd (fmts (r "iota")))
					  (:dt "CQ Zone") (:dd (fmts (r "cqzone")))
					  (:dt "ITU Zone") (:dd (fmts (r "ituzone"))))
			  (:h3 "QSL") (:dl
				       (:dt "Via") (:dd (fmts (r "qslmgr")))
				       (:dt "Direct?") (:dd (fmt-yes-or-no (r "mqsl")))
				       (:dt "eQSL?") (:dd (fmt-yes-or-no (r "eqsl")))
				       (:dt "LoTW?") (:dd (fmt-yes-or-no (r "lotw"))))))))))

(defun qso-details (qso)
  (with-qso-accessors qso
    (markup (:h2 (fmt "QSO with ~A on ~A" q-his-call (human-date q-qso-date)))
	    (:dl
	     (:dt "Band") (:dd (fmt q-band))
	     (:dt "QRG")  (:dd (fmt "~:D" q-qrg))
	     (:dt "RxRST") (:dd q-rx-rst)
	     (:dt "TxRST") (:dd q-tx-rst)
	     (:dt "His IOTA") (:dd (fmts q-his-iota))
	     (:dt "My IOTA") (:dd (fmts q-my-iota))
	     (:dt "Comment") (:dd (fmts q-comment))))))

(define-easy-handler (rigcss :uri "/rig.css") ()
  (setf (reply-external-format*) (flex:make-external-format :utf-8 :eol-style :lf)
	(content-type*) "text/css")
  "
body {
    font-family: Helvetica;
}
.button {
    padding: 1ex;
    margin: 1ex;
    text-align: center;
    background-color: #222;
    color: #fff;
    display: inline-block;
    font-weight: bold;
    -webkit-border-radius: 5px;
")

(define-easy-handler (rig :uri "/rig") ()
  (setf (reply-external-format*) (flex:make-external-format :utf-8 :eol-style :lf))
  (with-html
      (:html :lang "en"
	     (:head
	      (:meta :charset "utf-8")
	      (:meta :viewport "width=720,minimum-scale=1.0")
	      (:link :rel "stylesheet" :href "/rig.css"))
	     (:body
	      (:script :type "text/javascript"
		       (raw (ps (defun request (data)
				  (let ((req ((new |XMLHttpRequest|))))
				    (chain req (open "POST" "http://192.168.1.71:8001" true))
				    (chain req (set-request-header "Content-Type" "application/x-www-form-urlencoded"))
				    (chain req (send (concatenate 'string "command=" (chain -J-S-O-N (stringify data)))))))
				(defun qsy (qrg)
				  (request (list "qsy" qrg)))
				(defun tf ()
				  (request (list "toggle"))))))
	      (:div :class "button" :onclick "qsy('00021540000')" "QSY 21540")
	      (:div :class "button" :onclick "qsy('00021300000')" "QSY 21300")
	      (:div :class "button" "160m")
	      (:div :class "button" "80m")
	      (:div :class "button" "40m")
	      (:div :class "button" :onclick (ps (tf)) "20m")
	      (:div :class "button" :onclick (ps (tf)) "Toggle")))))

(define-easy-handler (review :uri "/review") ()
;  (let ((qso (first (select 'qso :order-by [id] :limit 1 :flatp t :caching nil))))
  (let ((qso (first (select 'qso :where [= 'his_call "G0VGS"] :limit 1 :flatp t :caching nil))))
    (standard-page (:title (format nil "~A log review" (get-config "user.call")))
		   (raw (qso-details qso))
		   (raw (qrz-info (q-his-call qso))))))

(define-easy-handler (qsl-options :uri "/qsl-options") (entity band)
  (standard-page (:title (mkstr "QSL Options: " (entity-adif->name entity)))
		 (markup
		  (:link :rel "stylesheet" :href "/resources/css/report.css")
		  (:img :src (format nil "/resources/flags/~A.svg" entity) :class "bg-flag")
		  (:h1 (mkstr "QSL Options For " (entity-adif->name entity)))
		  (:table :class "qso-table"
			  (:thead
			   (:tr
			    (:th "DX Call")
			    (:th "QSO Date")
			    (:th "QSL Sent?")
			    (:th "QSL Sent Date")
			    (:th "Comment")))
			  (loop for q in (select 'qso :where [and [= 'band band][= 'his_dxcc entity]] :flatp t)
			     collecting (markup (:tr
						 (:td (:a :href (mkstr "http://www.qrz.com/db/" (q-his-call q)) (q-his-call q)))
						 (:td (raw (human-date (q-qso-date q) :separator "&#8209;")))
						 (:td (q-qsl-sent q))
						 (:td (human-date (q-qsl-sdate q)))
						 (:td (q-comment q)))))))))

(define-easy-handler (report :uri "/report") ()
  (let ((bands (list "160m" "80m" "60m" "40m" "30m" "20m" "17m" "15m" "12m" "10m" "6m")))
    (labels ((country-band-worked (cty band)
	       (first (select [count [*]] :from 'qso :where [and [= 'band band][= 'his_dxcc cty]] :flatp t)))
	     (country-band-cfmd (cty band)
	       (first (select [count [*]] :from 'qso :where [and [= 'band band][= 'his_dxcc cty][= 'qsl_rcvd "Y"]] :flatp t)))
	     (header-row ()
	       (markup (:tr
			(:th "Entity")
			(:th "ADIF")
			(loop for band in bands
			   collecting (markup (:th band))))))
	     (country-row (adif)
	       (loop for band in bands
		  collecting (let ((worked (country-band-worked adif band))
				   (confirmed (country-band-cfmd adif band)))
			       (cond ((zerop worked)
				      (markup (:td "")))
				     ((zerop confirmed)
				      (markup (:td :class "unconfirmed qsl-stats" (:a :href (format nil "/qsl-options?entity=~A&band=~A" adif band) (mkstr confirmed "/" worked)))))
				     (t
				      (markup (:td :class "confirmed qsl-stats" (mkstr confirmed "/" worked)))))))))
      (when (entity-information-available-p)
	(standard-page (:title "DXCC Progress")
		       (markup
			(:link :rel "stylesheet" :href "/resources/css/report.css")
			(:table :class "qsl-table"
				       (:thead (raw (header-row)))
				       (:tbody (loop for i from 1
						  for v in (sort (all-entity-names) #'string-lessp)
						  collecting (let ((adif (mkstr (entity-name->adif v))))
							       (markup (:tr
									(:td v (:img :src (format nil "/resources/flags/~A.svg" adif) :class "small-flag"))
									(:td adif)
									(country-row adif))))
						  collecting (when (zerop (rem i 15)) (header-row))))))
		       (markup (:p (fmts "~D QSO~:P in your log have ~
                                  no associated DXCC information."
				  (first (select [count [*]] :from 'qso :where [null 'his_dxcc] :flatp t))))))))))

(define-easy-handler (root :uri "/") (call)
  (let ((total-qsos (first (select [count[*]] :from 'qso :flatp t)))
	(unique-calls (first (query "select count(distinct his_call) from qso" :flatp t))))
    (standard-page ()
		   (markup
		    (:h1 "Online Logs")
		    (:p (fmt "The ~a log contains ~:d QSO~:p, with ~:d unique station~:p. You can search for yours here." (get-config "user.call") total-qsos unique-calls))
		    (:form :style "float: right"
			   (:label "Callsign search:")
			   (:input :name "call"
				   :placeholder "Callsign"
				   :autofocus "autofocus"
				   :type "search")))
		   (when call
		     (log-result call)))))

(defun slurp-file (filespec)
  (with-open-file (f filespec
		     :direction :input
		     :if-does-not-exist nil)
    (when f
      (let ((tmp (make-string (file-length f))))
	(read-sequence tmp f)
	tmp))))

(defun start-server (&key (port 8080))
  (if (has-config-p "web.header")
      (setf *header* (slurp-file (get-config "web.header"))))
  (if (has-config-p "web.footer")
      (setf *footer* (slurp-file (get-config "web.footer"))))
  (setf *message-log-pathname* "errors"
	*default-content-type* "text/html; charset=utf-8"
	*dispatch-table* (list
;			  (create-regex-dispatcher "^/[a-z\\d]+\\d+[a-z]+$" #'(lambda () (log-details :cols *limited-columns*)))
			  'dispatch-easy-handlers
			  (create-folder-dispatcher-and-handler "/resources/flags/" (translate-logical-pathname "GL:resources;flags;"))
			  (create-folder-dispatcher-and-handler "/resources/css/"   (translate-logical-pathname "GL:resources;css;"))
			  'default-dispatcher))
  (start (make-instance 'easy-acceptor :address "127.0.0.1" :port port)))

(defun buildapp-init ()
  (load-entity-information))

(define-galosh-command galosh-web (:required-configuration '("user.call"))
  (let* ((port (parse-integer (or (third argv) "8080")))
	 (server (start-server :port port)))
    (dolist (thread (sb-thread:list-all-threads))
      (unless (equal sb-thread:*current-thread* thread)
	(sb-thread:join-thread thread)))))
