;;;; galosh -- amateur radio utilities.
;;;; Copyright (C) 2010 Michael Clarke, M0PRL
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

(defpackage :galosh-qrzcom
  (:use :cl :gl :clsql :drakma)
  (:export :qrzcom-client :details-by-call :username :password
	   :qrz-record
	   :qrz-id
	   :qrz-call
	   :qrz-lastname
	   :qrz-country-id
	   :qrz-country
	   :qrz-country-name
	   :qrz-firstname
	   :qrz-jpg-p
	   :qrz-dateofbirth
	   :qrz-effectivedate
	   :qrz-expirationdate
	   :qrz-mailstreet
	   :qrz-mailcity
	   :qrz-mailstate
	   :qrz-zipcode
	   :qrz-licenseclass
	   :qrz-previouscall
	   :qrz-previousclass
	   :make-qrz-record
	   :qrz-country
	   :country-id
	   :country-name))

(in-package :galosh-qrzcom)

(defparameter *qrz-api-url* "http://www.qrz.com/xml")
(defvar *qrz-keys-by-username* (make-hash-table :test 'equal))

;;;
;;; State management
;;;
(defun write-state (path)
  (with-open-file (s path
		     :direction :output
		     :if-does-not-exist :create
		     :if-exists :supersede)
    (with-standard-io-syntax
      (print `(setf *qrz-keys-by-username* ,*qrz-keys-by-username*) s))))

(defun read-state ()
  (let ((state-files (list (make-pathname :directory (fatal-get-galosh-dir) :name "galosh-qrzcom" :type "state"))))
    (dolist (file state-files)
      (if (probe-file file)
	  (load file)))))

#+sbcl
(let ((state-hook #'(lambda ()
		      (when (get-galosh-dir)
			(write-state (make-pathname :directory (get-galosh-dir) :name "galosh-qrzcom" :type "state"))))))
  (if (member state-hook sb-ext:*exit-hooks*)
      (warn "galosh-qrzcom: State hook already added to sb-ext:*exit-hooks*")
      (push state-hook sb-ext:*exit-hooks*)))

;;;
;;; QRZ.com client conditions
;;;
(define-condition session-timeout-error (error)
  ())
(define-condition invalid-session-key-error (error)
  ())
(define-condition callsign-not-found-error (error)
  ((text :initarg :text :reader text)))

(defclass qrzcom-client ()
  ((username :initarg :username :initform "")
   (password :initarg :password :initform "")))

(defun get-qrz-key (username)
  (gethash username *qrz-keys-by-username*))
(defun (setf get-qrz-key) (new username)
  (setf (gethash username *qrz-keys-by-username*) new))

(defun login (client)
  (let* ((doc (cxml:parse (http-request (format nil "~A?username=~A&password=~A&agent=~A"
						*qrz-api-url*
						(slot-value client 'username)
						(slot-value client 'password)
						"galosh-testing"))
			  (cxml-dom:make-dom-builder)))
	 (key (aref (dom:get-elements-by-tag-name doc "Key") 0)))
    (setf (get-qrz-key (slot-value client 'username)) (dom:node-value (dom:first-child key)))))

(defun api-call (client request)
  ;; If nothing is stored in *qrz-keys-by-username*, we haven't yet logged in.
  ;; Try reading our state file to see if that gives us a stored key we can use.
  (if (zerop (hash-table-count *qrz-keys-by-username*))
      (read-state))
  ;; Still nothing?  Try logging in!
  (if (zerop (hash-table-count *qrz-keys-by-username*))
      (login client))
  (labels ((api-request (try)
	     (let* ((response (http-request (format nil "~A?s=~A&~A" 
						    *qrz-api-url* 
						    (get-qrz-key (slot-value client 'username))
						    request)))
		    (doc (cxml:parse response (cxml-dom:make-dom-builder)))
		    (err (dom:get-elements-by-tag-name doc "Error")))
	       (if (> (length err) 0)
		   (let ((error-text (dom:node-value (dom:first-child (aref err 0)))))
		     (cond
		       ((string-equal error-text "Invalid session key")
			(if (> try 3)
			    (error 'invalid-session-key-error)
			    (progn
			      (login client)
			      (api-request (1+ try)))))
		       ((string-equal error-text "Session Timeout")
			(if (> try 3)
			    (error 'session-timeout-error)
			    (progn
			      (login client)
			      (api-request (1+ try)))))
		       ((string-equal (subseq error-text 0 10) "Not found:")
			(error 'callsign-not-found-error :text error-text))
		       (t
			(error (cats "Unknown error: " error-text)))))
		   doc))))
    (api-request 1)))

(defun details-by-call (client call)
  (let ((result (make-hash-table :test 'equal))
	doc callsign)
    (handler-case
	(setf doc (api-call client (cats "callsign=" call)))
      (callsign-not-found-error () (return-from details-by-call nil)))
    (setf callsign (aref (dom:get-elements-by-tag-name doc "Callsign") 0))
    (dom:do-node-list (node (dom:child-nodes callsign) result)
      (when (dom:element-p node)
	(setf (gethash (dom:node-name node) result) (dom:node-value (dom:first-child node)))))))

(def-view-class qrz-record ()
  ((id
    :type integer
    :db-type "INTEGER" ; Required to make sqlite autoincrement work properly.
    :db-kind :key
    :db-constraints :not-null
    :initform nil
    :accessor qrz-id)
   (call
    :db-type "TEXT"
    :initform nil
    :accessor qrz-call)
   (lastname
    :db-type "TEXT"
    :initform nil
    :accessor qrz-lastname)
   (country-id
    :type integer
    :initform nil
    :accessor qrz-country-id)
   (country
    :db-kind :join
    :db-info (:join-class qrz-country
			  :home-key country-id
			  :foreign-key id
			  :set nil)
    :accessor qrz-country)
   (firstname
    :db-type "TEXT"
    :initform nil
    :accessor qrz-firstname)
   (jpg
    :db-type "TEXT"
    :initform 0
    :accessor qrz-jpg-p)
   (dateofbirth
    :type integer
    :initform nil
    :accessor qrz-dateofbirth)
   (effectivedate
    :type integer
    :initform nil
    :accessor qrz-effectivedate)
   (expirationdate
    :type integer
    :initform nil
    :accessor qrz-expirationdate)
   (mailstreet
    :db-type "TEXT"
    :initform nil
    :accessor qrz-mailstreet)
   (mailcity
    :db-type "TEXT"
    :initform nil
    :accessor qrz-mailcity)
   (mailstate
    :db-type "TEXT"
    :initform nil
    :accessor qrz-mailstate)
   (zipcode
    :db-type "TEXT"
    :initform nil
    :accessor qrz-zipcode)
   (licenseclass
    :db-type "TEXT"
    :initform nil
    :accessor qrz-licenseclass)
   (previouscall
    :db-type "TEXT"
    :initform nil
    :accessor qrz-previouscall)
   (previousclass
    :db-type "TEXT"
    :initform nil
    :accessor qrz-previousclass))
  (:base-table "amateur"))

(def-view-class qrz-country ()
  ((id
    :type integer
    :db-kind :key
    :db-constraints :not-null
    :initform nil
    :initarg :id
    :accessor country-id)
   (name
    :db-type "TEXT"
    :initform nil
    :initarg :name
    :accessor country-name))
  (:base-table "countries"))

(defun make-qrz-record (fields)
  (if (= (length fields) 15)
      (let ((r (make-instance 'qrz-record)))
	(setf (qrz-call r) (remove #\Space (first fields)))
	(setf (qrz-lastname r) (second fields))
	(setf (qrz-country-id r) (parse-integer (string-trim '(#\.) (third fields)) :junk-allowed t))
	(setf (qrz-firstname r) (fourth fields))
	(setf (qrz-jpg-p r)     (fifth fields))
	(setf (qrz-dateofbirth r) (parse-integer (sixth fields) :junk-allowed t))
	(setf (qrz-effectivedate r) (parse-integer (seventh fields) :junk-allowed t))
	(setf (qrz-expirationdate r) (parse-integer (eighth fields) :junk-allowed t))
	(setf (qrz-mailstreet r) (ninth fields))
	(setf (qrz-mailcity r) (tenth fields))
	(setf (qrz-mailstate r) (nth 10 fields))
	(setf (qrz-zipcode r) (nth 11 fields))
	(setf (qrz-licenseclass r) (nth 12 fields))
	(setf (qrz-previouscall r) (nth 13 fields))
	(setf (qrz-previousclass r) (nth 14 fields))
	r)
      (format t "Not enough fields ~s" fields)))
  
(defun qrz-country-name (record)
  (if (qrz-country record)
      (country-name (qrz-country record))))
