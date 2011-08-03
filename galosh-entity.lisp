;;;; Galosh -- amateur radio utilities.
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

;;;; The functions contained within this file deal with entity resolution.
;;;;
;;;; Entity resolution is the process of converting a callsign to a
;;;; structure representing the associated DXCC entity, CQ zone,
;;;; ITU zone, etc.
;;;;
;;;; Galosh makes use of prefix and exception data from clublog.org.
;;;; The cty.xml file from clublog.org is parsed into a trie, which
;;;; is reponsible for most of the lookup process.
;;;; 
;;;; Unfortunately, the prefix information in cty.xml doesn't tell the
;;;; whole story.  In some cases, dedicated code is required in order
;;;; to determine whether a match is valid or not.  This code is loaded
;;;; from cty.galosh, and attached to various points in the trie as
;;;; required.  Such code snippets can either prevent a call matching
;;;; against a node in the trie, or can completely replace the result
;;;; after the match is finished.

(in-package :galosh-lisp)

(defvar *entity-trie* :not-tried)
(defvar *full-call-exceptions* (make-hash-table :test 'equal))
(defvar *invalid-calls* (make-hash-table :test 'equal))

(defclass entity ()
  ((prefix    :initarg :prefix    :accessor entity-prefix    :initform nil)
   (name      :initarg :name      :accessor entity-name      :initform nil)
   (adif      :initarg :adif            :initform nil)
   (cq-zone   :initarg :cq-zone   :accessor entity-cq-zone   :initform nil)
   (itu-zone  :initarg :itu-zone  :accessor entity-itu-zone  :initform nil)
   (continent :initarg :continent :accessor entity-continent :initform nil)
   (latitude  :initarg :latitude  :accessor entity-latitude  :initform nil)
   (longitude :initarg :longitude :accessor entity-longitude :initform nil)
   (start     :initarg :start     :accessor entity-start     :initform nil)
   (end       :initarg :end       :accessor entity-end       :initform nil)))

(defmethod entity-name ((o t)) "UNKNOWN")
(defmethod entity-adif ((o entity))
  (when (slot-value o 'adif)
    (parse-integer (slot-value o 'adif))))
(defmethod entity-adif ((o t)) nil)

(defmethod print-object ((object entity) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~A (DXCC: ~A, CQ: ~A, ITU: ~A, LAT: ~A, LONG: ~A)~%    (From: ~A To: ~A)"
	    (entity-prefix object)
	    (entity-name object)
	    (entity-adif object)
	    (entity-cq-zone object)
	    (entity-itu-zone object)
	    (entity-latitude object)
	    (entity-longitude object)
	    (entity-start object)
	    (entity-end object))))

;;;; ===================================================================
;;;; Trie
;;;; ===================================================================
(defstruct (node (:predicate trie-p))
  a b c d e f g h i j k l m n o p
  q r s t u v w x y z |0| |1| |2| |3| |4| |5| |6| |7| |8| |9| |/|
  value match-predicate match-filter)

(defun make-trie () (make-node))

(defun e-trie-slot (node slot)
  (slot-value node (find-symbol (string-upcase slot) 'galosh-lisp)))
(defun (setf e-trie-slot) (new node slot)
  (setf (slot-value node (find-symbol (string-upcase slot) 'galosh-lisp)) new))

(defun e-trie-find-node (trie key)
  (if (= (length key) 0)
      trie
      (let ((first-key (subseq key 0 1))
	    (rest-key (subseq key 1)))
	(unless (e-trie-slot trie first-key)
	  (setf (e-trie-slot trie first-key) (make-node)))
	(e-trie-find-node (e-trie-slot trie first-key) rest-key))))

(defun (setf e-trie-get) (new trie key)
  (let ((node (e-trie-find-node trie key)))
    (if (node-value node)
	(progn
	  (unless (vectorp (node-value node))
	    (let ((current (node-value node)))
	      (setf (node-value node) (make-array 2 :fill-pointer 0 :adjustable t))
	      (vector-push-extend current (node-value node))))
	  (vector-push-extend new (node-value node)))
	(setf (node-value node) new))))

(defun (setf e-trie-get-predicate) (new trie key)
  (setf (node-match-predicate (e-trie-find-node trie key)) new))

(defun (setf e-trie-get-filter) (new trie key)
  (setf (node-match-filter (e-trie-find-node trie key)) new))

(defun entity-date-matchp (entity datetime)
  (cond ((and (entity-end entity) (string-greaterp datetime (entity-end entity)))
	 nil)
	((and (entity-start entity) (string-lessp datetime (entity-start entity)))
	 nil)
	(t t)))
	 
(defun first-matching-element (vec fn)
  (do ((i 0 (+ i 1)))
      ((>= i (length vec))
       nil)
    (when (funcall fn (aref vec i))
      (return-from first-matching-element (aref vec i)))))

(defun check-node-predicate (node call)
  (if (node-match-predicate node)
      (funcall (node-match-predicate node) call)
      t))

(defun check-matches (node call datetime)
  "Check an entity matches any criteria required to permit it to be
added to the search results.  Returns either the matching entity, or nil."
  (and (entity-date-matchp (node-value node) datetime)
       (check-node-predicate node call)))

(defun e-trie-get (trie full-call sought datetime)
  (let (current-value
	filter)
    (labels ((process-filter (entity)
	       (if filter
		   (funcall filter full-call entity)
		   entity))
	     (t-get (trie key)
	       (when-let ((n (node-value trie)))
		 (if (vectorp n)
		     (if-let ((f (first-matching-element n #'(lambda (e)
							       (entity-date-matchp e datetime)))))
		       (setf current-value f))
		     (when (check-matches trie sought datetime)
		       (setf current-value n)
		       (when-let ((new-filter (node-match-filter trie)))
			 (setf filter new-filter)))))
	       (if (= (length key) 0)
		   current-value
		   (let ((first-key (subseq key 0 1))
			 (rest-key  (subseq key 1)))
		     (if (not (e-trie-slot trie first-key))
			 current-value
			 (t-get (e-trie-slot trie first-key) rest-key))))))
      (process-filter (t-get trie sought)))))

;;;; ===================================================================
;;;; Condition handling and definition
;;;; ===================================================================
(define-condition entity-not-found-error (error)
  ((call :initarg :call
	 :accessor entity-not-found-error-call
	 :initform nil
	 :documentation "The call (or partical call) for which the error is signalled.")))

(defmethod print-object ((object entity-not-found-error) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "Entity not found for call (or partial): '~A'"
	    (entity-not-found-error-call object))))

;;;; ===================================================================
;;;; Parsing
;;;; ===================================================================
(defun node-text (node &optional str)
  (if str
      (if (not (zerop (length (dom:get-elements-by-tag-name node str))))
	  (dom:node-value (dom:first-child (elt (dom:get-elements-by-tag-name node str) 0)))
	  "")
      (dom:node-value (dom:first-child node))))

(defun parse-prefix (node)
  "Parse an XML <entity> element into a an entity instance."
  (labels ((txt (name) (es->n (node-text node name))))
    (proclaim '(inline txt))
    (make-instance 'entity
		   :prefix    (txt "call")
		   :name      (string-capitalize (txt "entity"))
		   :adif      (txt "adif")
		   :cq-zone   (txt "cqz")
		   :itu-zone  (txt "ituz")
		   :continent (txt "cont")
		   :latitude  (txt "lat")
		   :longitude (txt "long")
		   :start     (txt "start")
		   :end       (txt "end"))))
		 
(defun add-exception (entity)
  (symbol-macrolet ((v (gethash (entity-prefix entity) *full-call-exceptions*)))
    (if v
	(progn
	  (unless (vectorp v)
	    (let ((current v))
	      (setf v (make-array 2 :fill-pointer 0 :adjustable t))
	      (vector-push-extend current v)))
	  (vector-push-extend entity v))
	(setf v entity))))

(defun add-invalid (entity)
  (symbol-macrolet ((v (gethash (entity-prefix entity) *invalid-calls*)))
    (if v
	(progn
	  (unless (vectorp v)
	    (let ((current v))
	      (setf v (make-array 2 :fill-pointer 0 :adjustable t))
	      (vector-push-extend current v)))
	  (vector-push-extend entity v))
	(setf v entity))))

(defun parse-cty (path)
  (let ((trie (make-trie))
	(doc (cxml:parse-file path (cxml-dom:make-dom-builder))))
    (loop for p across (dom:get-elements-by-tag-name doc "prefix")
       do (let ((prefix (parse-prefix p)))
	    (if (find #\- (entity-prefix prefix))
		;; Not sure what these elements represent
		nil ;(warn (mkstr "Oops -- " (entity-prefix prefix)))
		(setf (e-trie-get trie (entity-prefix prefix)) prefix))))
    (loop for p across (dom:get-elements-by-tag-name doc "exception")
       do (add-exception (parse-prefix p)))
    (loop for p across (dom:get-elements-by-tag-name doc "invalid")
       do (let ((prefix (parse-prefix p)))
	    (add-invalid prefix)))
    trie))

(defun get-exception (call datetime)
  (if-let ((value (gethash call *full-call-exceptions*)))
    (if (vectorp value)
	(first-matching-element value #'(lambda (e)
					  (entity-date-matchp e datetime)))
	(when (entity-date-matchp value datetime)
	    value))))

(defun get-invalid (call datetime)
  (if-let ((value (gethash call *invalid-calls*)))
    (if (vectorp value)
	(first-matching-element value #'(lambda (e)
					  (entity-date-matchp e datetime)))
	(when (entity-date-matchp value datetime)
	  (make-instance 'entity
			 :name "Invalid2" :adif "1000")))))


(defun get-entity (call &optional (datetime (get-datetime)) &key (error-p t))
  (setf call (string-upcase (string-trim '(#\Space #\Tab) call)))
  (if (entity-information-available-p)
      (labels ((gett (call)
		 (if-let ((it (get-exception call datetime)))
		   it
		   (cond ((ends-with-subseq "/MM" call)
			  (make-instance 'entity
					 :name "Maritime Mobile"
					 :adif "999"))
			 ((ends-with-subseq "/QRP" call)
			  (get-entity (subseq call 0 (- (length call) 4)) datetime))
			 ((or (ends-with-subseq "/P" call)
			      (ends-with-subseq "/M" call)
			      (ends-with-subseq "/A" call)
			      (ends-with-subseq "/B" call))
			  (get-entity (subseq call 0 (- (length call) 2)) datetime))
			 (t
			  (let ((best '(nil 0)))
			    (dolist (part (split #\/ call))
			      (when-let ((result (e-trie-get *entity-trie* call part datetime)))
				(let ((ratio (/ (length (entity-prefix result)) (length part))))
				  (when (> ratio (cadr best))
				    (setf best (list result ratio))))))
			    (if (null (car best))
				(if error-p
				    (error 'entity-not-found-error :call
					   call)
				    nil)
				(car best))))))))
	(if (sane-callsign-p call)
	    (if-let ((it (get-invalid call datetime)))
	      it
	      (gett call))
	    nil))
      nil))
  
(defun load-entity-information ()
  (setf *entity-trie* nil)
  (let ((file (get-config "core.cty-xml")))
    (if (probe-file file)
	(progn
	  (setf *entity-trie* (parse-cty file))
	  (load-galosh-entity-information)
	  t)	 ; Return t to prevent time-consuming printing of full
		 ; trie when running under slime.
	:failed)))

(defun predicate (prefix predicate)
  "Add a predicate to the trie.  If a predicate is attached to a trie node,
that node will only match if the predicate evalulates to true."
  (setf (e-trie-get-predicate *entity-trie* prefix) predicate))

(defun filter (prefix filter)
  "Add a filter function to the trie, which will be applied to all calls
under the specified prefix unless overwritten further down the trie."
  (if (listp prefix)
      (dolist (p prefix)
	(setf (e-trie-get-filter *entity-trie* p) filter))
      (setf (e-trie-get-filter *entity-trie* prefix) filter)))

(defun load-galosh-entity-information ()
  (let ((file (get-config "core.cty-lisp")))
    (if (probe-file file)
	(let ((*package* (find-package 'galosh-lisp)))
	  (load file)))))
      
(defun entity-information-available-p ()
  (cond ((eq *entity-trie* :not-tried)
	 (load-entity-information)
	 (entity-information-available-p))
	((trie-p *entity-trie*) t)
	(t nil)))

(defun get-datetime ()
  (multiple-value-bind
	(second minute hour date month year)
      (decode-universal-time (+ (get-universal-time) -14400) 0)
    (format nil "~A-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D+00:00" year month date hour minute second)))
