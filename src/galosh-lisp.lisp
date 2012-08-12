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

(in-package :galosh-lisp)

(defparameter *galosh-version* "pre-alpha")

(defvar *galosh-db* nil)

(defparameter *non-tone-modes* '("SSB" "ESSB" "AM"))
(defparameter *default-tone-mode-rst* 599)
(defparameter *default-non-tone-mode-rst* 59)

(defparameter *short-month-names*
  '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sept" "Oct" "Nov" "Dec"))

(defun parent-dir (pathname)
  (make-pathname :directory (butlast (pathname-directory pathname))
                 :defaults pathname))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args)
        (princ (if a a "") s)))))

(setf (logical-pathname-translations "GL")
      `(("RES;**;*.*.*"
         ,(mkstr (merge-pathnames
                   (make-pathname :directory '(:relative "resources"))
                   (parent-dir (ql:where-is-system "galosh-lisp")))
                 "**/*.*"))
        ("USR;**;*.*.*"
         ,(mkstr (merge-pathnames
                   (make-pathname :directory '(:relative ".galosh"))
                   (user-homedir-pathname)) "**/*.*"))))

(defmacro log-fatal (&body body) `(cl-log:log-message :fatal ,@body))

(defmacro log-error (err &optional (fmt "~A") &body body)
  (with-gensyms (e c)
    `(let* ((,e ,err)
	    (,c (if (typep ,e 'symbol)
		    (make-condition ,e)
		    ,e)))
       (cl-log:log-message :error ,fmt ,@body ,c)
       (error ,c))))

(defmacro log-warn  (&body body) `(cl-log:log-message :warn  ,@body))
(defmacro log-info  (&body body) `(cl-log:log-message :info  ,@body))
(defmacro log-debug (&body body) `(cl-log:log-message :debug ,@body))
(defmacro log-trace (&body body) `(cl-log:log-message :trace ,@body))

(defmacro with-safe-io-syntax (&body body)
  `(with-standard-io-syntax
     (let ((*read-eval* nil))
       ,@body)))

(defun ensure-integer (value)
  (if (stringp value)
      (parse-integer value :junk-allowed t)
      value))

(defun parse-float (string &key (start 0) (end nil))
  (cond
    ((floatp string) string)
    ((null string) nil)
    (t (with-safe-io-syntax
         (with-input-from-string (stream string :start start :end end)
           (let ((value (read stream)))
             (check-type value real "a real number")
             value))))))

(defgeneric clone (x))
(defmethod clone ((s string)) (copy-seq s))
(defmethod clone ((x null)) nil)
(defmethod clone ((x number)) x)

(defun mklist (obj)
  (if (listp obj) obj (list obj)))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun symb (&rest args)
   (values (intern (apply #'mkstr args)))))

(defun mkkeyword (&rest args)
  (values (intern (apply #'mkstr args) "KEYWORD")))

(defun single (list)
  (and (consp list) (not (cdr list))))
(proclaim '(inline single))

(defmacro split (sep seq)
  `(split-sequence:split-sequence ,sep ,seq))

(defun split-words (seq &key (first nil))
  (let ((options (reverse (list :remove-empty-subseqs t))))
    (when first
      (setf options (nconc (reverse (list :count first)) options)))
    (apply #'split-sequence:split-sequence  #\Space
	   (string-trim '(#\Space #\Tab #\Newline) seq)
	   (nreverse options))))

(defun last1 (lst)
  (car (last lst)))
(proclaim '(inline last1))

;;;
;;; A series of closure-related utility macros from
;;; Let Over Lambda
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun g!-symbol-p (s)
    (and (symbolp s)
	 (> (length (symbol-name s)) 2)
	 (string= (symbol-name s)
		  "G!"
		  :start1 0
		  :end1 2))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defmacro/g! (name args &rest body)
    (let ((docstring (if (or (null (car body)) (stringp (car body)))
			 (car body)
			 nil))
	  (syms (remove-duplicates
		 (remove-if-not #'g!-symbol-p
				(flatten body)))))
      `(defmacro ,name ,args
	 ,docstring
	 (let ,(mapcar
		(lambda (s)
		  `(,s (gensym ,(subseq
				 (symbol-name s)
				 2))))
		syms)
	   ,@(if docstring
		 (cdr body)
		 body))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun o!-symbol-p (s)
    (and (symbolp s)
	 (> (length (symbol-name s)) 2)
	 (string= (symbol-name s)
		  "O!"
		  :start1 0
		  :end1 2))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun o!-symbol-to-g!-symbol (s)
    (symb "G!"
	  (subseq (symbol-name s) 2))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defmacro! (name args &rest body)
    (let* ((docstring (if (stringp (car body))
			  (car body)
			  nil))
	   (os (remove-if-not #'o!-symbol-p (flatten args)))
	   (gs (mapcar #'o!-symbol-to-g!-symbol os)))
      `(defmacro/g! ,name ,args
	 ,docstring
	 `(let ,(mapcar #'list (list ,@gs) (list ,@os))
	    ,(progn ,@(if docstring
			  (cdr body)
			  body)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro alambda (params &body body)
    `(labels ((self ,params ,@body))
       #'self)))

;;
;; destructuring (or dispatching) lambda
;;
;; See [LoL,p147]
;;
(defmacro! dlambda (&rest ds)
  `(lambda (&rest ,g!args)
     (case (car ,g!args)
       ,@(mapcar
	  (lambda (d)
	    `(,(if (eq t (car d))
		   t
		   (list (car d)))
	       (apply (lambda ,@(cdr d))
		      ,(if (eq t (car d))
			   g!args
			   `(cdr ,g!args)))))
	  ds))))

(defmacro! dalambda (&rest ds)
  `(alambda (&rest ,g!args)
     (case (car ,g!args)
       ,@(mapcar
	  (lambda (d)
	    `(,(if (eq t (car d))
		   t
		   (list (car d)))
	       (apply (lambda ,@(cdr d))
		      ,(if (eq t (car d))
			   g!args
			   `(cdr ,g!args)))))
	  ds))))

(defun empty-string-p (str)
  (zerop (length str)))

(defun string-truncate (string length &key (mark t))
  (if (> (length string) length)
      (if mark
	  (mkstr (subseq string 0 (1- length)) "$")
	  (subseq string 0 length))
      string))

(defun string-right-pad (target-width str &key (padding-element #\Space))
  (if (< (length str) target-width)
      (concatenate 'string str (make-string (- target-width (length str)) :initial-element padding-element))
      str))

(defun n->es (string)
  (if string string ""))

(defun es->n (string)
  (if (empty-string-p string)
      nil
      string))
(proclaim '(inline es->n))

(defmacro cats (&rest strings)
  `(concatenate 'string ,@strings))

(defun join (strings &optional (separator ""))
  (let ((sep (string separator)))
    (with-output-to-string (out)
      (loop for (string . more?) on strings
	 do (write-string string out)
	 when more? do (write-string sep out)))))

(defun say (obj &optional (stream *standard-output*))
  (princ obj stream)
  (fresh-line stream))

(defun lecture (&rest lines)
  (fresh-line *error-output*)
  (dolist (l lines)
    (if (consp l)
	(apply #'format *error-output* l)
	(princ l *error-output*))
    (fresh-line *error-output*)))

(defun open-in-browser (call)
  (sb-ext:run-program "galosh" (list "browser" (mkstr "http://www.qrz.com/db/" call))
		      :wait nil
		      :search (sb-ext:posix-getenv "PATH")))

(defmacro default (place value)
  `(if (null ,place)
       (setf ,place ,value)))

(defun keys (hash)
  (loop for k being the hash-keys of hash collect k))

(defun default-rst-for-mode (mode)
  (if (member mode *non-tone-modes* :test #'string=)
      *default-non-tone-mode-rst*
      *default-tone-mode-rst*))

(defun valid-callsign-char-p (char)
  (or (alphanumericp char) (char= char #\/)))
(proclaim '(inline valid-callsign-char-p))

(defun sane-callsign-p (call)
  (let ((sane nil))
    (dolist (section (split #\/ call))
      (if (cl-ppcre:scan "^[A-Z0-9]{1,3}[0-9][A-Z0-9]*$" section)
	  (setf sane t)))
    sane))

(define-condition missing-galosh-db-error (error)
  ((text :initarg :text :reader text)))

(let ((time-fudge :not-set))
  (defun get-time-fudge ()
    (when (eq time-fudge :not-set)
	(setf time-fudge (parse-integer (get-config "core.time_fudge"))))
    time-fudge))

(defmacro with-galosh-db ((db &key (make-default t)) &body body)
  "Execute BODY with GALOSH-LISP:*GALOSH-DB* set to a CLSQL DATABASE
object representing the SQLite3 database DB.  If :MAKE-DEFAULT is
true (the default) set CLSQL-SYS:*DEFAULT-DATABASE* to the same object.

If DB is not specified or does not exist signal MISSING-GALOSH-DB-ERROR
with two restarts: IGNORE and ABORT.  Invoking IGNORE will execute BODY
without the database binding; ABORT will print an error message and
terminate."
  (once-only (db make-default)
    `(cond (*galosh-db*
             ;; We already have a connection option to the database;
             ;; just make it the default if required.
             (if ,make-default
               (let ((*default-database* *galosh-db*))
                 ,@body)))
           ((and ,db (probe-file ,db))
            ;; We don't already have the database open, but it does exist.
            (unwind-protect
              (progn
                (setf *galosh-db* (connect (list ,db)
                                           :database-type :sqlite3
                                           :make-default ,make-default
                                           :if-exists :old))
                ,@body)
              (disconnect :database *galosh-db*)
              (setf *galosh-db* nil)))
           (t
            ;; No database specified, or it doesn't exist.
            (restart-case
              (error 'missing-galosh-db-error :text
                     (format nil "Could not find database `~A'." ,db))
              (ignore ()
                :report "Continue without log database connection."
                ,@body)
              (abort (e)
                :report "Terminate with error message."
                (lecture (text e)
                         "This probably means that you are not in a Galosh repository."
                         "Most Galosh commands require you to be in a repository; see the manual for"
                         "more information.")
                (terminate 1)))))))

(define-condition missing-galosh-dir-error (error)
  ((text :initarg :text :reader text)))

(defun get-galosh-dir (&key (raise-error nil))
  (let ((gdir (sb-ext:posix-getenv "GALOSH_DIR")))
    (if gdir
	gdir
	(if raise-error
	    (error 'missing-galosh-dir-error :text
		   "GALOSH_DIR is not defined.")))))

(defun fatal-get-galosh-dir ()
  (handler-case
      (get-galosh-dir :raise-error t)
    (missing-galosh-dir-error (e)
      (lecture (text e)
	       "This probably means that you are not in a Galosh repository."
	       "Most Galosh commands require you to be in a repository; see the manual for"
	       "more information.")
      (terminate 1))))

(defun human-date (date &key (separator "-"))
  (if (= (length date) 8)
      (progn
	(format nil "~A~A~A~A~A"
		(subseq date 6 8)
		separator
		(nth (- (parse-integer date :start 4 :end 6) 1) *short-month-names*)
		separator
		(subseq date 0 4)))))

(defun log-date-time ()
  (multiple-value-bind
        (second minute hour date month year)
      (decode-universal-time (+ (get-universal-time ) (get-time-fudge)) 0)
    (values
     (format nil "~2,'0d~2,'0d~2,'0d" year month date)
     (format nil "~2,'0d~2,'02,'0d~2,'0d" hour minute second))))

(defun log-date ()
  (multiple-value-bind (date) (log-date-time)
    date))

(defun log-time ()
  (multiple-value-bind (date time) (log-date-time)
    (declare (ignore date))
    time))

(defun qrg->band (qrg)
  (labels ((between (lower upper)
	     (and (>= qrg lower) (<= qrg upper))))
    (cond ((< qrg 1800000) "DC")
          ((between 1800000 2000000) "160m")
	  ((between 3500000 4000000) "80m")
	  ((between 5330500 5403500) "60m")
	  ((between 7000000 7400000) "40m")
	  ((between 10100000 10150000) "30m")
	  ((between 14000000 14350000) "20m")
	  ((between 18068000 18168000) "17m")
	  ((between 21000000 21450000) "15m")
	  ((between 24890000 24990000) "12m")
	  ((between 28000000 29700000) "10m")
	  ((between 50000000 54000000) "6m")
	  ((between 144000000 148000000) "2m")
	  ((between 219000000 225000000) "1.25m")
	  ((between 420000000 450000000) "70cm")
	  ((between 902000000 928000000) "33cm")
	  ((between 1240000000 1300000000) "23cm")
	  ((> qrg 1300000000) "Daylight")
	  (t "Unknown band"))))

(define-condition missing-mandatory-configuration-error (error)
  ((text :initarg :text :reader text)
   (variables :initarg :variables :reader variables)))

(defvar *config* nil)

(defun set-defaults (config)
  (let ((home-galosh-dir (merge-pathnames (make-pathname :directory '(:relative ".galosh"))
					  (user-homedir-pathname)))
	(context (get-galosh-dir)))
    (with-input-from-string (s (join
				(remove-if #'null (list "[core]"
							(if context
							  (cats "log = " (namestring (merge-pathnames "log.db" context))))
							(cats "debug-log = " (namestring (merge-pathnames "debug.log" (if context context home-galosh-dir))))
							"time_fudge = 0"
							(cats "cty-xml = " (namestring (merge-pathnames "cty.xml" home-galosh-dir)))
							(cats "cty-lisp = " (namestring (merge-pathnames "cty.lisp" home-galosh-dir)))
							"[log]"
							(if context
							    (cats "attic = " (namestring (merge-pathnames "log.attic" context))))
							"[qrz]"
							(cats "offlinedb = " (namestring (merge-pathnames "qrz.db" home-galosh-dir)))))
				#\Newline))
      (setf config (read-stream config s)))))

(defun check-required-config (variables)
  (let ((missing ()))
    (dolist (ropt variables)
      (destructuring-bind (section option) (split #\. ropt)
	(unless (and (has-section-p *config* section)
		     (has-option-p *config* section option))
	  (setf missing (cons ropt missing)))))
    (if missing
	(error 'missing-mandatory-configuration-error :variables missing)
	t)))

(defun has-config-p (name)
  (unless *config* (init-config))
  (destructuring-bind (section option) (split #\. name)
    (and (has-section-p *config* section)
	 (has-option-p *config* section option))))

(defun get-config (name &key (default nil default-p))
  (unless *config* (init-config))
  (handler-case
      (destructuring-bind (section option) (split #\. name)
	(coerce (get-option *config* section option) 'simple-string))
    (t (e) (if default-p
	       default
	       (error e)))))

(defun init-config ()
  (let ((global-config (merge-pathnames (make-pathname :directory '(:relative ".galosh") :name "config") (user-homedir-pathname))))
    (setf *config* (set-defaults (make-config)))
    (read-files *config* (list global-config))
    (ignore-errors
      (read-files *config* (list (make-pathname :directory (get-galosh-dir :raise-error t)
                                                :name "config"))))))

(defmacro define-galosh-command (name (&key (require-config nil)
                                            (require-db t)) &body body)
  (let ((package (symbol-package name)))
    `(defun ,(intern "MAIN" (symbol-name name)) (,(intern "ARGV" (symbol-name name)))
       (generic-galosh-command (lambda () ,@body)
                               :name ,(package-name package)
                               :require-db ,require-db
                               :require-config ,require-config))))

(defun generic-galosh-command (callback
                                &key name require-db require-config)
  (flet ((load-mixins ()
           (when name
             (let* ((mixin-repo-path (make-pathname :directory (get-galosh-dir)
                                                    :name name
                                                    :type "mixin"))
                    (mixin-repo-true (ignore-errors (truename mixin-repo-path)))
                    (mixin-global-path (logical-pathname
                                         (mkstr "GL:USR;" name ".mixin")))
                    (mixin-global-true (ignore-errors
                                         (truename mixin-global-path)))
                    (*package* (find-package (string-upcase name))))
               (if mixin-global-true
                 (load mixin-global-true)
                 (log-debug (format nil "Global mixin ~A not found."
                                    mixin-global-path)))
               (if (and mixin-repo-true
                        (not (equal mixin-global-true
                                    mixin-repo-true)))
                 (progn
                   (log-debug (format nil "Repo mixin ~A found. Loading."
                                      mixin-repo-true))
                   (load mixin-repo-true))
                 (log-debug (format nil "Repo mixin ~A not found."
                                    mixin-repo-path)))))))
    (setf (cl-log:log-manager)
          (make-instance 'cl-log:log-manager
            :message-class 'cl-log:formatted-message))
    (cl-log:start-messenger 'cl-log:text-file-messenger
                            :filename (get-config "core.debug-log")
                            :category '(or :warn :error :fatal))
    (init-config)
    (handler-bind
      ((missing-mandatory-configuration-error
         (lambda (e)
           (lecture (list "~(~A~) requires values for the following configuration options:"
                          (or name "LAMBDA"))
                    (list "~{    ~A~&~}" (variables e))
                    "See the Configuration section of the Galosh manual for more information.")
           (terminate 1)))
       (missing-galosh-db-error
         (lambda (e)
           (if require-db
             (invoke-restart 'abort e)
             (invoke-restart 'ignore)))))
      (load-mixins)
      (check-required-config require-config)
      (with-galosh-db ((get-config "core.log" :default nil))
        (funcall callback)))))

(defun terminate (&optional (status 0))
  #+sbcl     (sb-ext:quit      :unix-status status)    ; SBCL
  #+ccl      (   ccl:quit      status)                 ; Clozure CL
  #+clisp    (   ext:quit      status)                 ; GNU CLISP
  #+cmu      (  unix:unix-exit status)                 ; CMUCL
  #+abcl     (   ext:quit      :status status)         ; Armed Bear CL
  #+allegro  (  excl:exit      status :quiet t)        ; Allegro CL
  (cl-user::quit))           ; Many implementations put QUIT in the sandbox CL-USER package.

;;
;; A simplistic function to convert a Maidenhead locator to a list of
;; (lat, long) as decimal degrees.  The method is taken from the
;; Wikipedia page on Maidenhead locators, at
;; http://en.wikipedia.org/wiki/Maidenhead_Locator_System
;;
(defun locator->decdeg (l)
  "Convert a Maidenhead locator to latitude and longitude as decimal degrees."
  ;; Field
  (let ((long (* (- (char-code (char (string-upcase l) 0)) (char-code #\A)) 20))
	(lat  (* (- (char-code (char (string-upcase l) 1)) (char-code #\A)) 10)))
    ;; Square
    (when (> (length l) 2)
      (setf long (+ long (* (parse-integer (string (char l 2))) 2))
	    lat  (+ lat     (parse-integer (string (char l 3))))))
    ;; Subsquare
    (when (> (length l) 4)
      (setf long (+ long (* (- (char-code (char (string-upcase l) 4)) (char-code #\A)) 5/60))
	    lat  (+ lat  (* (- (char-code (char (string-upcase l) 5)) (char-code #\A)) 5/120))))
    (list (float (- lat 90)) (float (- long 180)))))

(defun rad->deg (x)
  "Convert radians to decimal degrees."
  (* x (/ 180 pi)))

(defun deg->rad (x)
  "Convert decimal degrees to radians."
  (* x (/ pi  180)))

;;
;; Calculate the great-circle heading between two points.  Points may
;; be specificed either as Maidenhead locators or as lists of
;; (lat long).  The actual work is farmed out to great-circle-heading*
;; after inputs have been sanitized.
;;
;; The algorithm used was taken from:
;;  http://en.wikipedia.org/wiki/Great-circle_navigation and
;;  http://www.movable-type.co.uk/scripts/latlong.html
;;
(defun great-circle-heading (from to)
  "Calculate the great-circle heading between two points, specified either as
   Maidenhead locators or as lists of (lat, long) in decimal degrees."
  (labels ((convert-grid (o) (if (stringp o)
				  (locator->decdeg o)
				  o)))
    (let ((heading (great-circle-heading* (convert-grid from) (convert-grid to))))
      (values heading (if (<= 180 heading) (- heading 180) (+ heading 180))))))

(defun great-circle-heading* (from to)
  (labels ((norm (x) (mod (+ x 360) 360)))
    (let* ((theta-s (deg->rad (first from)))
	   (lambda-s (deg->rad (second from)))
	   (theta-f (deg->rad (first to)))
	   (lambda-f (deg->rad (second to)))
	   (delta-long (- lambda-f lambda-s))
	   (SA (* (cos theta-f) (sin delta-long)))
	   (SB (- (* (cos theta-s) (sin theta-f))
		  (* (sin theta-s) (cos theta-f) (cos delta-long)))))
      (norm (rad->deg (atan SA SB))))))

;; NB: this macro will currently fail if a clause is based on a function
;; that returns a list; this will cause it to go down the funcall route
;; rather than the member route.  Not sure if that is a problem or not.
(defmacro given (keyform test &body clauses)
  (with-gensyms (k)
    (let ((acc '(cond)))
      (dolist (form clauses)
	(if (eq (car form) t)
	    (setf acc (append acc `((t ,@(cdr form)))))
	    (if (and (consp (car form)) (eq (caar form) 'quote))
		(setf acc (append acc `(((member ,k ,(car form) :test ,test) ,@(cdr form)))))
		(setf acc (append acc `(((funcall ,test ,k ,(car form)) ,@(cdr form))))))))
      (append `(let ((,k ,keyform)))
		 (list acc)))))

(defun drop-last (str)
  (if (> (length str) 1)
      (subseq str 0 (- (length str) 1))
      ""))

(defun kill-last-word (str)
  (let ((index (position #\Space (string-right-trim '(#\Space #\Tab #\Newline) str) :from-end t)))
    (if index
	(values (subseq str 0 (+ index 1)) (subseq str (+ index 1)))
	(values "" nil))))

(defmacro! until= (o!test &body body)
  "Execute BODY until it returns TEST"
  `(let (,g!result)
     (tagbody ,g!start-tag
	(setf ,g!result ,@body)
	(if (equal ,g!result ,g!test)
	    ,g!result
	    (go ,g!start-tag)))))

(defmacro cbind (bindings &body body)
  "CBIND ({(binding value)}*) form*

Analagous to LET; binds the closure results of evaluating VALUES to
BINDINGS for the evaluation of FORMS.

  (cbind ((foo (lambda (x) (+ x 2))))
    (foo 4))"
  (loop for b in bindings
       as g = (gensym)
       collecting `(,g ,@(cdr b)) into binding-forms
       collecting `(,(car b) (&rest args) (apply ,g args)) into flets
       collecting (car b) into inline
       collecting g into functions
       finally (return
		 `(let (,@binding-forms)
		    (declare (function ,@functions))
		    (flet (,@flets)
		      (declare (inline ,@inline))
		      ,@body)))))
