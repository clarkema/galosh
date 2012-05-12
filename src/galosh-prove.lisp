;;;; galosh -- amateur radio utilities.
;;;; Copyright (C) 2012 Michael Clarke, M0PRL
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

(defpackage :galosh-prove
  (:use :cl :galosh-lisp)
  (:export #:prove #:is #:signals #:deftest))
(in-package :galosh-prove)

(defun ok (number &optional (description ""))
  (format t "ok ~A - ~A~%" number description))

(defun notok (number &optional (description "") (fail-comment ""))
  (princ "not ")
  (ok number description)
  (if fail-comment
    (comment fail-comment)))

(defun comment (string)
  (fresh-line)
  (princ "#     Failed test: ")
  (princ string)
  (terpri))

(defmacro! is (form o!expected &optional (o!description ""))
  `(let ((,g!count (incf *prove-count*))
         (,g!result ,form))
     (if (equal ,g!result ,g!expected)
         (ok ,g!count ,g!description)
         (notok ,g!count ,g!description
                (format nil "Expected ~S, but got ~S" ,g!expected ,g!result)))))

(defmacro deftest (name &rest tests)
  `(defun ,(symb name "-PROVE") ()
     (let ((*prove-count* 0))
       (declare (special *prove-count*))
       (format t "1..~A~%" ,(length tests))
       ,@tests)))

(defmacro! signals (form expected &optional (o!description ""))
  `(let ((,g!count (incf *prove-count*)))
     (handler-case
       (progn
         ,form
         (notok ,g!count ,g!description
                (format nil "Expected signal ~A, but completed successfully."
                        ',expected)))
       (,expected (e) (ok ,g!count ,g!description))
       (t (e) (notok ,g!count ,g!description
                     (format nil "Expected signal ~A, but received ~S"
                             ',expected e))))))

; (defmacro! like -- apply regex to scalar result

(defun prove (package)
  (let ((*package* (if (stringp package)
                       (find-package (string-upcase package))
                       package)))
    (do-symbols (s)
      (let ((sname (mkstr s)))
        (when (and (> (length sname) 6)
                   (equal (subseq sname (- (length sname) 6)) "-PROVE")
                   (fboundp s))
          (funcall (symbol-function s)))))))
