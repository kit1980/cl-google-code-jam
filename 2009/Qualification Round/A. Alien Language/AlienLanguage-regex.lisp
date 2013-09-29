;; Google Code Jam
;; Qualification Round 2009
;; A. Alien Language
;; https://code.google.com/codejam/contest/90101/dashboard#s=p0
;;
;; Regular expressions based solution in Common Lisp.
;;
;; Author: Sergey Dymchenko <kit1980@gmail.com>
;;
;; SBCL 1.0.29 - http://www.sbcl.org/
;; Quicklisp - http://www.quicklisp.org/
;; cl-ppcre - http://weitz.de/cl-ppcre/
;; Usage:
;; sbcl --noinform --load AlienLanguage-regex.lisp --eval "(main)" < in-file > out-file

(ql:quickload :cl-ppcre)
(use-package :cl-ppcre)

(defparameter *words* nil)

(defun replace-brackets (pattern)
  (regex-replace-all "\\)" (regex-replace-all "\\(" pattern "[") "]") )

(defun count-matches (pattern)
  (let ((scanner (create-scanner pattern)))
    (loop for word in *words* sum
         (if (scan scanner word) 1 0) ) ) )

(defun do-case (case-num pattern)
  (format t "Case #~a: ~a~%" case-num (count-matches (replace-brackets pattern))) )

(defun main ()
  (let (d n)
    (read) ; consume redundant value of L
    (setf d (read))
    (setf n (read))
    (dotimes (i d) (push (read-line) *words*)) ; words are reversed, but it doesn't matter
    (loop for case-num from 1 to n do
         (let (pattern)
           (setf pattern (read-line))
           (do-case case-num pattern) ) ) ) )
