;; Google Code Jam
;; Qualification Round 2012
;; A. Speaking in Tongues
;; https://code.google.com/codejam/contest/1460488/dashboard#s=p0
;;
;; Algorithmic solution in Common Lisp.
;; Author: Sergey Dymchenko <kit1980@gmail.com>
;;
;; SBCL 1.0.29 - http://www.sbcl.org/
;; Usage:
;; sbcl --noinform --load SpeakingInTongues.lisp --eval "(main)" < in-file > out-file

;; Additionally to the sample input/output, 
;; figure 'q' -> 'z' (Googlerese -> English) mapping from the problem statement.
;; Finally, 'z' -> 'q' is the only left possibility.
(defparameter *in-sample* "
ejp mysljylc kd kxveddknmc re jsicpdrysi
rbcpc ypc rtcsra dkh wyfrepkym veddknkmkrkcd
de kr kd eoya kw aej tysr re ujdr lkgc jv
qz
")
(defparameter *out-sample* "
our language is impossible to understand
there are twenty six factorial possibilities
so it is okay if you want to just give up
zq
")

(defparameter *mapping* (make-hash-table :test #'equal))

(defun initialize-mapping ()
  (loop for in-char across *in-sample* for out-char across *out-sample* do
       (setf (gethash in-char *mapping*) out-char) ) )

(defun translate (in-string)
  (concatenate 'string 
               (loop for in-char across in-string collect 
                    (gethash in-char *mapping*) ) ) )

(defun do-case (case-num in-string)
  (format t "Case #~a: ~a~%" case-num (translate in-string)) )

(defun main ()
  (initialize-mapping)
  (loop for case-num from 1 to (read) do
       (let (in-string) 
         (setf in-string (read-line))
         (do-case case-num in-string) ) ) )
