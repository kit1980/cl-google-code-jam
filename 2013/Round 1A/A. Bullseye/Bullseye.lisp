;; Google Code Jam
;; Round 1A 2013
;; A. Bullseye
;; https://code.google.com/codejam/contest/2418487/dashboard#s=p0
;;
;; Binary search solution in Common Lisp using infix math syntax.
;; Author: Sergey Dymchenko <kit1980@gmail.com>
;;
;; SBCL 1.0.29 - http://www.sbcl.org/
;; Quicklisp - http://www.quicklisp.org/
;; infix from FEMLISP - http://www.femlisp.org/
;; Usage:
;; sbcl --noinform --load Bullseye.lisp --eval "(main)" < in-file > out-file

(ql:quickload :infix)

(defun paint (r n)
    #I(2*r*n + 2*n*n - n) )

(defun find-max (r paint)
  (let (min_n max_n med_n)
    #I(min_n = 0)
    #I(max_n = paint + 1)
    (loop do
         #I(med_n = ((max_n - min_n) >> 1) + min_n)
         (if #I(paint(r, med_n) <= paint)
             #I(min_n = med_n) 
             #I(max_n = med_n) )
       while #I(max_n - min_n > 1) )
    min_n ) )

(defun do-case (case-num r paint)
  (format t "Case #~a: ~a~%" case-num (find-max r paint)) )

(defun main ()
  (loop for case-num from 1 to (read) do
       (let (r paint) 
         (setf r (read))
         (setf paint (read))
         (do-case case-num r paint) ) ) )
