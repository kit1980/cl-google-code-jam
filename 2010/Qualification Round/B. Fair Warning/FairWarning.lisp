;; Google Code Jam
;; Qualification Round 2010
;; B. Fair Warning
;; https://code.google.com/codejam/contest/433101/dashboard#s=p1
;;
;; Functional style algorithmic solution in Common Lisp.
;; Author: Sergey Dymchenko <kit1980@gmail.com>
;;
;; SBCL 1.0.29 - http://www.sbcl.org/
;; Quicklisp - http://www.quicklisp.org/
;; iterate - http://common-lisp.net/project/iterate/
;; Usage:
;; sbcl --noinform --load FairWarning.lisp --eval "(main)" < in-file > out-file

(ql:quickload :iterate)
(use-package :iterate)

(defun substract-val (items val)
  (mapcar #'(lambda (x) (- x val)) items) )

(defun first-optimum (events)
  (let* ((min (reduce #'min events)) 
         (gcd (reduce #'gcd (substract-val events min)))
         (rem (rem min gcd)) )
    (if (zerop rem) 0 (- gcd rem)) ) )
    
(defun do-case (case-num events)
  (format t "Case #~a: ~a~%" case-num (first-optimum events)) )

(defun main ()
  (iterate (for case-num from 1 to (read))
           (let (n events) 
             (setf n (read))
             (setf events (iterate (repeat n) (collect (read))))
             (do-case case-num events) ) ) )
