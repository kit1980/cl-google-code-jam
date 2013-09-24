;; Google Code Jam
;; Round 1A 2008
;; A. Minimum Scalar Product
;; https://code.google.com/codejam/contest/dashboard?c=32016#s=p0
;;
;; Algorithmic solution in Common Lisp.
;; Author: Sergey Dymchenko <kit1980@gmail.com>
;;
;; SBCL 1.0.29 - http://www.sbcl.org/
;; Usage:
;; sbcl --noinform --load MinimumScalarProduct.lisp --eval "(main)" < in-file > out-file

(defun dot-product (x y)
  (reduce '+ (mapcar '* x y)) )

(defun min-product (x y)
  (dot-product (sort x #'<) (sort y #'>)) )

(defun do-case (case-num x y)
  (format t "Case #~a: ~a~%" case-num (min-product x y)) )

(defun main ()
  (loop for case-num from 1 to (read) do
       (let (n x y) 
         (setf n (read))
         (setf x (loop repeat n collect (read)))
         (setf y (loop repeat n collect (read)))
         (do-case case-num x y) ) ) )
