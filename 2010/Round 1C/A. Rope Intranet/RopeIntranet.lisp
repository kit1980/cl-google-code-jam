;; Google Code Jam
;; Round 1C 2010
;; A. Rope Intranet
;; http://code.google.com/codejam/contest/619102/dashboard#s=p0
;;
;; Algorithmic solution in Common Lisp using cl-geometry library.
;; Author: Sergey Dymchenko <kit1980@gmail.com>
;;
;; SBCL 1.0.29 - http://www.sbcl.org/
;; Quicklisp - http://www.quicklisp.org/
;; cl-geometry - https://github.com/Ramarren/cl-geometry
;; Usage:
;; sbcl --noinform --load RopeIntranet.lisp --eval "(main)" < in-file > out-file

(ql:quickload :cl-geometry)
(use-package :2d-geometry)

(defun count-intersections (a-list b-list)
  (let (n a b)
    (setf n (length a-list))
    (setf a (make-array n :initial-contents a-list))
    (setf b (make-array n :initial-contents b-list))
    (loop for i from 0 below n sum
      (loop for j from (1+ i) below n sum
           (let (li lj inter)
             (setf li (make-instance 'line-segment 
                                     :start (make-instance 'point :x 0 :y (aref a i)) 
                                     :end (make-instance 'point :x 1 :y (aref b i)) ))
             (setf lj (make-instance 'line-segment 
                                     :start (make-instance 'point :x 0 :y (aref a j)) 
                                     :end (make-instance 'point :x 1 :y (aref b j)) ))
             (setf inter (line-segments-intersection-point li lj))
             (if inter 1 0)) ) ) ) )

(defun do-case (case-num a b)
  (format t "Case #~a: ~a~%" case-num (count-intersections a b)) )

(defun main ()
  (loop for case-num from 1 to (read) do
       (let (n a b) 
         (setf n (read))
         (dotimes (i n) 
              (push (read) a)
              (push (read) b) )
         ; items in a & b are reversed, but it doesn't matter
         (do-case case-num a b) ) ) )
