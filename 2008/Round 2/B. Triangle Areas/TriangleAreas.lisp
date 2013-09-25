;; Google Code Jam
;; Round 1A 2008
;; A. Minimum Scalar Product
;; https://code.google.com/codejam/contest/dashboard?c=32016#s=p0
;;
;; Common Lisp driver script to call ECLiPSe Prolog program TriangleAreas.ecl.
;; Prolog program does actual work.
;; Author: Sergey Dymchenko <kit1980@gmail.com>
;;
;; SBCL 1.0.29 - http://www.sbcl.org/
;; Quicklisp - http://www.quicklisp.org/
;; trivial-shell - http://common-lisp.net/project/trivial-shell/
;; ECLiPSe - http://www.eclipseclp.org/
;; Usage:
;; sbcl --noinform --load TriangleAreas.lisp --eval "(main)" < in-file > out-file

(ql:quickload :trivial-shell)

(defun prologify (items)
  (format nil "[~{~A~^, ~}].~%" items) )

(defun run-eclipse (input)
  (trivial-shell:shell-command "eclipse -f TriangleAreas.ecl -e main" :input input) )

(defun main ()
  (format t (run-eclipse (with-output-to-string (str)
                           (let (c)
                             (setf c (read))
                             (format str (prologify (list c)))
                             (loop for case-num from 1 to c do
                                  (let (n m a) 
                                    (setf n (read))
                                    (setf m (read))
                                    (setf a (read))
                                    (format str (prologify (list n m a))) ) ) ) ) ) ) )
