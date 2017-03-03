; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey


(define (foo x y z)
  (if (and (= (bitwise-and x 3) 0)
	   (= (bitwise-and y 3) 0)
	   (= (bitwise-and z 3) 0))
      (ashl (ashr x 2) 2)
      (ashl y (ashr z 2))))

(define-local-syntax (define-primitive id nargs)
  (let ((args (reverse (list-tail '(z y x) (- '3 nargs)))))
    `(define (,id . ,args)
       (call-primitively ,id . ,args))))

(define-primitive = 2)
(define-primitive bitwise-and 2)
(define-primitive ashl 2)
(define-primitive ashr 2)
