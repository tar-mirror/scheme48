; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey



(define (foo x y)
  (let ((bar (lambda (y)
	       (let ((baz (lambda (a b)
			    (foo (- a 2) (+ 3 b)))))
		 (+ (baz y 1) (baz (+ y 1) 2))))))
    (if (= x 0)
	(goto bar 10)
	(goto bar 4))))

(define (test)
  (foo 30 40))
