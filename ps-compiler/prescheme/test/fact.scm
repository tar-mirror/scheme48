; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey



(define *one* 1)

(define (fact n)
  (let loop ((i n) (r *one*))
    (if (<= *one* i)
	(loop (- i *one*) (* i r))
	r)))

(define (all)
  (set! *one* (fact (if (> (fact 10) 100) 10 20))))
