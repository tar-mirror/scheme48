; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey



(define (fact n)
  (let loop ((i n) (r 1))
    (if (<= i 1)
	r
	(loop (- i 1) (* i r)))))

(define facts (make-vector 5))

(do ((i 0 (+ i 1)))
    ((< 4 i))
  (vector-set! facts i (fact i)))

(define f4 (vector-ref facts 4))

