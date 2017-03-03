; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey



(define (odd? x)
  (cond ((= x 0)
	 #f)
;	((= x 100)
;	 (foo))
	(else
	 (goto even? (- x 1)))))

(define (even? x)
  (if (= x 0)
      #t
      (goto odd? (- x 1))))

(define (test x)
  (if (odd? (+ x 1))
      (error "an even number" x)))
