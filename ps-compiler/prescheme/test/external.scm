; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey


(define foo
  (external (=> (int32 int32) unit)
	    (lambda (x y)
	      (display (+ x y)))))

(define (test)
  (foo 3 4))
