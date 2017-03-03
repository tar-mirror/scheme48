; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey



(define (main)
  (write-number (+ (read-number (current-input-port)) 100)
		(current-output-port))
  0)



