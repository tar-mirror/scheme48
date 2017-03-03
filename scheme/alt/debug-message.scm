; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees

; Really poor man's version

(define (debug-message . stuff)
  (for-each (lambda (thing)
	      (write thing))
	    stuff)
  (newline))
