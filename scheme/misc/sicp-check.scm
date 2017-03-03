; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber

(define-test-suite sicp-tests)

(define-test-case get/put sicp-tests
  (check (begin (put 'foo 'prop 'a)
		(get 'foo 'prop)) 
	 => 'a))
