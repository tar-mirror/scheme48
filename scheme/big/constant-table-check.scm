; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber

(define-test-suite constant-tables-tests)

(define (check-table entries hash-function)
  (let ((table (make-constant-table entries hash-function)))
    (for-each (lambda (p)
		(check (constant-table-lookup table (car p))
		       => (cdr p)))
	      entries)))
	    
(define-test-case simple constant-tables-tests
  (check-table '((foo . 1) (bar . 2) (baz . 3) (bala . 4))
	       symbol-hash))

(define-test-case not-present constant-tables-tests
  (let ((table (make-constant-table '((foo . 1) (bar . 2) (baz . 3) (bala . 4)) 
				    symbol-hash)))
    (check-that (constant-table-lookup table 'yellow)
		(is-false))
    (check-that (constant-table-lookup table 'balab)
		(is-false))
    (check-that (constant-table-lookup table 'foobar)
		(is-false))
    (check-that (constant-table-lookup table 'foobarbaz)
		(is-false))))

(define-test-case bigger constant-tables-tests
  (let loop ((i 0) (entries '()))
    (if (= i 1000)
	(check-table entries symbol-hash)
	(loop (+ 1 i)
	      (cons (cons (string->symbol (number->string i))
			  i)
		    entries)))))
