; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber


; ,config ,load debug/test.scm

(define-structure testing (export (test :syntax) lost?)
  (open scheme handle conditions)
  (open i/o)
  (begin

(define *lost?* #f)
(define (lost?) *lost?*)

(define (run-test string compare want thunk)
  (let ((out (current-error-port)))
    (display "[" out)
    (display string out)
    (force-output out)
  (let ((result
	 (call-with-current-continuation
	   (lambda (k)
	     (with-handler (lambda (condition punt)
			     (if (serious-condition? condition)
				 (k condition)
				 (punt)))
	       thunk)))))
    (if (not (compare want result))
	(begin (display "Test ") (write string) (display " failed.") (newline)
	       (display "Wanted ") (write want)
	       (display ", but got ") (write result) (display ".")
	       (newline)
	       (set! *lost?* #t))))
  (display "]" out) (newline out)))

(define-syntax test
  (syntax-rules ()
    ((test ?string ?compare ?want ?exp)
     (run-test ?string ?compare ?want (lambda () ?exp)))))

))
