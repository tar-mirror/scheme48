; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees


; Fluid variables

(define (make-fluid val)
  (vector '<fluid> val))

(define (fluid f) (vector-ref f 1))

(define (set-fluid! f val)
  (vector-set! f 1 val))

(define (let-fluid f val thunk)
  (let ((swap (lambda () (let ((temp (fluid f)))
			   (set-fluid! f val)
			   (set! val temp)))))
    (dynamic-wind swap thunk swap)))

(define (let-fluids . args)		;Kind of gross
  (let loop ((args args)
	     (swap (lambda () #f)))
    (if (null? (cdr args))
	(dynamic-wind swap (car args) swap)
	(loop (cddr args)
	      (let ((f (car args))
		    (val (cadr args)))
		(lambda ()
		  (swap)
		  (let ((temp (fluid f)))
		    (set-fluid! f val)
		    (set! val temp))))))))
		

(define (fluid-cell-ref f)
  (cell-ref (fluid f)))

(define (fluid-cell-set! f val)
  (cell-set! (fluid f) val))
