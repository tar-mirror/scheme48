; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber

; Parameters are like fluids, but support mutation, and have a really
; awkward API.

; Note that the parameter cells are shared among threads, which gives
; us semantics different from, say, MzScheme, but probably the same as
; Gambit-C.

(define *return-fluid* (list 'return-fluid))
(define *return-converter* (list 'return-converter))

(define make-parameter
  (lambda (init . conv)
    (let* ((converter
	    (if (null? conv) (lambda (x) x) (car conv)))
	   (global-cell
	    (make-cell (converter init)))
	   ($fluid (make-fluid global-cell)))
      (letrec ((parameter
		(lambda new-val
		  (let ((cell (fluid $fluid)))
		    (cond ((null? new-val)
			   (cell-ref cell))
			  ((not (null? (cdr new-val)))
			   (apply assertion-violation
				  'make-parameter
				  "parameter object called with more than one argument"
				  parameter new-val))
			  ((eq? (car new-val) *return-fluid*)
			   $fluid)
			  ((eq? (car new-val) *return-converter*)
			   converter)
			  (else
			   (cell-set! cell (converter (car new-val)))))))))
	parameter))))


(define-syntax parameterize
  (syntax-rules ()
    ((parameterize ((?expr1 ?expr2) ...) ?body ...)
     (parameterize-helper ((?expr1 ?expr2) ...) () ?body ...))))
      
(define-syntax parameterize-helper
  (syntax-rules ()
    ((parameterize-helper ((?expr1 ?expr2) ?binding ...) (?args ...) ?body ...)
     (let ((val1 ?expr1)
	   (val2 ?expr2))
       (parameterize-helper (?binding ...)
			    (?args ... 
				   (val1 *return-fluid*) 
				   (make-cell ((val1 *return-converter*) val2)))
			    ?body ...)))
    ((parameterize-helper () (?args ...) ?body ...)
     (let-fluids ?args ... (lambda () ?body ...)))))
