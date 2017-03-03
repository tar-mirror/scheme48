; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber

; Optimizers

(define optimizers-table (make-table))

(define (set-optimizer! name opt)
  (table-set! optimizers-table name opt))

(define (get-optimizer names)
  (lambda (forms package)
    (apply-optimizers (map (lambda (name)
			     (or (table-ref optimizers-table name)
				 (begin
				   (note 'get-optimizer
					 "optional optimization pass not invoked"
					 name)
				   (lambda (forms package) forms))))
			   names)
		      forms
		      package)))

(define (apply-optimizers optimizers forms package)
  (fold (lambda (optimizer forms)
	  (optimizer forms package))
	optimizers
	forms))

