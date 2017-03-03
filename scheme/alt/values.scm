; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees


; Multiple return values

(define multiple-value-token (vector 'multiple-value-token))

(define (values . things)
  (if (and (pair? things)
	   (null? (cdr things)))
      (car things)
      (cons multiple-value-token things)))

(define (call-with-values producer consumer)
  (let ((things (producer)))
    (if (and (pair? things)
	     (eq? (car things) multiple-value-token))
	(apply consumer (cdr things))
	(consumer things))))
