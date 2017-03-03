; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Mike Sperber

(define-usual-macro 'syntax-rules
  (make-explicit-renaming-transformer/4
   (lambda (exp name? r c)
     (let ((%quote        (r 'quote))
	   (%cons         (r 'cons))
	   (%make-explicit-renaming-transformer/4 (r 'make-explicit-renaming-transformer/4)))
       
       (if (pair? (cdr exp))
	   (let ((subkeywords (cadr exp))
		 (rules (cddr exp)))
	     (if (and (list? subkeywords)
		      (every name? subkeywords))
		 (receive (code inserted)
		     (process-rules exp name? r c)
		   (if code
		       `(,%cons (,%make-explicit-renaming-transformer/4 ,code)
				(,%quote ,inserted)) ; should this be code-quote?
		       exp))
		 exp))
	   exp))))
  '(cons lambda code-quote make-explicit-renaming-transformer/4 apply-rules))

(define (process-rules exp name? r c)
  (let ((%quote        (r 'quote))
	(%code-quote   (r 'code-quote))

	(%cons         (r 'cons))
	(%lambda       (r 'lambda))
	(%apply-rules  (r 'apply-rules))

	(%input        (r 'input))
	(%name?	       (r 'name?))
	(%rename       (r 'rename))
	(%compare      (r 'compare)))
    (receive (compiled inserted)
	(compile-rules exp
		       (lambda (n)
			 (and (name? n)
			      (c n (r '...)))))
      (if compiled
	  (values `(,%lambda (,%input ,%name? ,%rename ,%compare)
		     (,%apply-rules ,%input
				    (,%code-quote ,compiled)
				    ,%name?
				    ,%rename
				    ,%compare))
		  inserted)
	  (values #f #f)))))

