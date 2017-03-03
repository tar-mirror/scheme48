; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber


; This file should be loaded into the bootstrap linker before any use
; of DEFINE-STRUCTURE.  Compare with env/init-defpackage.scm.

(define (evaluate-transformer exp env)
  (if (and (pair? exp)
	   (eq? (car exp) 'syntax-rules))
      
      (if (pair? (cdr exp))
	  (let ((subkeywords (cadr exp))
		(rules (cddr exp)))
	    (if (and (list? subkeywords)
		     (every name? subkeywords))
		(call-with-values
		    (lambda ()
		      (process-rules exp name? (lambda (x) x) eq?))
		  (lambda (code inserted)
		    ;; Pair of the procedure and list of auxiliary names
		    (cons
		     (eval `(let ((transformer ,code))
			      (lambda (exp rename compare) ; turn 4-arg transformer into 3-arg transformer
				(transformer exp name? rename compare)))
			   env)
		     inserted)))
		exp))
	  exp)      
      (eval exp env)))

(define-syntactic-tower-maker
  (lambda (clauses names)
    (let ((env (interaction-environment)))
      (delay
	(begin (if (not (null? clauses))
		   (warn "a FOR-SYNTAX clause appears in a package being linked by the cross-linker"
			 `(for-syntax ,@clauses)))
	       (cons evaluate-transformer env))))))

(define-reader read)

