; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber


; This file has to be loaded into the initial-image before any use of
; DEFINE-STRUCTURE.  Compare with alt/init-defpackage.scm.

; The procedure given to DEFINE-SYNTACTIC-TOWER-MAKER is called when
; a DEFINE-STRUCTURE form is evaluated.

(define-syntactic-tower-maker
  (let ((comp-env-macro-eval
	  (*structure-ref compiler-envs 'comp-env-macro-eval))
	(make-simple-interface
	  (*structure-ref interfaces 'make-simple-interface))
	(env (interaction-environment)))
    (lambda (clauses id)
      (if (null? clauses)
	  ;; (make-syntactic-tower eval (list scheme) id)
	  (comp-env-macro-eval (package->environment env))
	  (delay
	    (let ((package (eval `(a-package ,(if id
						  `(for-syntax ,id)
						  '(for-syntax))
					     ,@clauses)
				 env)))
	      (ensure-loaded (make-structure package
					     (lambda ()
					       (make-simple-interface #f '()))
					     'for-syntax))
	      (cons eval package)))))))

(define-reader read)

