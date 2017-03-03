; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Will Noble

; Transforms

; A transform represents a source-to-source rewrite rule: either a
; macro or an in-line procedure.

(define-record-type transform :transform
  (really-make-transform kind xformer env type aux-names source id)
  transform?
  ;; macro or inline
  (kind      transform-kind)
  (xformer   transform-procedure)
  (env	     transform-env)
  (type	     transform-type)
  (aux-names transform-aux-names) ;for reification
  (source    transform-source)    ;for reification
  (id	     transform-id))

(define (make-transform/macro thing env type source id)
  (let ((type (if (or (pair? type)
		      (symbol? type))
		  (sexp->type type #t)
		  type)))
    (call-with-values
	(lambda ()
	  (if (pair? thing)
	      (values (car thing) (cdr thing))
	      (values thing #f)))
      (lambda (transformer aux-names)
	;; The usual old-style transformers take 3 args: exp rename compare.
	;; However, syntax-rules-generated transformers need a 4th arg, name?.
	;; Distinguish between the two kinds.
	(let ((proc
	       (cond
		((explicit-renaming-transformer/4? transformer)
		 (explicit-renaming-transformer/4-proc transformer))
		(else ; standard explicit-renaming transformers take only 3 args
		 (lambda (exp name? rename compare)
		   (transformer exp rename compare))))))
	  (make-immutable!
	   (really-make-transform 'macro proc env type aux-names source id)))))))

; for backwards compatibility with the PreScheme compiler
(define make-transform make-transform/macro)

(define (make-transform/inline thing env type source id)
  (let ((type (if (or (pair? type)
		      (symbol? type))
		  (sexp->type type #t)
		  type)))
    (make-immutable!
     (really-make-transform 'inline (car thing) env type (cdr thing) source id))))

(define-record-discloser :transform
  (lambda (m) (list 'transform (transform-id m))))

; See also: Rees, "Implementing Lexically Scoped Macros",
; Lisp Pointers VI(1), January-March 1993
(define (maybe-apply-macro-transform transform exp parent-name env-of-use)
  (let* ((token (cons #f #f))
	 (new-env (bind-aliases token transform env-of-use))
	 (rename (make-name-generator (transform-env transform)
				      token
				      parent-name))
	 (compare (make-keyword-comparator new-env)))
    (values ((transform-procedure transform) exp name? rename compare)
	    new-env)))

(define (apply-inline-transform transform exp parent-name)
  (let* ((env (transform-env transform))
	 (rename (make-name-generator env (cons #f #f) parent-name)))
    ((transform-procedure transform) exp env rename)))

; Two keywords are the same if:
;  - they really are the same
;  - neither one is bound and they have the same symbol in the source
;  - they are bound to the same denotation (macro or location or ...)

(define (make-keyword-comparator environment)
  (lambda (name1 name2)
    (or (eqv? name1 name2)
	(and (name? name1)	; why might they not be names?
	     (name? name2)
	     (let ((v1 (lookup environment name1))
		   (v2 (lookup environment name2)))
	       (if v1
		   (and v2 (same-denotation? v1 v2))
		   (and (not v2)
			(equal? (name->source-name name1)
				(name->source-name name2)))))))))

; Get the name that appeared in the source.

(define (name->source-name name)
  (if (generated? name)
      (name->source-name (generated-name name))
      name))
				       
; The env-of-definition for macros defined at top-level is a package,
; and the package system will take care of looking up the generated
; names.

(define (bind-aliases token transform env-of-use)
  (let ((env-of-definition (transform-env transform)))
    (if (compiler-env? env-of-definition)
	(make-compiler-env
	 (lambda (name)
	   (if (and (generated? name)
		    (eq? (generated-token name)
			 token))
	       (lookup env-of-definition (generated-name name))
	       (lookup env-of-use name)))
	 (lambda (name type . rest)
	   (assertion-violation 'bind-aliases "no definitions allowed" name))
	 (comp-env-macro-eval env-of-use)
	 #f)
	env-of-use)))

; Generate names for bindings reached in ENV reached via PARENT-NAME.
; The names are cached to preserve identity if they are bound.  TOKEN
; is used to identify names made by this generator.

(define (make-name-generator env token parent-name)
  (let ((alist '()))			;list of (symbol . generated)
    (lambda (name)
      (if (name? name)
	  (let ((probe (assq name alist)))
	    (if probe
		(cdr probe)
		(let ((new-name (make-generated name token env parent-name)))
		  (set! alist (cons (cons name new-name)
				    alist))
		  new-name)))
	  (assertion-violation 'make-name-generator
			       "non-name argument to rename procedure"
			       name parent-name)))))

