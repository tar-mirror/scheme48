; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Robert Ransom

; Macro expansion.

;----------------
; Scanning for definitions.
;
; Returns a list of forms expanded to the point needed to distinguish
; definitions from other forms.  Definitions and syntax definitions are
; added to ENV.

(define (scan-forms forms env)
  (let loop ((forms forms) (expanded '()))
    (if (null? forms)
	(reverse expanded)
	(let ((form (expand-head (car forms) env))
	      (more-forms (cdr forms)))
	  (cond ((define? form)
		 (loop more-forms
		       (cons (scan-define form env) expanded)))
		((define-syntax? form)
		 (loop more-forms
		       (append (scan-define-syntax form env)
			       expanded)))
		((begin? form)
		 (loop (append (cdr form) more-forms)
		       expanded))
		(else
		 (loop more-forms (cons form expanded))))))))

(define (expand-scanned-form form env)
  (if (define? form)
      (expand-define form env)
      (expand form env)))

(define (scan-define form env)
  (let ((new-form (destructure-define form)))
    (if new-form
	 (begin
	   (comp-env-define! env (cadr new-form) usual-variable-type)
	   new-form)
	 (syntax-violation 'syntax-rules "ill-formed definition" form))))

(define (expand-define form env)
  (make-node operator/define
	     (list (car form)
		   (expand (cadr form) env)
		   (expand (caddr form) env))))

(define (scan-define-syntax form env)
  (if (and (or (this-long? form 3)
	       (this-long? form 4))  ; may have name list for reifier
	   (name? (cadr form)))
      (let ((name (cadr form))
	    (source (caddr form))
	    (package (extract-package-from-comp-env env)))
	(comp-env-define! env
			  name
			  syntax-type
			  (process-syntax (if (null? (cdddr form))
					      source
					      `(cons ,source ',(cadddr form)))
					  env
					  name
					  package))
	'())
      (syntax-violation 'define-syntax "ill-formed syntax definition" form)))

; This is used by the ,expand command.

(define (expand-form form env)
  (let loop ((forms (list form)) (expanded '()))
    (if (null? forms)
	(if (= (length expanded) 1)
	    (car expanded)
	    (make-node operator/begin (cons 'begin (reverse expanded))))
	(let ((form (expand-head (car forms) env))
	      (more-forms (cdr forms)))
	  (cond ((define? form)
		 (let* ((new-form (destructure-define form))
			(temp (if new-form
				  (expand-define new-form env)
				  (syntax-violation 'expand "ill-formed definition"
						    form))))
		   (loop more-forms (cons temp expanded))))
		((define-syntax? form)
		 (loop more-forms
		       (cons (make-node operator/define-syntax
					(list (car form)
					      (expand (cadr form) env)
					      (make-node operator/quote
							 `',(caddr form))))
			     expanded)))
		((begin? form)
		 (loop (append (cdr form) more-forms)
		       expanded))
		(else
		 (loop more-forms
		       (cons (expand form env) expanded))))))))

;----------------
; Looking for definitions.
; This expands the form until it reaches a name, a form whose car is an
; operator, a form whose car is unknown, or a literal.

(define (expand-head form env)
  (cond ((node? form)
	 (if (and (name-node? form)
		  (not (node-ref form 'binding)))
	     (expand-name (node-form form) env)
	     form))
	((name? form)
	 (expand-name form env))
        ((pair? form)
	 (let ((op (expand-head (car form) env)))
	   (if (and (node? op)
		    (name-node? op))
	       (let ((probe (node-ref op 'binding)))
		 (if (binding? probe)
		     (let ((s (binding-static probe)))
		       (cond ((and (transform? s)
				   (eq? (binding-type probe) syntax-type))
			      (expand-macro-application
			        s (cons op (cdr form)) env expand-head))
			     ((and (operator? s)
				   (eq? s operator/structure-ref))
			      (expand-structure-ref form env expand-head))
			     (else
			      (cons op (cdr form)))))
		     (cons op (cdr form))))
	       (cons op (cdr form)))))
	(else
	 form)))

; Returns a DEFINE of the form (define <id> <value>).  This handles the following
; kinds of defines:
;  (define <id> <value>)
;  (define <id>)		        ; value is unassigned
;  (define (<id> . <formals>) <value>)  ; value is a lambda
; The return value is #f if any syntax error is found.

(define (destructure-define form)
  (if (at-least-this-long? form 2)
      (let ((pat (cadr form))
	    (operator (car form)))
	(cond ((pair? pat)
	       (if (and (name? (car pat))
			(names? (cdr pat))
			(not (null? (cddr form))))
		   `(,operator ,(car pat)
			       (,operator/lambda ,(cdr pat)
						 . ,(cddr form)))
		   #f))
	      ((null? (cddr form))
	       `(,operator ,pat (,operator/unassigned)))
	      ((null? (cdddr form))
	       `(,operator ,pat ,(caddr form)))
	      (else
	       #f)))
      #f))

(define (make-operator-predicate operator-id)
  (let ((operator (get-operator operator-id syntax-type)))
    (lambda (form)
      (and (pair? form)
	   (eq? operator
		(static-value (car form)))))))

(define define?        (make-operator-predicate 'define))
(define begin?         (make-operator-predicate 'begin))
(define define-syntax? (make-operator-predicate 'define-syntax))

(define (static-value form)
  (if (and (node? form)
	   (name-node? form))
      (let ((probe (node-ref form 'binding)))
	(if (binding? probe)
	    (binding-static probe)
	    #f))
      #f))

; --------------------
; The horror of internal defines

; This returns a single node, either a LETREC, if there are internal definitions,
; or a BEGIN if there aren't any.  If there are no expressions we turn the last
; definition back into an expression, thus causing the correct warning to be
; printed by the compiler.

(define (expand-body body env)
  (if (null? (cdr body))  ;++
      (expand (car body) env)
      (call-with-values
       (lambda ()
	 (scan-body-forms body env '()))
       (lambda (defs exps env)
	 (if (null? defs)
	     (make-node operator/begin (cons 'begin (expand-list exps env)))
	     (call-with-values
	      (lambda ()
		(if (null? exps)
		    (values (reverse (cdr defs))
			    `((,operator/define ,(caar defs) ,(cdar defs))))
		    (values (reverse defs)
			    exps)))
	      (lambda (defs exps)
		(expand-letrec operator/letrec
			       (map car defs)
			       (map cdr defs)
			       exps
			       env))))))))

; Walk through FORMS looking for definitions.  ENV is the current environment,
; DEFS a list of definitions found so far.
;
; Returns three values: a list of (define <name> <value>) lists, a list of
; remaining forms, and the environment to use for expanding all of the above.

(define (scan-body-forms forms env defs)
  (if (null? forms)
      (values defs '() env)
      (let ((form (expand-head (car forms) env))
	    (more-forms (cdr forms)))
	(cond ((define? form)
	       (let ((new-form (destructure-define form)))
		 (if new-form
		     (let* ((name (cadr new-form))
			    (node (make-node operator/name name)))
		       (scan-body-forms more-forms
					(bind1 name node env)
					(cons (cons node
						    (caddr new-form))
					      defs)))
		     (syntax-violation 'scan-body-forms
				       "ill-formed definition" form))))
	      ((begin? form)
	       (call-with-values
		(lambda ()
		  (scan-body-forms (cdr form)
				   env
				   defs))
		(lambda (new-defs exps env)
		  (cond ((null? exps)
			 (scan-body-forms more-forms env new-defs))
			((eq? new-defs defs)
			 (values defs (append exps more-forms) env))
			(else
			 (body-lossage forms env))))))
	      (else
	       (values defs (cons form more-forms) env))))))

(define (body-lossage node env)
  (syntax-violation 'body
		    "definitions and expressions intermixed"
		    (schemify node env)))

;--------------------
; Expands all macros in FORM and returns a node.

(define (expand form env)
  (cond ((node? form)
	 (if (and (name-node? form)
		  (not (node-ref form 'binding)))
	     (expand-name (node-form form) env)
	     form))
	((name? form)
	 (expand-name form env))
        ((pair? form)
	 (if (operator? (car form))
	     (expand-operator-form (car form) (car form) form env)
	     (let ((op-node (expand (car form) env)))
	       (if (name-node? op-node)
		   (let ((probe (node-ref op-node 'binding)))
		     (if (binding? probe)
			 (let ((s (binding-static probe)))
			   (cond ((operator? s)
				  (expand-operator-form s op-node form env))
				 ((and (transform? s)
				       (eq? (binding-type probe) syntax-type))
				  ;; Non-syntax transforms get done later
				  (expand-macro-application
				   s (cons op-node (cdr form)) env expand))
				 (else
				  (expand-call op-node form env))))
			 (expand-call op-node form env)))
		   (expand-call op-node form env)))))
	((literal? form)
	 (expand-literal form))
	;; ((qualified? form) ...)
	(else
	 (syntax-violation 'expand "invalid expression" form))))

(define (expand-list exps env)
  (map (lambda (exp)
	 (expand exp env))
       exps))

(define (expand-literal exp)
  (make-node operator/literal (make-immutable! exp)))

(define (expand-call proc-node exp env)
  (if (list? exp)
      (make-node operator/call
		 (cons proc-node (expand-list (cdr exp) env)))
      (syntax-violation 'expand-call "invalid expression" exp)))

; An environment is a procedure that takes a name and returns one of
; the following:
;
;  1. A binding record.
;  2. A pair (<binding-record> . <path>)
;  3. A node, which is taken to be a substitution for the name.
;     Or, for lexically bound variables, this is just a name node.
;  4. #f, for unbound variables
;
; In case 1, EXPAND caches the binding as the node's BINDING property.
; In case 2, it simply returns the node.

(define (expand-name name env)
  (let ((binding (lookup env name)))
    (if (node? binding)
	binding
	(let ((node (make-node operator/name name)))
	  (node-set! node 'binding (or binding 'unbound))
	  node))))

; Expand a macro.  EXPAND may either be expand or expand-head.

(define (expand-macro-application transform form env-of-use expand)
  (call-with-values
   (lambda ()
     (maybe-apply-macro-transform transform
				  form
				  (node-form (car form))
				  env-of-use))
   (lambda (new-form new-env)
     (if (eq? new-form form)
	 (syntax-violation (schemify (car form) env-of-use)
			   "use of macro doesn't match definition"
			   (cons (schemify (car form) env-of-use)
				 (desyntaxify (cdr form))))
	 (expand new-form new-env)))))

;--------------------
; Specialist classifiers for particular operators

(define (expand-operator-form op op-node form env)
  ((operator-table-ref expanders (operator-uid op))
   op op-node form env))

(define expanders
  (make-operator-table (lambda (op op-node form env)
			 (if (let ((nargs (operator-nargs op)))
			       (or (not nargs)
				   (and (list? (cdr form))
					(= nargs (length (cdr form))))))
			     (make-node op
					(cons op-node
					      (expand-list (cdr form) env)))
			     (expand-call op-node form env)))))

(define (define-expander name proc)
  (operator-define! expanders name syntax-type proc))

; Definitions are not expressions.

(define-expander 'define
  (lambda (op op-node exp env)
    (syntax-violation 'define
		      (if (destructure-define exp)
			  "definition in expression context"
			  "ill-formed definition")
		      exp)))

; Remove generated names from quotations.

(define-expander 'quote
  (lambda (op op-node exp env)
    (if (this-long? exp 2)
	(make-node op (list op (desyntaxify (cadr exp))))
	(syntax-violation 'quote "invalid expression" exp))))

; Don't evaluate, but don't remove generated names either.  This is
; used when writing macro-defining macros.  Once we have avoided the
; use of DESYNTAXIFY it is safe to replace this with regular QUOTE.

(define-expander 'code-quote
  (lambda (op op-node exp env)
    (if (this-long? exp 2)
	(make-node operator/quote (list op (cadr exp)))
	(syntax-violation 'code-quote "invalid expression" exp))))

; Convert one-armed IF to two-armed IF.

(define-expander 'if
  (lambda (op op-node exp env)
    (cond ((this-long? exp 3)
	   (make-node op
		      (cons op
			    (expand-list (append (cdr exp)
						 (list (unspecific-node)))
					 env))))
	  ((this-long? exp 4)
	   (make-node op
		      (cons op (expand-list (cdr exp) env))))
	  (else
	   (syntax-violation 'if "invalid expression" exp)))))

(define (unspecific-node)
  (make-node operator/unspecific '(unspecific)))

; For the module system:

(define-expander 'structure-ref
  (lambda (op op-node form env)
    (expand-structure-ref form env expand)))

; This is also called by EXPAND-HEAD, which passes in a different expander.

(define (expand-structure-ref form env expander)
  (let ((struct-node (expand (cadr form) env))
	(lose (lambda ()
		(syntax-violation 'structure-ref "invalid structure reference" form))))
    (if (and (this-long? form 3)
	     (name? (caddr form))
	     (name-node? struct-node))
	(let ((b (node-ref struct-node 'binding)))
	  (if (and (binding? b)
		   (binding-static b)) ; (structure? ...)
	      (expander (generate-name (desyntaxify (caddr form))
				       (binding-static b)
				       (node-form struct-node))
			env)
	      (lose)))
	(lose))))

; Scheme 48 internal special form principally for use by the
; DEFINE-STRUCTURES macro.

(define-expander '%file-name%
  (lambda (op op-node form env)
    (make-node operator/quote `',(source-file-name env))))

; Checking the syntax of others special forms

(define-expander 'lambda
  (lambda (op op-node exp env)
    (if (and (at-least-this-long? exp 3)
	     (names? (cadr exp)))
	(expand-lambda (cadr exp) (cddr exp) env)
	(syntax-violation 'lambda "invalid expression" exp))))

(define (expand-lambda names body env)
  (call-with-values
    (lambda ()
      (bind-names names env))
    (lambda (names env)
      (make-node operator/lambda
		 (list 'lambda names (expand-body body env))))))

(define (bind-names names env)
  (let loop ((names names) (nodes '()) (out-names '()))
    (cond ((null? names)
	   (values (reverse nodes)
		   (bind out-names nodes env)))
	  ((name? names)
	   (let ((last (make-node operator/name names)))
	     (values (append (reverse nodes) last)
		     (bind (cons names out-names) (cons last nodes) env))))
	  (else
	   (let ((node (make-node operator/name (car names))))
	     (loop (cdr names) (cons node nodes) (cons (car names) out-names)))))))

(define (names? l)
  (or (null? l)
      (name? l)
      (and (pair? l)
	   (name? (car l))
	   (names? (cdr l)))))

(define-expander 'set!
  (lambda (op op-node exp env)
    (if (and (this-long? exp 3)
	     (name? (cadr exp)))
	(make-node op (cons op (expand-list (cdr exp) env)))
	(syntax-violation 'set! "invalid expression" exp))))

(define (letrec-expander op/letrec)
  (lambda (op op-node exp env)
    (if (and (at-least-this-long? exp 3)
	     (let-specs? (cadr exp)))
	(let ((specs (cadr exp))
	      (body (cddr exp)))
	  (let* ((names (map (lambda (spec)
			       (make-node operator/name (car spec)))
			     specs))
		 (env (bind (map car specs) names env)))
	    (expand-letrec op/letrec names (map cadr specs) body env)))
	(syntax-violation 'letrec "invalid expression" exp))))

(define-expander 'letrec
  (letrec-expander operator/letrec))

(define-expander 'letrec*
  (letrec-expander operator/letrec*))

(define (expand-letrec op/letrec names values body env)
  (let* ((new-specs (map (lambda (name value)
			   (list name
				 (expand value env)))
			 names
			 values)))
    (make-node op/letrec
	       (list 'letrec new-specs (expand-body body env)))))

(define-expander 'loophole
  (lambda (op op-node exp env)
    (if (this-long? exp 3)
	(make-node op (list op
			    (sexp->type (desyntaxify (cadr exp)) #t)
			    (expand (caddr exp) env)))
	(syntax-violation 'loophole "invalid expression" exp))))

(define-expander 'let-syntax
  (lambda (op op-node exp env)
    (if (and (at-least-this-long? exp 3)
	     (let-specs? (cadr exp)))
	(let ((specs (cadr exp)))
	  (expand-body (cddr exp)
		       (bind (map car specs)
			     (map (lambda (spec)
				    (make-binding syntax-type
						  (list 'let-syntax)
						  (process-syntax (cadr spec)
								  env
								  (car spec)
								  env)))
				  specs)
			     env)))
	(syntax-violation 'let-syntax "invalid expression" exp))))

(define-expander 'letrec-syntax
  (lambda (op op-node exp env)
    (if (and (at-least-this-long? exp 3)
	     (let-specs? (cadr exp)))
	(let* ((specs (cadr exp))
	       (bindings (map (lambda (spec)
				(make-binding syntax-type
					      (list 'letrec-syntax)
					      'unassigned))
			      specs))
	       (new-env (bind (map car specs) bindings env)))
	  (for-each (lambda (spec binding)
		      (set-binding-static! binding
					   (process-syntax (cadr spec)
							   new-env
							   (car spec)
							   new-env)))
		    specs bindings)
	  (expand-body (cddr exp) new-env))
	(syntax-violation 'letrec-syntax "invalid expression" exp))))
    
(define (process-syntax form env name env-or-package)
  (let ((eval+env (force (comp-env-macro-eval env))))
    (make-transform/macro ((car eval+env) form (cdr eval+env))
			  env-or-package
			  syntax-type
			  form
			  name)))

; This just looks up the names that the LAP code will want and replaces them
; with the appropriate node.
;
; (lap <id> (<free name> ...) <instruction> ...)

(define-expander 'lap
  (lambda (op op-node exp env)
    (if (and (at-least-this-long? exp 4)
	     (name? (cdr exp))
	     (every name? (caddr exp)))
	(make-node op `(,op
			,(desyntaxify (cadr exp))
			,(map (lambda (name)
				(expand-name (cadr exp) env))
			      (caddr exp))
			. ,(cdddr exp)))
	(syntax-violation 'lap "invalid expression" exp))))

; --------------------
; Syntax checking utilities

(define (this-long? l n)
  (cond ((null? l)
	 (= n 0))
	((pair? l)
	 (this-long? (cdr l) (- n 1)))
	(else
	 #f)))

(define (at-least-this-long? l n)
  (cond ((null? l)
	 (<= n 0))
	((pair? l)
	 (at-least-this-long? (cdr l) (- n 1)))
	(else
	 #f)))

(define (let-specs? x)
  (or (null? x)
      (and (pair? x)
	   (let ((s (car x)))
	     (and (pair? s)
		  (name? (car s))
		  (pair? (cdr s))
		  (null? (cddr s))))
	   (let-specs? (cdr x)))))

; --------------------
; Utilities

(define (literal? exp)
  (or (number? exp) (char? exp) (string? exp) (boolean? exp)
      (code-vector? exp)))

(define (syntax? d)
  (cond ((operator? d)
	 (eq? (operator-type d) syntax-type))
	((transform? d)
	 (eq? (transform-type d) syntax-type))
	(else #f)))
