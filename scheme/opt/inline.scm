; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Robert Ransom

; Once we know that we want something to be inlined, the following things
; actually make use of the fact.  For procedures for which all
; arguments can be substituted unconditionally, we make a transform
; (a macro, really) that performs the substitution.

(define (make-inline-transform node type package name)
  (let* ((free (find-node-usages node))
	 (env (package->environment package))
	 (qualified-free (map (lambda (name)
				(cons name
				      (name->qualified name env)))
			      free)))
    (let ((form (clean-node node '()))
	  (aux-names (map (lambda (pair)
			    (do ((name (cdr pair) (qualified-parent-name name)))
				((not (qualified? name))
				 name)))
			  qualified-free)))
      (make-transform/inline (inline-transform form aux-names)
			     package		;env ?
			     type
			     `(inline-transform ',(remove-bindings form
								   qualified-free)
						',aux-names)
			     name))))

; This routine is obligated to return an S-expression.
; It's better not to rely on the constancy of node id's, so 
; the output language is a sort of quasi-Scheme.  Any form that's a list
; has an operator name in its car.
;
; ENV is an a-list mapping names to qualified (for package variables) or
; non-clashing (for lexical variables) new names.
;
; What about SET! ?

(define (clean-node node env)
  (cond ((name-node? node)
	 (clean-lookup env node))
	((quote-node? node)
	 `(quote ,(cadr (node-form node))))
	((lambda-node? node)
	 (clean-lambda node env))
	((call-node? node)
	 (cons 'call
	       (map (lambda (node) (clean-node node env))
		    (node-form node))))
	((loophole-node? node)
	 (let ((args (cdr (node-form node))))
	   `(loophole ,(type->sexp (car args) #t)
		      ,(clean-node (cadr args) env))))
	;; LETREC had better not occur, since we are not prepared for it
	((pair? (node-form node))
	 (cons (operator-name (node-operator node))
	       (map (lambda (subnode)
		      (clean-node subnode env))
		    (cdr (node-form node)))))
	(else (node-form node))))	;literal

(define (clean-lambda node env)
  (let* ((exp (node-form node))
	 (formals (cadr exp))
	 (env (fold (lambda (name-node env)
		      `((,name-node . , (unused-name env (node-form name-node)))
			. ,env))
		    (normalize-formals formals)
		    env)))
    `(lambda ,(let recur ((foo formals))
		(cond ((node? foo) (clean-lookup env foo))
		      ((pair? foo)
		       (cons (recur (car foo))
			     (recur (cdr foo))))
		      (else foo)))	; when does this happen?
       ,(clean-node (caddr exp) env))))

; Package names get looked up by name, lexical names get looked up by the
; node itself.

(define (clean-lookup env node)
  (let ((binding (node-ref node 'binding))) 
    (if (binding? binding)
	`(package-name ,(node-form node) ,binding)
	(cdr (assq node env)))))
  
; I'm aware that this is pedantic.

(define (unused-name env name)
  (let ((sym (name->symbol name)))
    (do ((i 0 (+ i 1))
	 (name sym
	       (string->symbol (string-append (symbol->string sym)
					      (number->string i)))))
	((every (lambda (pair)
		  (not (eq? name (cdr pair))))
		env)
	 name))))

; We need to remove the binding records from the form that will be used for
; reification.  A better alternative might be for packages to provide dumpable
; names as stand-ins for bound generated names.  The problem is that packages
; use EQ? tables for names and the linker does not preserve EQ-ness for
; generated names.  Instead, we remember the path and do the lookup that way.
; This doesn't work if the generated name is itself bound.
;  If the environment in the generated name were the package itself, instead
; of its environment wrapper, the linker could probably do the right thing
; with all package-level generated names.

(define (remove-bindings form free)
  (let label ((form form))
    (if (pair? form)
	(case (car form)
	  ((package-name)
	   (cdr (assq (cadr form) free))) ; just the name
	  ((quote) form)
	  ((lambda)
	   `(lambda ,(cadr form)
	      ,(label (caddr form))))
	  (else
	   (map label form)))
	form)))

;----------------
; ST stands for substitution template (cf. MAKE-SUBSTITUTION-TEMPLATE)

(define (inline-transform st aux-names)
  (cons
   (if (and (pair? st)
	    (eq? (car st) 'lambda))
       (let ((formals (cadr st))
	     (body (caddr st)))
	 (lambda (exp package rename)
	   (let ((args (cdr exp)))
	     (if (= (length formals) (length args))
		 (reconstitute body
			       package
			       (make-substitution rename formals args))
		 ;; No need to generate warning since the type checker will
		 ;; produce one.  Besides, we don't want to produce a warning
		 ;; for things like (> x y z).
		 exp))))
       (lambda (exp package rename)
	 (cons (reconstitute st package rename)
	       (cdr exp))))
   aux-names))

(define (make-substitution rename formals args)
  (let ((subs (map cons formals args)))
    (lambda (name)
      (let ((probe (assq name subs)))
	(cond (probe
	       (cdr probe))
	      ((generated? name)
	       (note 'make-substitution "this shouldn't happen" name)
	       name)			;TEMPORARY KLUDGE.
	      (else
	       (rename name)))))))

; Turn an s-expression back into a node.
; ST is an S-expression as returned by MAKE-SUBSTITUTION-TEMPLATE.

(define (reconstitute st package rename)
  (let label ((st st))
    (cond ((symbol? st)
	   (let ((foo (rename st)))
	     (if (name? foo)
		 (reconstitute-name foo package)
		 foo)))
	  ((qualified? st)
	   (reconstitute-name (qualified->name st rename)
			      package))
	  ((pair? st)
	   (case (car st)
	     ((quote)
	      (make-node (get-operator 'quote) st))
	     ((package-name)
	      (let ((node (make-node operator/name (cadr st))))
		(node-set! node 'binding (caddr st))
		node))
	     ((call)
	      (make-node (get-operator 'call)
			 (map label (cdr st))))
	     ((loophole)
	      (make-node (get-operator 'loophole)
			 (list 'loophole
			       (sexp->type (cadr st) #t)
			       (label (caddr st)))))
	     ((lambda)
	      (assertion-violation 'reconstitute-name "lambda substitution NYI" st))
	     (else
	      (let ((keyword (car st)))
		(make-node (get-operator keyword)
			   (cons keyword
				 (map label (cdr st))))))))
	  (else
	   (make-node operator/literal st)))))

(define (reconstitute-name name package)
  (let ((binding (package-lookup package name))
	(node (make-node operator/name name)))
    (if (binding? binding)
	(node-set! node 'binding binding))
    node))

; --------------------
; Convert a qualified name #(>> parent-name symbol) to an alias.

(define (qualified->name qualified rename)
  (let recur ((name qualified))
    (if (qualified? name)
	(let ((parent (recur (qualified-parent-name name))))
	  (generate-name (qualified-symbol name)
			 (get-qualified-env (generated-env parent)
					    (generated-name parent))
			 parent))
	(rename name))))

(define (get-qualified-env env parent)
  (let ((binding (generic-lookup env parent)))
    (if (binding? binding)
	(let ((static (binding-static binding)))
	  (cond ((transform? static)
		 (transform-env static))
		((structure? static)
		 static)
		(else
		 (assertion-violation 'get-qualified-env "invalid qualified reference"
				      env parent static))))
	(assertion-violation 'get-qualified-env "invalid qualified reference"
			     env parent binding))))

