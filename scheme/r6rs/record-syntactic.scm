; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber

(define-syntax define-record-type
  (let ((uid-count 0))
    (lambda (e r c)
      ;; returns clause or #f
      (define (search-clause keyword clauses)
	(let loop ((clauses clauses))
	  (cond
	   ((null? clauses)
	    #f)
	   ((c keyword (caar clauses))
	    (let ((clause (car clauses)))
	      ;; rudimentary checks
	      (cond
	       ((not (list? clause))
		(syntax-violation 'define-record-type "invalid clause" e clause))
	       ((search-clause keyword (cdr clauses))
		=> (lambda (duplicate)
		     (syntax-violation 'define-record-type "duplicate clause" e clause)))
	       (else
		clause))))
	   (else (loop (cdr clauses))))))

      (define (clause-value clause value-ok?)
	(cond
	 ((not clause) #f)
	 ((not (= (length clause) 2))
	  (syntax-violation 'define-record-type "invalid clause" clause))
	 ((not (value-ok? (cadr clause)))
	  (syntax-violation 'define-record-type "invalid clause value" clause))
	 (else
	  (cadr clause))))
    
      (define s->s symbol->string)
      (define s-conc (lambda args (string->symbol (apply string-append args))))

      (define %mutable (r 'mutable))
      (define %immutable (r 'immutable))

      ;; returns a field desc; each desc is a three-element list:
      ;; field-name accessor-name (maybe mutator-name)
      (define (parse-field-spec record-name-string field-spec)
	(cond
	 ((symbol? field-spec)
	  (list field-spec
		(s-conc record-name-string "-" (s->s field-spec))
		#f))
	 ((and (pair? field-spec)
	       (pair? (cdr field-spec)))
	  (let ((tag (car field-spec))
		(field (cadr field-spec))
		(size (length field-spec)))
	    (cond
	     ((c tag %immutable) 
	      (case size
		((2)
		 (list field
		       (s-conc record-name-string "-" (s->s field))
		       #f))
		((3)
		 (list field
		       (caddr field-spec)
		       #f))
		(else
		 (syntax-violation 'define-record-type "invalid field spec" e field-spec))))
	     ((c tag %mutable)
	      (case size
		((2)
		 (list field
		       (s-conc record-name-string "-" (s->s field))
		       (s-conc record-name-string "-" (s->s field) "-set!")))
		((4)
		 (list field
		       (caddr field-spec)
		       (cadddr field-spec)))
		(else
		 (syntax-violation 'define-record-type "invalid field spec" e field-spec))))
	     (else
	      (syntax-violation 'define-record-type "invalid field spec" e field-spec)))))
	 (else
	  (syntax-violation 'define-record-type "invalid field spec" e field-spec))))

      (let ((name-spec (cadr e))
	    (clauses (cddr e)))
      
	(call-with-values
	    (lambda ()
	      (cond ((symbol? name-spec) ; probably barfs on generated name
		     (values name-spec
			     (s-conc "make-" (s->s name-spec))
			     (s-conc (s->s name-spec) "?")))
		    ((or (not (list? name-spec))
			 (not (= 3 (length name-spec))))
		     (syntax-violation 'define-record-type "invalid name spec" e name-spec))
		    (else
		     (apply values name-spec))))
	  (lambda (record-name constructor-name predicate-name)
	    (let ((record-name-string (s->s record-name))
		  (field-specs
		   (cond
		    ((search-clause (r 'fields) clauses)
		     => cdr)
		    (else '())))
		  (parent (clause-value (search-clause (r 'parent) clauses) symbol?)) ; probably barfs on generated names
		  (protocol (clause-value (search-clause (r 'protocol) clauses) values))
		  (sealed? (clause-value (search-clause (r 'sealed) clauses) boolean?))
		  (opaque? (clause-value (search-clause (r 'opaque) clauses) boolean?))
		  (nongenerative-clause (search-clause (r 'nongenerative) clauses))
		  (parent-rtd-clause (search-clause (r 'parent-rtd) clauses)))

	      (if (and parent parent-rtd-clause)
		  (syntax-violation 'define-record-type "can't have both `parent' and `parent-rtd' clauses"
				    e))
	      (if (and parent-rtd-clause
		       (or (not (list? parent-rtd-clause))
			   (not (= 3 (length parent-rtd-clause)))))
		  (syntax-violation 'define-record-type "invalid `parent-rtd' clause" e parent-rtd-clause))
	    
	      (let ((field-descs
		     ;; cons field index onto descs
		     (let loop ((i 0) (field-specs field-specs) (res '()))
		       (if (null? field-specs)
			   (reverse res)
			   (loop (+ 1 i)
				 (cdr field-specs)
				 (cons (cons i (parse-field-spec record-name-string (car field-specs)))
				       res)))))
		    (nongenerative-uid
		     (and nongenerative-clause
			  (let ((size (length nongenerative-clause)))
			    (cond
			     ((= size 1) 
			      (set! uid-count (+ 1 uid-count)) ; #### not enough in the presence of separate compilation
			      (s-conc "record-type-" (number->string uid-count)))
			     ((= size 2)
			      (cadr nongenerative-clause))
			     (else
			      (syntax-violation 'define-record-type "invalid `nongenerative' clause" e nongenerative-clause))))))
		    (parent-rtd
		     (cond
		      (parent
		       `(,(r 'record-type-descriptor) ,parent))
		      (parent-rtd-clause => cadr)
		      (else #f)))
		    (parent-cd
		     (cond
		      (parent
		       `(,(r 'record-constructor-descriptor) ,parent))
		      (parent-rtd-clause => caddr)
		      (else #f)))
		    (rtd-name (r 'record-rtd))
		    (cd-name (r 'cd))
		    (%define (r 'define)))
		
		`(,(r 'begin)
		  (,%define ,rtd-name
			    (,(r 'make-record-type-descriptor)
			     ',record-name
			     ,parent-rtd
			     ',nongenerative-uid
			     ,sealed?
			     ,opaque?
			     ',(list->vector
				(map
				 (lambda (desc)
				   (apply (lambda (index name accessor mutator)
					    (list (if mutator
						      'mutable
						      'immutable)
						  name))
					  desc))
				 field-descs))))
		  (,%define ,cd-name
			    (,(r 'make-record-constructor-descriptor)
			     ,rtd-name
			     ,parent-cd
			     ,protocol))
		  ,(cond
		    (parent
		     `(,parent
		       ,(r 'dispatch)
		       (,record-name
			,constructor-name ,predicate-name 
			,rtd-name ,cd-name
			,parent ,protocol ,sealed? ,opaque? ,nongenerative-uid ,parent-rtd ,parent-cd
			,field-descs)
		       ,(r 'define-known-record-type-helper)
		       ,(r 'define-unknown-record-type-helper)))
		    (parent-rtd-clause
		     `(,(r 'define-unknown-record-type-helper)
		       ,record-name
		       ,constructor-name ,predicate-name 
		       ,rtd-name ,cd-name
		       ,parent ,protocol ,sealed? ,opaque? ,nongenerative-uid ,parent-rtd ,parent-cd
		       ,field-descs))
		    (else
		     `(,(r 'define-known-record-type-helper)
		       0 #t
		       ,record-name
		       ,constructor-name ,predicate-name 
		       ,rtd-name ,cd-name
		       ,parent ,protocol ,sealed? ,opaque? ,nongenerative-uid ,parent-rtd ,parent-cd
		       ,field-descs))))))))))))
    
; fallback: the supertype isn't completely known statically
(define-syntax define-unknown-record-type-helper
  (lambda (e r c)

    (define %begin (r 'begin))
    (define %define (r 'define))
    (define %record-accessor (r 'record-accessor))
    (define %record-mutator (r 'record-mutator))

    (define %loophole (r 'loophole))
    (define %:value (r ':value))
    (define %:unspecific (r ':value))
    (define %proc (r 'proc))

    (apply
     (lambda (_ record-name constructor-name predicate-name rtd-name cd-name
		parent protocol sealed? opaque? nongenerative-uid parent-rtd parent-cd
		field-descs)
       `(,%begin
	 (,(r 'define-unknown-record-type-name) ,record-name ,rtd-name ,cd-name)
	 (,%define ,constructor-name 
		   (,(r 'record-constructor) ,cd-name))
	 (,%define ,predicate-name (,(r 'record-predicate) ,rtd-name))
	 ,@(map (lambda (desc)
		  (apply (lambda (index name accessor mutator)
			   (let ((acc
				  `(,%define ,accessor
					     (,%loophole (,%proc (,rtd-name) ,%:value)
							 (,%record-accessor ,rtd-name ,index)))))
			     (if mutator
				 `(,%begin
				   ,acc
				   (,%define ,mutator
					     (,%loophole (,%proc (,rtd-name ,%:value) ,%:unspecific)
							 (,%record-mutator ,rtd-name ,index))))
				 acc)))
			 desc))
		field-descs)))
     e)))

; This knows about the implementation of records and creates
; constructor, accessors, mutators, etc. directly instead of calling
; the procedures from r6rs-records-procedural.  This is done to allow
; the optional auto-inlining optimizer to inline the accessors,
; mutators, etc.

(define-syntax define-known-record-type-helper
  (lambda (e r c)

    (define %begin (r 'begin))
    (define %define (r 'define))
    (define %record (r 'record))
    (define %checked-record-ref (r 'checked-record-ref))
    (define %checked-record-set! (r 'checked-record-set!))
    (define %r (r 'r))
    (define %val (r 'val))
    (define %loophole (r 'loophole))
    (define %:value (r ':value))
    (define %:unspecific (r ':value))
    (define %proc (r 'proc))

    (define (build-args count)
      (let loop ((i count)
		 (args '()))
	(if (zero? i)
	    args
	    (loop (- i 1)
		  (cons (r (string->symbol (string-append "arg-" (number->string i))))
			args)))))

    (apply
     (lambda (_ parent-field-count parent-default-constructor?
		record-name constructor-name predicate-name rtd-name cd-name
		parent protocol sealed? opaque? nongenerative-uid parent-rtd parent-cd
		field-descs)
       (let ((field-count (+ parent-field-count (length field-descs)))
	     (default-constructor? (and parent-default-constructor? (not protocol))))
	 `(,%begin
	   (,(r 'define-known-record-type-name) ,record-name ,rtd-name ,cd-name
	    ,field-count ,default-constructor?)
	   ,(if default-constructor?
		(let ((args (build-args field-count)))
		  `(,%define (,constructor-name . ,args)
			     (,%loophole (,%proc ,(map (lambda (_) %:value) args) ,rtd-name)
					 (,%record ,rtd-name . ,args))))
		`(,%define ,constructor-name (,(r 'record-constructor) ,cd-name)))
	   (,%define ,predicate-name (,(r 'record-predicate) ,rtd-name))
	   ,@(map (lambda (desc)
		    (apply (lambda (index name accessor mutator)
			     (let* ((real-index (+ 1 parent-field-count index))
				    (acc
				     `(,%define (,accessor ,%r)
						(,%loophole (,%proc (,rtd-name) ,%:value)
							    (,%checked-record-ref ,%r ,rtd-name
										  ,real-index)))))
			       (if mutator
				   `(,%begin
				     ,acc
				     (,%define (,mutator ,%r ,%val)
					       (,%loophole (,%proc (,rtd-name ,%:value) ,%:unspecific)
							   (,%checked-record-set! ,%r ,rtd-name ,real-index ,%val))))
				   acc)))
			   desc))
		  field-descs))))
     e)))

(define-syntax define-known-record-type-name
  (syntax-rules ()
    ((define-known-record-type-name ?name ?rtd ?constructor-descriptor ?field-count ?default-constructor?)
     (define-syntax ?name
       (syntax-rules (descriptor constructor-descriptor dispatch)
	 ((?name descriptor) ?rtd)
	 ((?name constructor-descriptor) ?constructor-descriptor)
	 ((?name dispatch ?args ?known ?unknown) (?known ?field-count ?default-constructor? . ?args)))))))

(define-syntax define-unknown-record-type-name
  (syntax-rules ()
    ((define-unknown-record-type-name ?name ?rtd ?constructor-descriptor)
     (define-syntax ?name
       (syntax-rules (descriptor constructor-descriptor dispatch)
	 ((?name descriptor) ?rtd)
	 ((?name constructor-descriptor) ?constructor-descriptor)
	 ((?name dispatch ?args ?known ?unknown) (?unknown . ?args)))))))

; Retrofitting RTS record types to R6RS record types.
; For now, we do default constructors only.
; (define-retrofitted-record-type r6rs-type rts-type (mutable field1) (immutable field2) ...)
; (define-retrofitted-record-type (r6rs-type r6rs-base-type) rts-type (mutable field1) ...)

(define-syntax define-retrofitted-record-type
  (lambda (e r c)
    (cons (r 'define-retrofitted-record-type-helper)
	  (cons (length (list-tail e 6))
		(cdr e)))))

(define-syntax define-retrofitted-record-type-helper
  (syntax-rules ()
    ((define-retrofitted-record-type-helper ?field-count 
       (?r6rs-type ?r6rs-parent-type) ?rts-type ?uid ?sealed? ?opaque? ?field-spec1 ...)
     (begin
       (retrofit-record-type! ?rts-type ?uid ?sealed? ?opaque? '#(?field-spec1 ...))
       (define cd (make-record-constructor-descriptor ?rts-type #f #f))
       (?r6rs-parent-type dispatch (?r6rs-type ?rts-type cd ?field-count)
			  define-known-retrofitted-record-type-helper
			  define-unknown-retrofitted-record-type-helper)))
    ((define-retrofitted-record-type-helper ?field-count
       ?r6rs-type ?rts-type ?uid ?sealed? ?opaque? ?field-spec1 ...)
     (begin
       (retrofit-record-type! ?rts-type ?uid ?sealed? ?opaque? '#(?field-spec1 ...))
       (define cd (make-record-constructor-descriptor ?rts-type #f #f))
       (define-known-record-type-name ?r6rs-type ?rts-type cd ?field-count #t)))))

(define-syntax define-known-retrofitted-record-type-helper
  (lambda (e r c)
    (apply 
     (lambda (_ parent-field-count parent-default-constructor? r6rs-type rts-type cd field-count)
       `(,(r 'define-known-record-type-name)
	 ,r6rs-type ,rts-type ,cd ,(+ parent-field-count field-count) ,parent-default-constructor?))
     e)))

(define-syntax define-unknown-retrofitted-record-type-helper
  (lambda (e r c)
    (apply 
     (lambda (_ r6rs-type rts-type cd field-count)
       `(,(r 'define-unknown-record-type-name) ,r6rs-type ,rts-type ,cd))
     e)))

(define-syntax record-type-descriptor
  (syntax-rules ()
    ((record-type-descriptor ?type)
     (?type descriptor))))

(define-syntax record-constructor-descriptor
  (syntax-rules ()
    ((record-constructor-descriptor ?type)
     (?type constructor-descriptor))))
