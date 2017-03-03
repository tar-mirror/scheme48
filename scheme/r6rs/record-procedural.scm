; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber

(define-record-type :record-type-data
  (make-record-type-data uid sealed? opaque? field-specs immutable?)
  record-type-data?
  (uid record-type-data-uid) ; not to be confused with the generated uid
  (sealed? record-type-data-sealed?)
  (opaque? record-type-data-opaque?)
  (field-specs record-type-data-field-specs)
  (immutable? record-type-data-immutable?))

(define make-field-spec cons)

(define field-spec-mutable? car)
(define field-spec-name cdr)

(define (field-spec=? spec-1 spec-2)
  (equal? spec-1 spec-2))

(define (record-type-uid rtd)
  (record-type-data-uid (record-type-data rtd)))
(define (record-type-sealed? rtd)
  (record-type-data-sealed? (record-type-data rtd)))
(define (record-type-opaque? rtd)
  (record-type-data-opaque? (record-type-data rtd)))
(define (record-type-field-specs rtd)
  (record-type-data-field-specs (record-type-data rtd)))
(define (record-type-immutable? rtd)
  (record-type-data-immutable? (record-type-data rtd)))

(define (record-type-descriptor=? rtd-1 rtd-2)
  (and (eq? (record-type-parent rtd-1) (record-type-parent rtd-2))
       (eq? (record-type-uid rtd-1) (record-type-uid rtd-2))
       (for-all field-spec=?
		(record-type-field-specs rtd-1)
		(record-type-field-specs rtd-2))))

(define nongenerative-record-types-table
  (user-context-accessor 'nongenerative-record-types-table
			 (lambda () #f))) ; initializers don't work after the fact anyway
(define set-nongenerative-record-types-table!
  (user-context-modifier 'nongenerative-record-types-table))

(define nongenerative-record-types-table-lock (make-lock))

(define (record-type-generative? rtd)
  (not (record-type-uid rtd)))

(define (nongenerative-record-types)
  (obtain-lock nongenerative-record-types-table-lock)
  (let ((l
	 (table->entry-list (nongenerative-record-types-table))))
    (release-lock nongenerative-record-types-table-lock)
    l))

(define (delete-nongenerative-record-type thing)
  (let ((name (cond
	       ((symbol? thing)
		thing)
	       ((record-type? thing)
		(record-type-uid thing))
	       (else
		(assertion-violation 'delete-nongenerative-record-type "invalid argument" thing))))
	(table
	 (nongenerative-record-types-table)))
    (if (not (symbol? name))
	(assertion-violation 'delete-nongenerative-record-type "generative record type" name))
    (obtain-lock nongenerative-record-types-table-lock)
    (cond
     ((table-ref table name)
      (table-set! table name #f)
      (release-lock nongenerative-record-types-table-lock)
      #t)
     (else
      (release-lock nongenerative-record-types-table-lock)
      #f))))

(define (make-record-type-descriptor name parent uid sealed? opaque? fields)
  (check-parent-type 'make-record-type-descriptor name parent uid sealed? opaque? fields)
  (let ((opaque? (if parent
		     (or (record-type-opaque? parent)
			 opaque?)
		     opaque?))
	(field-specs (map parse-field-spec (vector->list fields))))
    (let ((rtd (make-record-type name (map field-spec-name field-specs) parent))
	  (data (make-record-type-data uid sealed? opaque? field-specs
				       (not (exists field-spec-mutable? field-specs)))))
      (record-record-type-data! rtd data)
      rtd)))

(define (check-parent-type caller name parent uid sealed? opaque? fields)
  (if (and parent
	   (record-type-sealed? parent))
      (assertion-violation caller "can't extend a sealed parent class"
			   name parent uid sealed? opaque? fields))
  (if (and parent
	   (not (record-type-uid parent)) ; parent generative
	   uid)			  ; ... but this one is non-generative
      (assertion-violation caller
			   "a generative type can only be extended to give a generative type"
			   name parent uid sealed? opaque? fields)))  

(define (record-record-type-data! rtd data)
  (set-record-type-data! rtd data)
  (cond
   ((record-type-data-uid data)
    => (lambda (uid)
	 (let ((table (nongenerative-record-types-table)))
	   (obtain-lock nongenerative-record-types-table-lock)
	   (cond
	    ((table-ref table uid)
	     => (lambda (old-rtd)
		  (release-lock nongenerative-record-types-table-lock)
		  (if (record-type-descriptor=? rtd old-rtd)
		      old-rtd
		      (assertion-violation "mismatched nongenerative record types with identical uids"
					   old-rtd rtd))))
	    (else
	     (table-set! table uid rtd)
	     (release-lock nongenerative-record-types-table-lock))))))))

; making non-R6RS record types into R6RS record types
(define (retrofit-record-type! rtd uid sealed? opaque? fields)
  (let ((parent (record-type-parent rtd))
	(name (record-type-name rtd)))
    (if (and parent
	     (not (record-type-data? (record-type-data parent))))
	(assertion-violation 'retrofit-record-type!
			     "parent type not an R6RS record type"
			     parent))
    (check-parent-type 'retrofit-record-type! name parent uid sealed? opaque? fields)
    (let ((opaque? (if parent
		       (or (record-type-opaque? parent)
			   opaque?)
		       opaque?))
	  (field-specs (map parse-field-spec (vector->list fields))))
      (record-record-type-data! rtd
				(make-record-type-data uid sealed? opaque? field-specs
						       (not (exists field-spec-mutable? field-specs)))))))

(define (record-type-descriptor? thing)
  (and (record-type? thing)
       (record-type-data? (record-type-data thing))))

(define (ensure-rtd who thing)
  (if (not (record-type-descriptor? thing))
      (assertion-violation who "not a record-type descriptor" thing)))

(define (parse-field-spec spec)
  (apply (lambda (mutability name)
	   (make-field-spec
	    (case mutability
	      ((mutable) #t)
	      ((immutable) #f)
	      (else
	       (assertion-violation 'parse-field-spec
				    "field spec with invalid mutability specification" spec)))
	    name))
	 spec))
 
(define (record? thing)
  (and (primitive:record? thing)
       (let ((rtd (primitive:record-type thing)))
	 (and (record-type-descriptor? rtd)
	      (not (record-type-opaque? rtd))))))

(define (record-rtd rec)
  (primitive:record-type rec))

; Constructing constructors

(define-record-type :record-constructor-descriptor
  (really-make-record-constructor-descriptor rtd protocol custom-protocol? previous)
  (rtd record-constructor-descriptor-rtd)
  (protocol record-constructor-descriptor-protocol)
  (custom-protocol? record-constructor-descriptor-custom-protocol?)
  (previous record-constructor-descriptor-previous))
  
(define (make-record-constructor-descriptor rtd previous protocol)
  (let ((parent (record-type-parent rtd)))
    (if (and previous (not parent))
	(assertion-violation 'make-record-constructor-descriptor
			     "mismatch between rtd and constructor descriptor" rtd previous))

    (if (and previous
	     (not protocol)
	     (record-constructor-descriptor-custom-protocol? previous))
	(assertion-violation 'make-record-constructor-descriptor
			     "default protocol requested when parent constructor descriptor has custom one"
			     protocol previous)) 
  
    (let ((custom-protocol? (and protocol #t))
	  (protocol (or protocol (default-protocol rtd)))
	  (previous
	   (if (or previous
		   (not parent))
	       previous
	       (make-record-constructor-descriptor parent #f #f))))
 
      (really-make-record-constructor-descriptor rtd protocol custom-protocol? previous))))

(define (default-protocol rtd)
  (let ((parent (record-type-parent rtd)))
    (if (not parent)
	(lambda (p)
	  (lambda field-values
	    (apply p field-values)))
	(let ((parent-field-count (record-type-size parent)))
	  (lambda (p)
	    (lambda all-field-values
	      (call-with-values
		  (lambda () (split-at all-field-values parent-field-count))
		(lambda (parent-field-values this-field-values)
		  (apply (apply p parent-field-values) this-field-values)))))))))

; from SRFI 1
(define (split-at lis i)
  (let loop ((i i)
	     (lis lis)
	     (rev '()))
    (if (zero? i)
	(values (reverse rev) lis)
	(loop (- i 1) (cdr lis) (cons (car lis) rev)))))

; A "seeder" is the procedure passed to the protocol, used to seed the
; initial field values.

(define (make-make-seeder real-rtd for-desc)
  (let recur ((for-desc for-desc))
    (let* ((for-rtd (record-constructor-descriptor-rtd for-desc))
	   (for-rtd-field-count (length (record-type-field-specs for-rtd))))
      (cond
       ((record-constructor-descriptor-previous for-desc)
	=> (lambda (parent-desc)
	     (let ((parent-protocol (record-constructor-descriptor-protocol parent-desc))
		   (parent-make-seeder (recur parent-desc)))
	       (lambda extension-field-values
		 (lambda parent-protocol-args
		   (lambda for-rtd-field-values
		     (if (not (= (length for-rtd-field-values) for-rtd-field-count))
			 (assertion-violation 'record-constructor
					      "wrong number of arguments to record constructor"
					      for-rtd for-rtd-field-values))
		     (apply (parent-protocol
			     (apply parent-make-seeder
				    (append for-rtd-field-values extension-field-values)))
			    parent-protocol-args)))))))
       (else
	(let-syntax ((construct-with-wrap
		      (syntax-rules ()
			((construct-with-wrap ?wrap)
			 (lambda extension-field-values
			   (lambda for-rtd-field-values
			     (if (not (= (length for-rtd-field-values) for-rtd-field-count))
				 (assertion-violation 'record-constructor
						      "wrong number of arguments to record constructor"
						      for-rtd for-rtd-field-values))
			     (?wrap
			      (apply record real-rtd
				     (append for-rtd-field-values extension-field-values)))))))))
	  (if (record-type-immutable? real-rtd)
	      (construct-with-wrap (lambda (r)
				     (make-immutable! r)
				     r))
	      (construct-with-wrap values))))))))

; needs optimization
(define (record rtd . field-vals)
  (let ((r (primitive:make-record (+ 1 (length field-vals)) (unspecific))))
    (primitive:record-set! r 0 rtd)
    (let loop ((i 1)
	       (field-vals field-vals))
      (if (null? field-vals)
	  r
	  (begin
	    (primitive:record-set! r i (car field-vals))
	    (loop (+ 1 i) (cdr field-vals)))))))

(define (record-constructor desc)
  (let ((rtd (record-constructor-descriptor-rtd desc)))
    (if (record-constructor-descriptor-custom-protocol? desc) ; +++
	((record-constructor-descriptor-protocol desc)
	 ((make-make-seeder rtd desc)))
	(let ((construct (record-standard-constructor rtd)))
	  (if (record-type-immutable? rtd)
	      (lambda args
		(let ((r (apply construct args)))
		  (make-immutable! r)
		  r))
	      construct)))))

(define (record-with-rtd? obj rtd)
  (and (primitive:record? obj)
       (record-type<=? (primitive:record-type obj) rtd)))

(define (record-accessor rtd field-id)
  (let ((index (+ 1 (field-id-index rtd field-id))))
    (lambda (thing)
      (if (not (record-with-rtd? thing rtd))
	  (assertion-violation 'record-accessor "not a record of record type" thing rtd))
      (primitive:record-ref thing index))))

(define (record-mutator rtd field-id)
  (if (not (record-field-mutable? rtd field-id))
      (assertion-violation 'record-mutator
			   "record-mutator called on immutable field" rtd field-id))
  (let ((index (+ 1 (field-id-index rtd field-id))))
    (lambda (thing val)
      (if (not (record-with-rtd? thing rtd))
	  (assertion-violation 'record-mutator "not a record of record type" thing rtd))
      (primitive:record-set! thing index val))))

; A FIELD-ID is an index, which refers to a field in RTD itself.
(define (field-id-index rtd field-id)
  (+ (record-type-parent-size rtd)
     field-id))

(define (record-field-mutable? rtd field-id)
  (field-spec-mutable? (list-ref (record-type-field-specs rtd) field-id)))

(define (record-type-parent-size rt)
  (cond
   ((record-type-parent rt)
    => record-type-size)
   (else 0)))

; Initialization
(set-nongenerative-record-types-table! (make-symbol-table))
