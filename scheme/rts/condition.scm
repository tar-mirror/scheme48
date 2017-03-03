; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber

; Sort-of the R6RS condition library.

(define-record-type &condition
  (make-simple-condition)
  simple-condition?)

(define-record-type &compound-condition
  (make-compound-condition components)
  compound-condition?
  (components explode-condition))

(define-record-discloser &compound-condition
  (lambda (r)
    (cons 'compound-condition
	  (explode-condition r))))

(define (simple-conditions con)
  (cond
   ((simple-condition? con)
    (list con))
   ((compound-condition? con)
    (explode-condition con))
   (else
    (assertion-violation 'simple-conditions
			 "not a condition"
			 con))))

(define (condition? thing)
  (or (simple-condition? thing)
      (compound-condition? thing)))

(define (condition . components)
  (make-compound-condition
   (apply append
	  (map (lambda (component)
		 (cond
		  ((simple-condition? component)
		   (list component))
		  ((compound-condition? component)
		   (explode-condition component))
		  (else
		   (assertion-violation 'condition
					"component wasn't a condition"
					component))))
	       components))))

(define (condition-predicate type)
  (if (not (record-type<=? type &condition))
      (assertion-violation 'condition-predicate
			   "not a subtype of &condition"
			   type))
  (let ((simple-pred (record-predicate type)))
    (lambda (con)
      (cond
       ((simple-condition? con)
	(simple-pred con))
       ((compound-condition? con)
	(any? simple-pred (explode-condition con)))
       (else #f)))))

(define (condition-accessor type simple-access)
  (if (not (record-type<=? type &condition))
      (assertion-violation 'condition-predicate
			   "not a subtype of &condition"
			   type))
  (let ((simple-pred (record-predicate type)))
    (lambda (con)
      (cond
       ((simple-condition? con)
	(simple-access con))
       ((compound-condition? con)
	(cond
	 ((first simple-pred (explode-condition con))
	  => simple-access)
	 (else
	  (assertion-violation '<condition-accessor>
			       "condition isn't of type"
			       con type))))
       (else
	(assertion-violation '<condition-accessor>
			     "condition isn't of type"
			     con type))))))

(define-syntax define-condition-type
  (syntax-rules ()
    ((define-condition-type ?name ?supertype ?constructor ?predicate
       (?field1 ?accessor1) ...)
     (define-condition-type-helper
       ?name ?supertype ?constructor ?predicate
       ((?field1 ?accessor1) ...)
       ()))))

(define-syntax define-condition-type-helper
  (syntax-rules ()
    ((define-condition-type-helper
       ?name ?supertype ?constructor ?predicate
       ((?field1 ?accessor1) ?rest ...)
       (?spec1 ...))
     (define-condition-type-helper
       ?name ?supertype ?constructor ?predicate
       (?rest ...)
       (?spec1 ... (?field1 ?accessor1 temp-condition-accessor))))
    ((define-condition-type-helper
       ?name ?supertype ?constructor ?predicate
       ()
       ((?field1 ?accessor1 ?condition-accessor1) ...))
     (begin
       (define ?name (make-record-type '?name '(?field1 ...) ?supertype))

       (define-record-discloser ?name
	 (lambda (r)
	   (list '?name (?condition-accessor1 r) ...)))

       (define ?constructor (record-standard-constructor ?name))
       
       (define ?predicate (condition-predicate ?name))

       (define ?condition-accessor1 (record-accessor ?name '?field1))
       ...
       (define ?accessor1
	 (condition-accessor ?name ?condition-accessor1))
       ...))))
       
;; Utilities, defined locally to avoid having to load SRFI 1

;; (These need to come before the standard condition types below.)

(define (first pred list)
  (let loop ((list list))
    (cond ((null? list)
	   #f)
          ((pred (car list))
	   (car list))
          (else
	   (loop (cdr list))))))

(define (any? proc list)
  (let loop ((list list))
    (cond ((null? list)
	   #f)
          ((proc (car list))
	   #t)
          (else
	   (loop (cdr list))))))

;; Standard condition types

(define-condition-type &message &condition 
  make-message-condition message-condition?
  (message condition-message))

(define-condition-type &warning &condition
  make-warning warning?)

(define-condition-type &serious &condition
  make-serious-condition serious-condition?)

(define-condition-type &error &serious
  make-error error?)

(define-condition-type &violation &serious
  make-violation violation?)

(define-condition-type &non-continuable &violation
  make-noncontinuable-violation non-continuable-violation?)

(define-condition-type &implementation-restriction &violation
  make-implementation-restriction-violation implementation-restriction-violation?)

(define-condition-type &lexical &violation
  make-lexical-violation lexical-violation?)

(define-condition-type &syntax &violation
  make-syntax-violation syntax-violation?
  (form syntax-violation-form)
  (subform syntax-violation-subform))

(define-condition-type &undefined &violation
  make-undefined-violation undefined-violation?)

(define-condition-type &assertion &violation
  make-assertion-violation assertion-violation?)

(define-condition-type &irritants &condition
  make-irritants-condition irritants-condition?
  (irritants condition-irritants))

(define-condition-type &who &condition
  make-who-condition who-condition?
  (who condition-who))

; Scheme-48-specific condition types

; VM Exceptions

(define-condition-type &vm-exception &serious
  make-vm-exception vm-exception?
  (opcode vm-exception-opcode) ; number
  (reason vm-exception-reason) ; symbol
  )

; OS errors (errno or Windows error codes)

(define-condition-type &os-error &error
  make-os-error os-error?
  (code os-error-code))

; I/O errors

(define-condition-type &i/o-error &error
  make-i/o-error i/o-error?)

(define-condition-type &i/o-port-error &i/o-error
  make-i/o-port-error i/o-port-error?
  (port i/o-error-port))

; Decoding errors

(define-condition-type &decoding-error &error
  make-decoding-error decoding-error?
  (encoding-name decoding-error-encoding-name)
  (bytes decoding-error-bytes)
  (start decoding-error-start))

; Notes

(define-condition-type &note &condition
  make-note note?)

; Interrupts

(define-condition-type &interrupt &condition
  make-interrupt-condition interrupt-condition?
  (source interrupt-source))

; Decoding a condition for printing

;; Return a symbol describing the type,
;; a symbol or string describing the source of the problem, an error
;; message or #f, and a list of other objects describing the
;; problem.  Valid type symbols include:

;; ERROR, ASSERTION-VIOLATION, SYNTAX-VIOLATION, VM-EXCEPTION,
;; WARNING, SERIOUS, NOTE, INTERRUPT

(define (decode-condition con)
  (let ((type
	 (cond
	  ((error? con) 'error)
	  ((assertion-violation? con) 'assertion-violation)
	  ((syntax-violation? con) 'syntax-violation)
	  ((vm-exception? con) 'vm-exception)
	  ((serious-condition? con) 'serious)
	  ((interrupt-condition? con) 'interrupt)
	  ((warning? con) 'warning)
	  ((note? con) 'note)
	  (else 'unknown)))
	(who
	 (and (who-condition? con)
	      (condition-who con)))
	(message
	 (and (message-condition? con)
	      (condition-message con)))
	(stuff (if (irritants-condition? con)
		   (condition-irritants con)
		   '()))
	(syntax-stuff
	 (if (syntax-violation? con)
	     (let ((form (syntax-violation-form con))
		   (subform (syntax-violation-subform con)))
	       (if subform
		   (list form subform)
		   (list form)))
	     '()))
	(more-stuff
	 (delete-first
	  (lambda (con)	; make sure interesting subtypes still get printed
	    (memq (record-type con)
		  *covered-condition-txpes*))
	  ;; we don't expect interesting subtypes here
	  (delete-first
	   vm-exception?
	   (delete-first
	    message-condition?
	    (delete-first
	     who-condition?
	     (delete-first
	      irritants-condition?
	      (simple-conditions con))))))))
    (values type who message (append stuff syntax-stuff more-stuff))))

(define (delete-first pred? l)
  (let recur ((l l))
    (cond
     ((null? l) l)
     ((pred? (car l)) (cdr l))
     (else
      (cons (car l) (recur (cdr l)))))))

(define *covered-condition-txpes*
  (list &syntax &warning &note &interrupt &error &assertion &serious))
