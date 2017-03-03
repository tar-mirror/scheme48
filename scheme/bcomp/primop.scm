; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber

; Primops.

(define-record-type primop :primop
  (make-primop name type closed compilator)
  primop?
  (name primop-name)
  (type primop-type)
  (closed primop-closed)
  (compilator primop-compilator))

(define-record-discloser :primop
  (lambda (primop)
    `(primop ,(primop-name primop))))

(define primop-table (make-symbol-table))

; This is used to add definitions of the primitives to a package.

(define (walk-primops proc)
  (table-walk (lambda (name primop)
		(proc name (primop-type primop) primop))
	      primop-table))

(define (define-compiler-primitive name type compilator closed)
  (table-set! primop-table
	      name
	      (make-primop name (or type value-type) closed compilator)))

(define (get-primop name)
  (or (table-ref primop-table name)
      (assertion-violation 'get-primop "unknown compiler primitive" name)))
