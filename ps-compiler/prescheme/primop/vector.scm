; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Mike Sperber

(define-polymorphic-scheme-primop make-vector allocate
  (lambda (call)
    (make-pointer-type (node-type (call-arg call 1)))))

(define-polymorphic-scheme-primop vector-ref read
  (lambda (call)
    (pointer-type-to (node-type (call-arg call 0)))))

(define-nonsimple-scheme-primop vector-set! write)

(define-scheme-primop make-string   allocate type/string)
(define-scheme-primop string-length          type/integer)
(define-scheme-primop string-ref    read     type/char)

(define-nonsimple-scheme-primop string-set! write)

(define-polymorphic-scheme-primop make-record allocate
  (lambda (call)
    (literal-value (call-arg call 1))))

(define-polymorphic-scheme-primop record-ref read
  (lambda (call)
    (record-field-type
     (get-record-type-field (literal-value (call-arg call 1))
			    (literal-value (call-arg call 2))))))

(define-nonsimple-scheme-primop record-set! write)

(define (simplify-type-case-test call)
  (simplify-args call 0))

; There should be no discovered calls to TYPE-CASE.

(define (expand-type-case call)
  (bug "Trying to expand a call to TYPE-CASE (~D) ~S"
       (node-hash (node-parent (nontrivial-ancestor call)))
       call))

; See simplify-if? in simplify/call.scm

(define (simplify-type-case? call index value)
  #f)

(define-scheme-cond-primop type-case
  simplify-type-case-test
  expand-test
  simplify-type-case?)

(define-scheme-primop deallocate        deallocate type/unit)
(define-scheme-primop allocate-memory   allocate   type/address)
(define-scheme-primop deallocate-memory deallocate type/unit)

(define (simplify-address+ call)
  (simplify-args call 0)
  ((pattern-simplifier
    ((address+ a '0) a)
    ((address+ (address+ a x) y) (address+ a (+ x y))))
   call))

(define-scheme-primop address+ #f type/address simplify-address+)

(define-scheme-primop address-difference type/address)
(define-scheme-primop address=           type/boolean)
(define-scheme-primop address<           type/boolean)
(define-scheme-primop address->integer   type/integer)
(define-scheme-primop integer->address   type/address)

(define-scheme-primop copy-memory!  write type/unit)
(define-scheme-primop memory-equal?       type/boolean)
(define-scheme-primop byte-ref      read  type/integer)
(define-scheme-primop word-ref      read  type/integer)
(define-scheme-primop flonum-ref    read  type/float)

(define-nonsimple-scheme-primop byte-set! write)
(define-nonsimple-scheme-primop word-set! write)
(define-nonsimple-scheme-primop flonum-set! write)

; We delete the length argument because we don't need it.  This is allowable
; because trivial calls can't have WRITE side effects.

(define-scheme-primop char-pointer->string #f type/string
  (lambda (call)
    (if (= 2 (call-arg-count call))
	(remove-call-arg call 1))))

(define-scheme-primop char-pointer->nul-terminated-string type/string)

; (COMPUTED-GOTO <exit0> <exit1> ... <exitN> <dispatch-value>)

; Remove an unecessary coercion on the dispatch-value, if possible.

(define (simplify-computed-goto call)
  (simplify-args call 0)
  (let ((value (call-arg call (call-exits call))))
    (cond ((and (call-node? value)
		(eq? 'coerce (primop-id (call-primop value)))
		(< (call-exits call) 256)
		(eq? type/integer (literal-value (call-arg value 1))))
	   (replace value (detach (call-arg value 0)))))))

(define-nonsimple-scheme-primop computed-goto #f simplify-computed-goto)



