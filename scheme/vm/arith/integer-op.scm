; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Martin Gasbichler, Mike Sperber

; Integer-only primitive operations

; These predicates are used to characterize the numeric representations that
; are implemented in the VM.

(define (unary-lose x)
  (raise-exception wrong-type-argument 0 x))

(define (binary-lose x y)
  (raise-exception wrong-type-argument 0 x y))

; They're all numbers, even if we can't handle them.

(define-primitive number? (any->)
  (lambda (x)
    (or (fixnum? x)
	(bignum? x)
	(ratnum? x)
	(double? x)
	(extended-number? x)))
  return-boolean)

(define (integer? n)
  (or (fixnum? n)
      (bignum? n)))
  
(define (vm-integer? n)
  (cond ((integer? n)
	 (goto return-boolean #t))
	((extended-number? n)
	 (unary-lose n))
	(else
	 (goto return-boolean #f))))

(define-primitive integer?  (any->)
  (lambda (n)
    (cond ((or (fixnum? n)
	       (bignum? n))
	       (goto return-boolean #t))
	  ((or (extended-number? n)
	       (double? n))
	   (unary-lose n))
	  (else
	   (goto return-boolean #f)))))

(define vm-number-predicate
  (lambda (n)
    (cond ((or (fixnum? n)
	       (bignum? n)
	       (ratnum? n)
	       (double? n))
	   (goto return-boolean #t))
	  ((extended-number? n)
	   (unary-lose n))
	  (else
	   (goto return-boolean #f)))))

(define-primitive rational? (any->)
  (lambda (n)
    (cond ((or (fixnum? n)
	       (bignum? n)
	       (ratnum? n))
	   (goto return-boolean #t))
	  ((double? n)
	   (goto return-boolean (flonum-rational? n)))
	  ((extended-number? n)
	   (unary-lose n))
	  (else
	   (goto return-boolean #f)))))

(define-primitive real?     (any->) vm-number-predicate)
(define-primitive complex?  (any->) vm-number-predicate)

; These assume that ratnums and doubles aren't being used.

;(define-primitive integer?  (any->) vm-integer?)
;(define-primitive rational? (any->) vm-integer?)
;(define-primitive real?     (any->) vm-integer?)
;(define-primitive complex?  (any->) vm-integer?)

;----------------
; A macro for defining primitives that only operate on integers.

(define-syntax define-integer-only
  (syntax-rules ()
    ((define-integer-only (opcode arg) value)
     (define-integer-only (opcode arg) (any->) value))
    ((define-integer-only (opcode arg0 arg1) value)
     (define-integer-only (opcode arg0 arg1) (any-> any->) value))
    ((define-integer-only (opcode arg ...) specs value)    
     (define-primitive opcode specs
       (lambda (arg ...)
	 (if (and (integer? arg) ...)
	     (goto return value)
	     (raise-exception wrong-type-argument 0 arg ...)))))))

; These primitives have a simple answer in the case of integers; for all others
; they punt to the run-time system.

(define-integer-only (exact?      n) true)
(define-integer-only (real-part   n) n)
(define-integer-only (imag-part   n) (enter-fixnum 0))
(define-integer-only (floor       n) n)
(define-integer-only (numerator   n) n)
(define-integer-only (denominator n) (enter-fixnum 1))

(define-primitive angle (vm-integer->)
  (lambda (n)
    (if (if (fixnum? n)
	    (fixnum> n (enter-fixnum 0))
	    (bignum-nonnegative? n))
	(goto return (enter-fixnum 0))
	(unary-lose n))))

(define-primitive magnitude (vm-integer->) 
  (lambda (x)
    (if (fixnum? x)
	(goto return-integer (abs (extract-fixnum x)))
	(goto return (integer-abs x)))))

; These all just raise an exception and let the run-time system do the work.

(define-syntax define-punter
  (syntax-rules ()
    ((define-punter opcode)
     (define-primitive opcode (any->) unary-lose))))

(define-punter exact->inexact)
(define-punter inexact->exact)
(define-punter exp)
(define-punter log)
(define-punter sin)
(define-punter cos)
(define-punter tan)
(define-punter asin)
(define-punter acos)
(define-punter sqrt)

(define-syntax define-punter2
  (syntax-rules ()
    ((define-punter2 opcode)
     (define-primitive opcode (any-> any->) binary-lose))))

(define-punter  atan1)
(define-punter2 atan2)
(define-punter2 make-polar)
(define-punter2 make-rectangular)

(define-syntax define-fixnum-or-integer
  (syntax-rules ()
    ((define-fixnum-or-integer (opcode arg) fixnum-val integer-val)
     (define-fixnum-or-integer (opcode arg)
       (any->)
       fixnum-val integer-val))
    ((define-fixnum-or-integer (opcode arg0 arg1) fixnum-val integer-val)
     (define-fixnum-or-integer (opcode arg0 arg1)
       (any-> any->)
       fixnum-val integer-val))
    ((define-fixnum-or-integer (opcode arg ...) specs fixnum-val integer-val)
     (define-primitive opcode specs
       (lambda (arg ...)
	 (if (and (fixnum? arg) ...)
	     (goto return fixnum-val)
	     (if (and (integer? arg) ...)
		 (goto return integer-val)
		 (raise-exception wrong-type-argument 0 arg ...))))))))

(define-syntax define-fixnum-or-integer-or-float
  (syntax-rules ()
    ((define-fixnum-or-integer (opcode arg) fixnum-val integer-val float-val)
     (define-fixnum-or-integer (opcode arg) (any->)
       fixnum-val integer-val float-val))
    ((define-fixnum-or-integer-or-float (opcode arg0 arg1)
       fixnum-val integer-val float-val)
     (define-fixnum-or-integer-or-float (opcode arg0 arg1)
       (any-> any->)
       fixnum-val integer-val float-val))
    ((define-fixnum-or-integer-or-float (opcode arg ...) specs
       fixnum-val integer-val float-val)
     (define-primitive opcode specs
       (lambda (arg ...)
	 (cond ((and (fixnum? arg) ...)
		(goto return fixnum-val))
	       ((and (integer? arg) ...)
		(goto return integer-val))
	       ((and (double? arg) ...)
		(goto return float-val))
	       (else
		(raise-exception wrong-type-argument 0 arg ...))))))))

(define-fixnum-or-integer-or-float (+ x y) 
  (enter-integer (+ (extract-fixnum x) 
                    (extract-fixnum y))
                 (ensure-space long-as-integer-size))
  (integer-add x y)
  (flonum-add x y))

(define-fixnum-or-integer-or-float (- x y) 
  (enter-integer (- (extract-fixnum x) 
		    (extract-fixnum y))
		 (ensure-space long-as-integer-size))
  (integer-subtract x y)
  (flonum-subtract x y))

(define (return-integer x)
  (goto return (enter-integer x (ensure-space long-as-integer-size))))

(define-primitive * (any-> any->)
  (lambda (x y)
    (cond ((and (fixnum? x) (fixnum? y))
	   (goto multiply-carefully x y 
		 return-integer
		 (lambda (x y)
		   (goto return (integer-multiply x y)))))
	  ((and (integer? x) (integer? y))
	   (goto return (integer-multiply x y)))
	  ((and (double? x) (double? y))
	   (goto return (flonum-multiply x y)))
	  (else
	   (binary-lose x y)))))

;----------------------------------------------------------------
; division and friends

(define-primitive / (any-> any->)
  (lambda (x y)
    (cond ((= y (enter-fixnum 0))
	   (binary-lose x y))
          ((and (fixnum? x)
		(fixnum? y))
	   (divide-carefully x y return-integer 
			     binary-lose))
	  ((and (integer? x)
		(integer? y))
	   (call-with-values
	    (lambda ()
	      (integer-divide x y))
	    (lambda (div-by-zero? quot rem x y)
	      (if (and (not div-by-zero?)
		       (fixnum? rem)
		       (= (enter-fixnum 0) rem))
		  (goto return quot)
		  (binary-lose x y)))))
	  ((and (double? x) (double? y))
	   (goto return (flonum-divide x y)))
	  (else
	   (binary-lose x y)))))

(define (divide-action fixnum-op integer-op)
  (lambda (x y)
    (cond ((= y (enter-fixnum 0))
	   (binary-lose x y))
	  ((and (fixnum? x)
		(fixnum? y))
	   (fixnum-op x
		      y
		      return 
		      (lambda (x y)
			(goto return (integer-op x y)))))
	  ((and (integer? x)
		(integer? y))
	   (goto return
		 (integer-op x y)))
	  (else
	   (binary-lose x y)))))

(let ((action (divide-action quotient-carefully integer-quotient)))
  (define-primitive quotient (any-> any->) action))

(let ((action (divide-action remainder-carefully integer-remainder)))
  (define-primitive remainder (any-> any->) action))

;----------------------------------------------------------------
; comparisons

(define-syntax define-comparison
  (syntax-rules ()
    ((define-comparison op fixnum integer float)
     (define-fixnum-or-integer-or-float (op x y)
       (enter-boolean (fixnum x y))
       (enter-boolean (integer x y))
       (enter-boolean (float x y))))))

(define-comparison =  fixnum=  integer=  flonum=)     
(define-comparison <  fixnum<  integer<  flonum<)     
(define-comparison >  fixnum>  integer>  flonum>)     
(define-comparison <= fixnum<= integer<= flonum<=)     
(define-comparison >= fixnum>= integer>= flonum>=)     

;----------------------------------------------------------------
; bitwise operations

; Shifting left by a bignum number of bits loses; shifting right gives 0 or
; -1 depending on the sign of the first argument.

(define-primitive arithmetic-shift (any-> any->)
  (lambda (x y)
    (cond ((bignum? y)
	   (goto shift-by-bignum x y))
	  ((not (fixnum? y))
	   (binary-lose x y))
	  ((fixnum? x)
	   (goto shift-carefully x y return-integer
		 (lambda (x y)
		   (goto return (integer-arithmetic-shift x y)))))
	  ((bignum? x) 
	   (goto return (integer-arithmetic-shift x y)))
	  (else
	   (binary-lose x y)))))

(define (shift-by-bignum x y)	  
  (cond ((bignum-positive? y)
	 (raise-exception arithmetic-overflow 0 x y))
	((fixnum? x)
	 (goto return
	       (if (fixnum<= (enter-fixnum 0)
			     x)
		   (enter-fixnum 0)
		   (enter-fixnum -1))))
	((bignum? x)
	 (goto return
	       (if (bignum-positive? x)
		   (enter-fixnum 0)
		   (enter-fixnum -1))))
	(else
	 (raise-exception arithmetic-overflow 0 x y))))

(define-fixnum-or-integer (bitwise-not x)
  (fixnum-bitwise-not x)
  (integer-bitwise-not x))

(define-fixnum-or-integer (bit-count x)
  (fixnum-bit-count x)
  (integer-bit-count x))

(define-fixnum-or-integer (bitwise-and x y)
  (fixnum-bitwise-and x y)
  (integer-bitwise-and x y))

(define-fixnum-or-integer (bitwise-ior x y)
  (fixnum-bitwise-ior x y)
  (integer-bitwise-ior x y))

(define-fixnum-or-integer (bitwise-xor x y)
  (fixnum-bitwise-xor x y)
  (integer-bitwise-xor x y))
