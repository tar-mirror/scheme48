; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Martin Gasbichler, Mike Sperber


; Operations on integers.

; These expect their arguments to be integers.

(define integer-add       bignum-add)
(define integer-subtract  bignum-subtract)
(define integer-multiply  bignum-multiply)

(define integer-remainder bignum-remainder)
(define integer-quotient  bignum-quotient)
(define integer-divide    bignum-divide)

(define integer-abs bignum-abs)

; Fixnums and bignums are disjoint.

(define (integer= x y)
  (cond ((and (fixnum? x)
	      (fixnum? y))
	 (fixnum= x y))
	((and (bignum? x)
	      (bignum? y))
	 (bignum= x y))
	(else
	 #f)))

; Positive bignums are greater than all fixnums, negative bignums are less
; than all fixnums.

(define (integer< x y)
  (cond ((fixnum? x)
	 (if (fixnum? y)
	     (fixnum< x y)
	     (bignum-positive? y)))
	((fixnum? y)
	 (not (bignum-positive? x)))
	(else
	 (bignum< x y))))

(define (integer<= x y)
  (not (integer< y x)))

(define (integer> x y)
  (integer< y x))

(define (integer>= x y)
  (not (integer< x y)))

(define integer-arithmetic-shift bignum-arithmetic-shift)

(define integer-bitwise-not  bignum-bitwise-not)

(define integer-bit-count bignum-bit-count)

(define integer-bitwise-and bignum-bitwise-and)

(define integer-bitwise-ior bignum-bitwise-ior)
 
(define integer-bitwise-xor bignum-bitwise-xor)
		
(define (enter-integer x key)
  (if  (or (too-big-for-fixnum? x)
	   (too-small-for-fixnum? x))
       (long->bignum x key)
       (enter-fixnum x)))

(define (enter-unsigned-integer x key)
  (if  (unsigned-too-big-for-fixnum? x)
       (unsigned-long->bignum x key)
       (enter-fixnum (unsigned->integer x))))

(define long-as-integer-size (bignum-bits-to-size 32))
