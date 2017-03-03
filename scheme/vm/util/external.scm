; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Martin Gasbichler


(define (fake-it name)
  (lambda args
    (display "Call to ")
    (display (cons name args))
    (newline)
    0))

(define extended-vm        (fake-it 'extended-vm))
(define external-call      (fake-it 'call-external-value))
(define external-call-2    (fake-it 'call-external-value-2))
(define schedule-interrupt (fake-it 'schedule-interrupt))

(define dequeue-external-event! (fake-it 'dequeue-external-event!))

(define-syntax document-it
  (syntax-rules 
   ()
   ((document-it name op)
    (define (name . args)
      (display "Call to ")
      (display (cons name args))
      (newline)
      (apply op args)))))

(document-it external-bignum-make-cached-constants (lambda () #f))
(document-it external-bignum-make-zero (lambda () #f))
(document-it external-bignum-make-one (lambda (x) #f))
(document-it external-bignum-add +)
(document-it external-bignum-subtract -)
(document-it external-bignum-multiply *)
(document-it external-bignum-quotient quotient) 
(document-it external-bignum-remainder remainder) 
(document-it external-bignum-divide /)
(document-it external-bignum-equal? =) 
(document-it external-bignum-compare (lambda (x y)
				       (if (< x y)
					   -1
					   (if (= x y)
					       0
					       1))))
(document-it external-bignum-test (lambda (x)
				    (if (< x 0) -1
					(if (= x 0) 0
					    1))))
(document-it external-bignum-negate  (lambda (x) (- x)))
(document-it external-bignum-from-long (lambda (x) x))
(document-it external-bignum-from-unsigned-long (lambda (x) x))
(document-it external-bignum-fits-in-word? 
	     (lambda (bignum word-length two-compl?)
	       (and (>= bignum -134217728)
		    (<= bignum 134217727))))
(document-it external-bignum->long (lambda (x) x))
(document-it external-bignum-bitwise-and bitwise-and)
(document-it external-bignum-bitwise-xor bitwise-xor)
(document-it external-bignum-bitwise-ior bitwise-ior)
(document-it external-bignum-bitwise-not bitwise-not)
(document-it external-bignum-bit-count   bit-count)
(document-it external-bignum-arithmetic-shift arithmetic-shift)

(define (trace-external-calls)
  (fake-it 'trace-external-calls))

(define (real-time) 0)
(define (run-time) 0)
(define (cheap-time) 0)

(define s48-call-native-procedure (fake-it 's48-call-native-code))
(define s48-invoke-native-continuation (fake-it 's48-call-native-code))
(define s48-native-return 0)
(define s48-jump-native (fake-it 's48-jump-native))

(define get-proposal-lock!     (fake-it 'get-proposal-lock!))
(define release-proposal-lock! (fake-it 'release-proposal-lock!))

(define (shared-ref x) x)
(define-syntax shared-set!
  (syntax-rules ()
    ((shared-set! x v)
     (set! x v))))

(define (get-os-string-encoding)
  "UTF-8")

(define host-architecture "s48")

(define (argument-type-violation val)
  (fake-it 'argument-type-violation))

(define (range-violation val min max)
  (fake-it 'range-violation))
