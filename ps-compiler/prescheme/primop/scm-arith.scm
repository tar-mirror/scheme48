; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Mike Sperber


; Arithmetic inference rules

(define (arith-op-rule args node depth return?)
  (for-each (lambda (arg)
	      (unify! (infer-type arg depth) type/integer node))
	    args)
  type/integer)

(define (arith-float-op-rule args node depth return?)
  (for-each (lambda (arg)
	      (unify! (infer-type arg depth) type/float node))
	    args)
  type/float)

(define (arith-unsigned-integer-op-rule args node depth return?)
  (for-each (lambda (arg)
	      (unify! (infer-type arg depth) type/unsigned-integer node))
	    args)
  type/unsigned-integer)

(define (arith-comparison-rule args node depth return?)
  (arith-op-rule args node depth return?)
  type/boolean)

(define (float-comparison-rule args node depth return?)
  (arith-float-op-rule args node depth return?)
  type/boolean)

(define (unsigned-integer-comparison-rule args node depth return?)
  (arith-unsigned-integer-op-rule args node depth return?)
  type/boolean)

(define (integer-binop-rule args node depth return?)
  (check-arg-type args 0 type/integer depth node)
  (check-arg-type args 1 type/integer depth node)
  type/integer)

(define (float-binop-rule args node depth return?)
  (check-arg-type args 0 type/float depth node)
  (check-arg-type args 1 type/float depth node)
  type/float)

(define (unsigned-integer-binop-rule args node depth return?)
  (check-arg-type args 0 type/unsigned-integer depth node)
  (check-arg-type args 1 type/unsigned-integer depth node)
  type/unsigned-integer)

(define (integer-monop-rule args node depth return?)
  (check-arg-type args 0 type/integer depth node)
  type/integer)

(define (integer-comparison-rule args node depth return?)
  (check-arg-type args 0 type/integer depth node)
  type/boolean)

;----------------------------------------------------------------
; Arithmetic

(define (nonnegative-integer? x)
  (and (integer? x)
       (not (negative? x))))

(define-complex-primitive (+ . integer?) +
  arith-op-rule
  (lambda (x y) (+ x y))
  (lambda (args type)
    (if (null? args)
	(make-literal-node 0 type/integer)
	(n-ary->binary args
		       (make-literal-node (get-prescheme-primop '+))
		       type))))

(define-complex-primitive (fl+ . real?) +
  arith-float-op-rule
  (lambda (x y) (fl+ x y))
  (lambda (args type)
    (if (null? args)
	(make-literal-node 0.0 type/float)
	(n-ary->binary args
		       (make-literal-node (get-prescheme-primop 'fl+))
		       type))))

(define-complex-primitive (un+ . nonnegative-integer?) +
  arith-unsigned-integer-op-rule
  (lambda (x y) (un+ x y))
  (lambda (args type)
    (if (null? args)
	(make-literal-node 0 type/unsigned-integer)
	(n-ary->binary args
		       (make-literal-node (get-prescheme-primop 'un+))
		       type))))

(define-complex-primitive (* . integer?) *
  arith-op-rule
  (lambda (x y) (* x y))
  (lambda (args type)
    (if (null? args)
	(make-literal-node 1)
	(n-ary->binary args
		       (make-literal-node (get-prescheme-primop '*))
		       type))))

(define-complex-primitive (fl* . real?) *
  arith-float-op-rule
  (lambda (x y) (fl* x y))
  (lambda (args type)
    (if (null? args)
	(make-literal-node 1.0)
	(n-ary->binary args
		       (make-literal-node (get-prescheme-primop 'fl*))
		       type))))

(define-complex-primitive (un* . nonnegative-integer?) *
  arith-unsigned-integer-op-rule
  (lambda (x y) (un* x y))
  (lambda (args type)
    (if (null? args)
	(make-literal-node 1)
	(n-ary->binary args
		       (make-literal-node (get-prescheme-primop 'un*))
		       type))))

(define (subtract-action name)
  (lambda args
    (if (or (null? (cdr args))
	    (null? (cddr args)))
	(apply - args)
	(user-error "error while evaluating: type error ~A" (cons name args)))))

(define (subtract-checker type name)
  (lambda (args node depth return)
    (case (length args)
      ((1)
       (check-arg-type args 0 type depth node)
       type)
      ((2)
       (check-arg-type args 0 type depth node)
       (check-arg-type args 1 type depth node)
       type)
      (else
       (user-error "wrong number of arguments to ~S in ~S"
		   name
		   (schemify node))))))

(define (subtract-maker name zero)
  (lambda (args type)
    (let ((primop (get-prescheme-primop name)))
      (if (null? (cdr args))
	  (make-primop-call-node primop
				 (list (make-literal-node zero) (car args))
				 type)
	  (make-primop-call-node primop args type)))))

(define-complex-primitive (- integer? . integer?)
  (subtract-action '-)
  (subtract-checker type/integer '-)
  (lambda (x y) (- x y))
  (subtract-maker '- 0))

(define-complex-primitive (fl- real? . real?)
  (subtract-action '-)
  (subtract-checker type/float 'fl-)
  (lambda (x y) (fl- x y))
  (subtract-maker 'fl- 0.0))

(define-complex-primitive (un- nonnegative-integer? . nonnegative-integer?)
  (subtract-action '-)
  (subtract-checker type/unsigned-integer 'un-)
  (lambda (x y) (un- x y))
  (subtract-maker 'un- 0))

(define (n-ary->binary args proc type)
  (let loop ((args args))
    (if (null? (cdr args))
	(car args)
	(loop (cons (make-call-node proc
				    (list (car args) (cadr args))
				    type)
		    (cddr args))))))

(define-syntax define-binary-primitive
  (syntax-rules ()
    ((define-binary-primitive id op predicate type-reconstruct)
     (define-complex-primitive (id predicate predicate) op
       type-reconstruct
       (lambda (x y) (id x y))
       (lambda (args type)
	 (make-primop-call-node (get-prescheme-primop 'id) args type))))))

(define-binary-primitive =   = integer?             arith-comparison-rule)
(define-binary-primitive <   < integer?             arith-comparison-rule)
(define-binary-primitive fl= = real?                float-comparison-rule)
(define-binary-primitive fl< < real?                float-comparison-rule)
(define-binary-primitive un= = nonnegative-integer? unsigned-integer-comparison-rule)
(define-binary-primitive un< < nonnegative-integer? unsigned-integer-comparison-rule)

(define-semi-primitive (>  integer? integer?) >
  arith-comparison-rule
  (lambda (x y) (< y x)))

(define-semi-primitive (<= integer? integer?) <=
  arith-comparison-rule
  (lambda (x y) (not (< y x))))

(define-semi-primitive (>= integer? integer?) >=
  arith-comparison-rule
  (lambda (x y) (not (< x y))))

(define-semi-primitive (fl> real? real?) >
  float-comparison-rule
  (lambda (x y) (fl< y x)))

(define-semi-primitive (fl<= real? real?) <=
  float-comparison-rule
  (lambda (x y) (not (fl< y x))))

(define-semi-primitive (fl>= real? real?) >=
  float-comparison-rule
  (lambda (x y) (not (fl< x y))))

(define-semi-primitive (un> nonnegative-integer? nonnegative-integer?) >
  unsigned-integer-comparison-rule
  (lambda (x y) (un< y x)))

(define-semi-primitive (un<= nonnegative-integer? nonnegative-integer?) <=
  unsigned-integer-comparison-rule
  (lambda (x y) (not (un< y x))))

(define-semi-primitive (un>= nonnegative-integer? nonnegative-integer?) >=
  unsigned-integer-comparison-rule
  (lambda (x y) (not (un< x y))))

(define-binary-primitive quotient    quotient  integer?             integer-binop-rule)
(define-binary-primitive unquotient  quotient  nonnegative-integer? unsigned-integer-binop-rule)
(define-binary-primitive fl/         /         real?                float-binop-rule)
(define-binary-primitive remainder   remainder integer?             integer-binop-rule)
(define-binary-primitive unremainder remainder nonnegative-integer? integer-binop-rule)
(define-binary-primitive modulo      modulo    integer?             integer-binop-rule)

(define-primitive bitwise-and
  ((integer? type/integer) (integer? type/integer))
  type/integer)

(define-primitive bitwise-ior
  ((integer? type/integer) (integer? type/integer))
  type/integer)

(define-primitive bitwise-xor
  ((integer? type/integer) (integer? type/integer))
  type/integer)

(define-primitive bitwise-not
  ((integer? type/integer))
  type/integer)

(define-primitive shift-left
  ((integer? type/integer) (integer? type/integer))
  type/integer
  ashl)

(define-primitive logical-shift-right
  ((integer? type/integer) (integer? type/integer))
  type/integer
  lshr)

(define-primitive arithmetic-shift-right
  ((integer? type/integer) (integer? type/integer))
  type/integer
  ashr)

(define-semi-primitive (abs integer?) abs
  arith-op-rule
  (lambda (n) (if (< n 0) (- 0 n) n)))

(define-semi-primitive (zero? integer?) zero?
  arith-comparison-rule
  (lambda (n) (= n 0)))

(define-semi-primitive (positive? integer?) positive?
  arith-comparison-rule
  (lambda (n) (< 0 n)))

(define-semi-primitive (negative? integer?) negative?
  arith-comparison-rule
  (lambda (n) (< n 0)))

(define-semi-primitive (even? integer?) even?
  integer-comparison-rule
  (lambda (n) (= 0 (remainder n 2))))

(define-semi-primitive (odd? integer?) odd?
  integer-comparison-rule
  (lambda (n) (not (even? n))))
  
(define-semi-primitive (max integer? . integer?) max
  arith-op-rule
  (lambda (x y)
    (if (< x y) y x)))
  
(define-semi-primitive (min integer? . integer?) min
  arith-op-rule
  (lambda (x y)
    (if (< x y) x y)))

(define-semi-primitive (expt integer? positive-integer?) expt
  arith-op-rule
  (lambda (x y)
    (do ((r x (* r x))
	 (y y (- y 1)))
	((<= y 0)
	 r))))

(define (unsigned->integer x) x)
(define (integer->unsigned x) x)

(define-primitive unsigned->integer ((nonnegative-integer? type/unsigned-integer)) type/integer)
(define-primitive integer->unsigned ((integer? type/integer)) type/unsigned-integer)
