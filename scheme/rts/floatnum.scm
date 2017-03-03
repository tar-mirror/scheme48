; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber

; Inexact rational arithmetic using hacked-in floating point numbers.

(define floatnum? double?)

(define-enumeration flop
  (fixnum->float
   string->float
   float->string
   exp log sin cos tan asin acos atan1 atan2 sqrt
   floor
   integer?
   float->fixnum
   quotient
   remainder))

(define-syntax floperate
  (syntax-rules ()
    ((floperate ?which ?x)
     (vm-extension (+ ?which 100) ?x))
    ((floperate ?which ?x ?y)
     (vm-extension (+ ?which 100) (cons ?x ?y)))
    ((floperate ?which ?x ?y ?z)
     (vm-extension (+ ?which 100) (vector ?x ?y ?z)))))

(define (float&float->float op)
  (lambda (a b)
    (let ((res (make-double)))
      (floperate op (x->float a) (x->float b) res)
      res)))

(define (float&float->boolean op)
  (lambda (a b)
    (floperate op (x->float a) (x->float b))))

(define (float1 op)
  (lambda (float)
    (floperate op float)))

(define (float2 op)
  (lambda (a b)
    (floperate op a b)))

(define (float->float op)
  (lambda (a)
    (let ((res (make-double)))
      (floperate op (x->float a) res)
      res)))

(define (string->float string)
  (let ((res (make-double)))
    (or (floperate (enum flop string->float) string res)
	(implementation-restriction-violation
	 'string->float
	 "not enough memory for STRING->FLOAT string buffer" string))))

; Call the VM to get a string

(define (float->string float)
  (let* ((res (make-string 40 #\space))
	 (len (floperate (enum flop float->string)
			 float
			 res)))
    (substring res 0 len)))

; Call back into the VM for a regular operation

(define (extend-float&float->val op)
  (lambda (a b)
    (op (x->float a) (x->float b))))

(define (x->float x)
  (cond ((double? x)
	 x)
	((integer? x)
	 (exact-integer->float (if (exact? x)
				   x
				   (inexact->exact x))))
	((rational? x)
	 ;; This loses when num or den overflows flonum range
	 ;; but x doesn't.
	 (float/ (numerator x) (denominator x)))
	(else
	 (assertion-violation 'x->float "cannot coerce to a float" x))))

; Conversion to/from exact integer

(define (exact-integer->float k)
  (or (fixnum->float k)
      (float+ (float* (fixnum->float definitely-a-fixnum)
		      (quotient k definitely-a-fixnum))
	      (fixnum->float (remainder k definitely-a-fixnum)))))

(define (fixnum->float k)    ;Returns #f is k is a bignum
  (let ((res (make-double)))
    (if (floperate (enum flop fixnum->float) k res)
	res
	#f)))

(define (float->exact-integer x)
  (or (float->fixnum x)
      (let ((d (fixnum->float definitely-a-fixnum)))
	(+ (* definitely-a-fixnum
	      (float->exact-integer (float-quotient x d)))
	   (float->fixnum (float-remainder x d))))))

(define definitely-a-fixnum (expt 2 23))    ;Be conservative

(define integral-floatnum? (float1 (enum flop integer?)))
(define float->fixnum      (float1 (enum flop float->fixnum)))

(define float+ (extend-float&float->val +))
(define float- (extend-float&float->val -))
(define float* (extend-float&float->val *))
(define float/ (extend-float&float->val /))
(define float-quotient (float&float->float (enum flop quotient)))
(define float-remainder (float&float->float (enum flop remainder)))
(define float-atan1 (float->float (enum flop atan1)))
(define float-atan2 (float&float->float (enum flop atan2)))

(define float= (extend-float&float->val =))
(define float< (extend-float&float->val <))

(define float-exp (float->float (enum flop exp)))
(define float-log (float->float (enum flop log)))
(define float-sin (float->float (enum flop sin)))
(define float-cos (float->float (enum flop cos)))
(define float-tan (float->float (enum flop tan)))
(define float-asin (float->float (enum flop asin)))
(define float-acos (float->float (enum flop acos)))
(define float-sqrt (float->float (enum flop sqrt)))
(define float-floor (float->float (enum flop floor)))

; This lets you do ,open floatnum to get faster invocation
; (begin 
;   (define exp float-exp)
;   (define log float-log)
;   (define sin float-sin)
;   (define cos float-cos)
;   (define tan float-tan)
;   (define asin float-asin)
;   (define acos float-acos)
;   (define (atan a . maybe-b)
;     (cond ((null? maybe-b)
; 	   (float-atan1 a))
; 	  ((null? (cdr maybe-b))
; 	   (float-atan2 a (car maybe-b)))
; 	  (else
; 	   (apply assertion-violation 'atan "too many arguments to ATAN" a maybe-b))))
;   (define sqrt float-sqrt))

(define (float-fraction-length x)
  (let ((two (exact-integer->float 2)))
    (do ((x x (float* x two))
	 (i 0 (+ i 1)))
	((integral-floatnum? x) i)
      (if (> i 3000) (assertion-violation 'float-fraction-length "I'm bored." x)))))

(define (float-denominator x)
  (expt (exact-integer->float 2) (float-fraction-length x)))

(define (float-numerator x)
  (float* x (float-denominator x)))

(define float-precision
  (delay
    (do ((n 0 (+ n 1))
	 (x (fixnum->float 1) (/ x 2)))
	((= (fixnum->float 1) (+ (fixnum->float 1) x)) n))))

(define infinity (delay (expt (exact->inexact 2) (exact->inexact 1500))))

(define (nan? x)
  (not (= x x)))

(define (float->exact x)
  (define (lose)
    (implementation-restriction-violation 'inexact->exact
					  "no exact representation"
					  x))

  (cond
   ((integral-floatnum? x)
    (float->exact-integer x))		;+++
   ((not (rational? x))
    (lose))
   (else
    (let ((deliver
	   (lambda (y d)
	     (let ((q (expt 2 (float-fraction-length y))))
	       (if (exact? q)
		   (let ((e (/ (/ (float->exact-integer
				   (float* y (exact-integer->float q)))
				  q)
			       d)))
		     (if (exact? e)
			 e
			 (lose)))
		   (lose))))))

	
      (if (and (< x (fixnum->float 1)) ; watch out for denormalized numbers
	       (> x (fixnum->float -1)))
	  (deliver (* x (expt (fixnum->float 2) (force float-precision)))
		   (expt 2 (force float-precision)))
	  (deliver x 1))))))


; Methods on floatnums

(define-method &integer? ((x <double>))
  (integral-floatnum? x))

(define-method &rational? ((n <double>))
  (and (not (nan? n))
       (not (= (force infinity) n))
       (not (= (- (force infinity)) n))))

(define-method &exact? ((x <double>)) #f)

(define-method &inexact->exact ((x <double>))
  (float->exact x))

(define-method &exact->inexact ((x <rational>))
  (x->float x))		;Should do this only if the number is within range.

(define-method &floor ((x <double>)) (float-floor x))

; beware infinite regress
(define-method &numerator ((x <double>)) (float-numerator x))
(define-method &denominator ((x <double>)) (float-denominator x))

(define (define-floatnum-method mtable proc)
  (define-method mtable ((m <rational>) (n <rational>)) (proc m n))
  ;; the horror
  (define-method mtable ((m <double>) (n <rational>)) (proc m n))
  (define-method mtable ((m <rational>) (n <double>)) (proc m n))
  (define-method mtable ((m <double>) (n <double>)) (proc m n)))

;; the numerical tower sucks
(define (define-floatnum-comparison mtable proc float-proc)
  (define-method mtable ((m <double>) (n <double>)) (float-proc m n))
  (define-method mtable ((m <double>) (n <rational>))
    (cond
     ((nan? m) #f) ; #### not always correct, when < is used to implement >
     ((= m (force infinity)) #f)
     ((= m (- (force infinity))) #t)
      
    (proc (float->exact m) n))
  (define-method mtable ((m <rational>) (n <double>))
    (proc m (float->exact n)))))

; the numerical tower sucks big-time
(define-method &= ((m <double>) (n <double>)) (float= m n))
(define-method &= ((m <double>) (n <rational>))
  (and (rational? m)
       (float= (float->exact m) n)))
(define-method &= ((m <rational>) (n <double>))
  (and (rational? n)
       (float= m (float->exact n))))

(define-method &< ((m <double>) (n <double>)) (float< m n))
(define-method &< ((m <double>) (n <rational>))
  (cond ((nan? m) #f)
	((= (force infinity) m) #f)
	((= (- (force infinity))  m) #t)
	(else
	 (float< (float->exact m) n))))
(define-method &< ((m <rational>) (n <double>))
  (cond ((nan? n) #f) ; #### not correct when < is used to implement >
	((= (force infinity) n) #t)
	((= (- (force infinity))  n) #f)
	(else
	 (float< m (float->exact n)))))

(define-floatnum-method &+ float+)
(define-floatnum-method &- float-)
(define-floatnum-method &* float*)
(define-floatnum-method &/ float/)
(define-floatnum-method &quotient float-quotient)
(define-floatnum-method &remainder float-remainder)
(define-floatnum-method &atan2 float-atan2)

(define-method &exp   ((x <rational>)) (float-exp   x))
(define-method &log   ((x <rational>))
  (cond 
   ((> x (exact->inexact 0)) ; avoid calling inexact->exact on X
    (float-log x))
   ((= x (exact->inexact 0))
    (if (exact? x)
	(assertion-violation 'log "log of exact 0 is undefined" x)
	(float-log x)))
   (else
    (next-method))))
(define-method &sqrt  ((x <rational>))
  (if (>= x (exact->inexact 0))
      (float-sqrt x)
      (next-method)))
(define-method &sin   ((x <rational>)) (float-sin   x))
(define-method &cos   ((x <rational>)) (float-cos   x))
(define-method &tan   ((x <rational>)) (float-tan   x))
(define-method &acos  ((x <rational>)) (float-acos  x))
(define-method &asin  ((x <rational>)) (float-asin  x))
(define-method &atan1 ((x <rational>)) (float-atan1 x))

(define-method &number->string ((n <double>) radix)
  (cond
   ((= radix 10)
    (float->string n))
   ((zero? n)
    (string-copy "#i0"))
   ((not (= n n))
    (string-copy "+nan.0"))
   ;; awkward, so we don't get IEEE representations into the image
   ((= n (/ 1 (exact->inexact 0)))
    (string-copy "+inf.0"))
   ((= n (/ -1 (exact->inexact 0)))
    (string-copy "-inf.0"))
   (else
    (let* ((p (abs (inexact->exact (numerator n))))
	   (q (inexact->exact (denominator n))))
      (string-append "#i"
		     (if (negative? n) "-" "")
		     (number->string p radix)
		     (if (not (= q 1))
			 (string-append "/"
					(number->string q radix))
			 ""))))))

; Recognizing a floating point number.  This doesn't know about `#'.

(define (float-string? s)
  (let ((len (string-length s)))
    (define (start)
      (and (< 1 len)
	   (let ((first (string-ref s 0))
		 (second (string-ref s 1)))
	     (if (char-numeric? first)
		 (digits 1 #f #f)
		 (case first
		   ((#\+ #\-)
		    (or (and (char-numeric? second)
			     (digits 2 #f #f))
			(string=? s "+nan.0")
			(string=? s "-nan.0")
			(string=? s "+inf.0")
			(string=? s "-inf.0")))
		   ((#\.)
		    (and (char-numeric? second)
			 (digits 2 #t #f)))
		   (else #f))))))

    ; Read digits until the end or an `e' or a `.'.  E-OR-DOT? is true if
    ; we have seen either, E? is true if we've seen an `e'.
    (define (digits i e-or-dot? e?)
      (if (= i len)
	  e-or-dot?
	  (let ((next (string-ref s i)))
	    (if (char-numeric? next)
		(digits (+ i 1) e-or-dot? e?)
		(case next
		  ((#\e #\E)
		   (and (not e?)
			(exponent (+ i 1) #f)))
		  ((#\.)
		   (and (not e-or-dot?)
			(digits (+ i 1) #t #f)))
		  (else #f))))))

    ; Read in an exponent.  If SIGN? is true then we have already got the sign.
    (define (exponent i sign?)
      (and (< i len)
	   (let ((next (string-ref s i)))
	     (if (char-numeric? next)
		 (digits (+ i 1) #t #t)
		 (case next
		   ((#\+ #\-)
		    (and (not sign?)
			 (exponent (+ i 1) #t)))
		   (else #f))))))
    (start)))

(define-simple-type <float-string> (<string>) float-string?)

(define-method &really-string->number ((s <float-string>) radix exact?)
  (if (and (= radix 10)
	   (not exact?))
      (string->float s)
      (next-method)))
