; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Mike Sperber, Marcus Crestani

(define-local-syntax (define-c-arith-binop-generator id c-op)
  `(define-c-generator ,id #t
     (lambda (call port indent)
       (simple-c-primop ,c-op call port))))

(define-c-arith-binop-generator + "+")
(define-c-arith-binop-generator - "-")
(define-c-arith-binop-generator * "*")
(define-c-arith-binop-generator quotient  "/")

(define-c-arith-binop-generator un+ "+")
(define-c-arith-binop-generator un- "-")
(define-c-arith-binop-generator un* "*")
(define-c-arith-binop-generator unquotient  "/")

(define-c-arith-binop-generator fl+ "+")
(define-c-arith-binop-generator fl- "-")
(define-c-arith-binop-generator fl* "*")
(define-c-arith-binop-generator fl/ "/")

(define-c-generator small* #t
  (lambda (call port indent)
    (format port "PS_SMALL_MULTIPLY(")
    (c-value (call-arg call 0) port)
    (format port ", ")
    (c-value (call-arg call 1) port)
    (format port ")")))

(define-c-arith-binop-generator remainder "%")
(define-c-arith-binop-generator unremainder "%")
(define-c-arith-binop-generator bitwise-and "&")
(define-c-arith-binop-generator bitwise-ior "|")
(define-c-arith-binop-generator bitwise-xor "^")

(define-c-generator ashl #t
  (lambda (call port indent)
    (generate-shift call port indent "LEFT" #f)))

(define-c-generator ashr #t
  (lambda (call port indent)
    (generate-shift call port indent "RIGHT" #f)))

(define-c-generator lshr #t
  (lambda (call port indent)
    (generate-shift call port indent "RIGHT_LOGICAL" #t)))

(define (generate-shift call port indent macro logical?)
  (cond ((= 1 (call-exits call))
	 ; PS_SHIFT_??? is a C macro that handles overshifting even if C doesn't
	 (indent-to port indent)
	 (format port "PS_SHIFT_~A(" macro)
	 (c-value (call-arg call 1) port)
	 (format port ", ")
	 (c-value (call-arg call 2) port)
	 (format port ", ")
	 (c-variable (car (lambda-variables (call-arg call 0))) port)
	 (format port ")"))
	((and (literal-node? (call-arg call 1))
	      (>= (literal-value (call-arg call 1)) pre-scheme-integer-size))
	 (format port "0L"))
	(else
	 (format port "PS_SHIFT_~A_INLINE(" macro)
	 (c-value (call-arg call 0) port)
	 (format port ", ")
	 (c-value (call-arg call 1) port)
	 (format port ")"))))
    
(define-c-generator bitwise-not #t
  (lambda (call port indent)
    (simple-c-primop "~" call port)))

(define-local-syntax (define-c-comp-binop-generator id c-op)
  `(define-c-generator ,id #t
     (lambda (call port indent)
       (simple-c-primop ,c-op call port))))

(define-c-comp-binop-generator =      "==")
(define-c-comp-binop-generator <      "<" )
(define-c-comp-binop-generator fl=    "==")
(define-c-comp-binop-generator fl<    "<" )
(define-c-comp-binop-generator un=    "==")
(define-c-comp-binop-generator un<    "<" )
(define-c-comp-binop-generator char=? "==")
(define-c-comp-binop-generator char<? "<" )

(define-c-generator ascii->char #t
  (lambda (call port indent)
    (display "((char) " port)
    (c-value (call-arg call 0) port)
    (display ")" port)))
    
(define-c-generator char->ascii #t
  (lambda (call port indent)
    (display "((unsigned char) " port)
    (c-value (call-arg call 0) port)
    (display ")" port)))

(define-c-generator unsigned->integer #t
  (lambda (call port indent)
    (display "((long) " port)
    (c-value (call-arg call 0) port)
    (display ")" port)))

(define-c-generator integer->unsigned #t
  (lambda (call port indent)
    (display "((unsigned long) " port)
    (c-value (call-arg call 0) port)
    (display ")" port)))
    
;(define-c-generator sign-extend #t
;  (lambda (call port indent)
;    (display "((long) " port)
;    (c-value (call-arg call 0) port)
;    (display ")" port)))
;
;(define-c-generator zero-extend #t
;  (lambda (call port indent)
;    (display "((unsigned long) " port)
;    (c-value (call-arg call 0) port)
;    (display ")" port)))
