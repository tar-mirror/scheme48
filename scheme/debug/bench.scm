; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber

; Tiny benchmarking image.

; This returns the result of calling recursive FIB on its first argument.

(define (start arg in in-encoding out out-encoding error error-encoding)
  (fib (string->integer (vector-ref arg 0))))

(define (string->integer s)
  (letrec ((loop (lambda (i r)
		   (if (= i (string-length s))
		       r
		       (loop (+ i 1)
			     (+ (- (char->ascii (string-ref s i))
				   (char->ascii #\0))
				(* 10 r)))))))
    (loop 0 0)))

(define (fib n)
  (if (< n 2)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))
