; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber

;; The following tests are not very extensive but should cover all
;; operations. See the structure test-bignum in debug-package.scm
;; for a bigger test suite, which, however, requires a full Scheme
;; environment.

(define (start arg in in-encoding out out-encoding error-out error-out-encoding resumer-records)

  (arith-test (- (* 1000 1000 1000 1000)
		 (* 1000 1000 1000 1000))
	      "*, -, external-bignum->long"
	      0
	      out)

  (arith-test (quotient (* 1000 1000 1000 1000)
                        (* 1000 1000 1000))
              "quotient, external-bignum->long"
              1000
              out)
  
  (arith-test (remainder (* 1000 1000 1000 1000)
                         (* 1000 1000 1000))
              "remainder, external-bignum->long"
              0
              out)
  
  (arith-test (/ (* 1000 1000 1000 1000)
                 (* 1000 1000 1000))
              "/, external-bignum->long"
              1000
              out)
  
  (arith-test (* 1000 1000 1000 1000)
              "*,="
              (* 1000 1000 1000 1000)
              out)

  (bool-test (< (* 1000 1000 1000 1000)
                (* 1000 1000 1000 1001))
             "*,<"
             #t
             out)
  
  (bool-test (> (* 1000 1000 1000 1001)
                (* 1000 1000 1000 1000))
             "*,>"
             #t
             out)

  (arith-test (magnitude (* -1000 1000 1000 1000))
              "negate,test"
              (* 1000 1000 1000 1000)
              out)

  (arith-test (bitwise-and (bitwise-ior (arithmetic-shift
                                         (arithmetic-shift 1 130) 10)
                                        #xffffff)
                           #xffffff)
              "bitwise-and, bitwise-ior, arithmetic-shift"
              #xffffff
              out)

  (arith-test (bit-count (bitwise-ior (arithmetic-shift 1 140)
                                      #xff))
              "bit-count, bitwise-ior, arithmetic-shift"
              9
              out)

  ;; tests *, -, external-bignum->long
  (bool-test (= 0 (- (* 1000 1000 1000 1000)
                     (* 1000 1000 1000 1000)))
             "(= 0 (- (* 1000 1000 1000 1000)
                     (* 1000 1000 1000 1000)))"
             #t
             out)

  ;; tests quotient, external-bignum->long
  (bool-test (= 1000 (quotient (* 1000 1000 1000 1000)
                               (* 1000 1000 1000)))
             "(= 1000 (quotient (* 1000 1000 1000 1000)
                    (* 1000 1000 1000)))"
             #t
             out)


  ;; tests remainder, external-bignum->long
  (bool-test (= 0 (remainder (* 1000 1000 1000 1000)
                             (* 1000 1000 1000)))
             "(= 0 (remainder (* 1000 1000 1000 1000)
                   (* 1000 1000 1000)))"
             #t
             out)
  
  ;; tests /, external-bignum->long
  (bool-test (= 1000 (/ (* 1000 1000 1000 1000)
                        (* 1000 1000 1000)))
             "(= 1000 (/ (* 1000 1000 1000 1000)
              (* 1000 1000 1000)))"
             #t
             out)

  ;; tests *,=
  (bool-test (= (* 1000 1000 1000 1000)
                (* 1000 1000 1000 1000))
             "(= (* 1000 1000 1000 1000)
      (* 1000 1000 1000 1000))"
             #t
             out)

  ;; tests *, compare
  (bool-test (< (* 1000 1000 1000 1000)
                (* 1000 1000 1000 1001))
             "(< (* 1000 1000 1000 1000)
      (* 1000 1000 1000 1001))"
             #t
             out)
  
  ;; tests *, compare
  (bool-test (> (* 1000 1000 1000 1001)
                (* 1000 1000 1000 1000))
             "(> (* 1000 1000 1000 1001)
      (* 1000 1000 1000 1000))"
             #t
             out)

  ;; tests negate, test 
  (bool-test (=  (magnitude (* -1000 1000 1000 1000))
                 (* 1000 1000 1000 1000))
             "(=  (magnitude (* -1000 1000 1000 1000))
       (* 1000 1000 1000 1000))"
             #t
             out)

  ;; test bitwise-and, bitwise-ior, arithmetic-shift
  (bool-test (= (bitwise-and (bitwise-ior (arithmetic-shift
                                           (arithmetic-shift 1 130) 10)
                                          #xffffff)
                             #xffffff)
                #xffffff)
             "(= (bitwise-and (bitwise-ior (arithmetic-shift
                                 (arithmetic-shift 1 130) 10)
                                #xffffff)
                   #xffffff)
      #xffffff)"
             #t
             out)

  ;; test bit-count, bitwise-ior, arithmetic-shift
  (bool-test (= (bit-count (bitwise-ior (arithmetic-shift 1 140)
					#xff))
		9)
             "(= (bit-count (bitwise-ior (arithmetic-shift 1 140)
                              #xff))
      9)"
             #t
             out))

(define numbers
  '#("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
     "10" "11" "12" "13" "14" "15" "16" "17" "18" "19"
     "20" "21" "22" "23" "24" "25" "26" "27" "28" "29"))

(define (number->string n)
  (vector-ref numbers n))

(define (arith-test n s a out)
  (if (= n a)
      (begin
	(write-string "Success: " out)
	(write-string s out)
	(newline out))
      (begin
	(write-string "Failure: " out)
	(write-string s out)
	(write-string " = " out)
	(write-string (number->string n) out)
	(write-string " and not " out)
	(write-string (number->string a) out)
	(newline out))))

(define (bool-test n s a out)
  (if (eq? n a)
      (begin
	(write-string "Success: " out)
	(write-string s out)
	(newline out))
      (begin
	(write-string "Failure: " out)
	(write-string s out)
	(write-string " => " out)
	(write-string (if n "#t" "#f") out)
	(newline out))))

(define (write-string string . channel-option) ; test n-ary procedures
  (write-byte-vector (string->byte-vector string)
		     (car channel-option)))

(define (write-byte-vector bytes channel)
  (channel-maybe-write channel
		       bytes
		       0
		       (byte-vector-length bytes)))

(define (string->byte-vector string)
  ((lambda (size)
     ((lambda (bytes)
	(letrec ((loop
		  (lambda (i)
		    (if (< i size)
			(begin
			  (byte-vector-set! bytes i
					    (char->scalar-value (string-ref string i)))
			  (loop (+ 1 i)))))))
	  (loop 0)
	  bytes))
      (make-byte-vector size 0)))
   (string-length string)))

(define (newline channel)
    (write-string "
" channel))
