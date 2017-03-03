; Adapted from the R6RS reference implementation, which is:

; Copyright 2007 William D Clinger.

; Permission to copy this software, in whole or in part, to use this
; software for any lawful purpose, and to redistribute this software
; is granted subject to the restriction that all copies made of this
; software must include this copyright notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.

(define *random-stress-tests* 100)            ; number of tests
(define *random-stress-test-max-size* 50)     ; twice average size of string

  
(define (test-roundtrip bvec tostring tobvec)
  (let* ((s1 (tostring bvec))
	 (b2 (tobvec s1))
	 (s2 (tostring b2)))
    (check (string=? s1 s2) => #t)))

(define random
  (letrec ((random14
	    (lambda (n)
	      (set! x (remainder (+ (* a x) c) (+ m 1)))
	      (remainder (quotient x 8) n)))
	   (a 701)
	   (x 1)
	   (c 743483)
	   (m 524287)
	   (loop
	    (lambda (q r n)
	      (if (zero? q)
		  (remainder r n)
		  (loop (quotient q 16384)
			(+ (* 16384 r) (random14 16384))
			n)))))
    (lambda (n)
      (if (< n 16384)
	  (random14 n)
	  (loop (quotient n 16384) (random14 16384) n)))))
 
  
; Returns a random bytevector of length up to n.

  
(define (random-bytevector n)
  (let* ((n (random n))
	 (bv (make-bytevector n)))
    (do ((i 0 (+ i 1)))
	((= i n) bv)
      (bytevector-u8-set! bv i (random 256)))))

  ; Returns a random bytevector of even length up to n.

 
(define (random-bytevector2 n)
  (let* ((n (random n))
	 (n (if (odd? n) (+ n 1) n))
	 (bv (make-bytevector n)))
    (do ((i 0 (+ i 1)))
	((= i n) bv)
      (bytevector-u8-set! bv i (random 256)))))

  ; Returns a random bytevector of multiple-of-4 length up to n.

 
(define (random-bytevector4 n)
  (let* ((n (random n))
	 (n (* 4 (round (/ n 4))))
	 (bv (make-bytevector n)))
    (do ((i 0 (+ i 1)))
	((= i n) bv)
      (bytevector-u8-set! bv i (random 256)))))

  
(define (test-char-range lo hi tostring tobytevector)
  (let* ((n (+ 1 (- hi lo)))
	 (s (make-string n))
	 (replacement-character (integer->char #xfffd)))
    (do ((i lo (+ i 1)))
	((> i hi))
      (let ((c (if (or (<= 0 i #xd7ff)
		       (<= #xe000 i #x10ffff))
		   (integer->char i)
		   replacement-character)))
	(string-set! s (- i lo) c)))
    (check (string=? (tostring (tobytevector s)) s) => #t)))
  
(define (test-exhaustively name tostring tobytevector)
  (test-char-range 0 #xffff tostring tobytevector)
  (test-char-range #x10000 #x1ffff tostring tobytevector)
  (test-char-range #x20000 #x2ffff tostring tobytevector)
  (test-char-range #x30000 #x3ffff tostring tobytevector)
  (test-char-range #x40000 #x4ffff tostring tobytevector)
  (test-char-range #x50000 #x5ffff tostring tobytevector)
  (test-char-range #x60000 #x6ffff tostring tobytevector)
  (test-char-range #x70000 #x7ffff tostring tobytevector)
  (test-char-range #x80000 #x8ffff tostring tobytevector)
  (test-char-range #x90000 #x9ffff tostring tobytevector)
  (test-char-range #xa0000 #xaffff tostring tobytevector)
  (test-char-range #xb0000 #xbffff tostring tobytevector)
  (test-char-range #xc0000 #xcffff tostring tobytevector)
  (test-char-range #xd0000 #xdffff tostring tobytevector)
  (test-char-range #xe0000 #xeffff tostring tobytevector)
  (test-char-range #xf0000 #xfffff tostring tobytevector)
  (test-char-range #x100000 #x10ffff tostring tobytevector))
  
; Feel free to replace this with your favorite timing macro.
  
(define-test-case utf-8 string-bytevectors-tests
  
  (check-that (string->utf8 "k\x007f;\x0080;\x07ff;\x0800;\xffff;")
	      (is bytevector=?
		  (u8-list->bytevector '(#x6b
					 #x7f
					 #b11000010 #b10000000
					 #b11011111 #b10111111
					 #b11100000 #b10100000 #b10000000
					 #b11101111 #b10111111 #b10111111))))

  (check-that (u8-list->bytevector '(#b11110000 #b10010000 #b10000000 #b10000000
							 #b11110100 #b10001111 #b10111111 #b10111111))
	      (is bytevector=?
		  (string->utf8 "\x010000;\x10ffff;")))

  (check (utf8->string (u8-list->bytevector '(#x61 ; a
					      #xc0 #x62 ; ?b
					      #xc1 #x63 ; ?c
					      #xc2 #x64 ; ?d
					      #x80 #x65 ; ?e
					      #xc0 #xc0 #x66 ; ??f
					      #xe0 #x67 ; ?g
					      )))
	 => "a\xfffd;b\xfffd;c\xfffd;d\xfffd;e\xfffd;\xfffd;f\xfffd;g")

  (check (utf8->string (u8-list->bytevector '(#x20 #x68 ; ???h
						   #xe0 #xc0 #x20 #x69 ; ???i
						   #xf0 #x6a ; ?j
						   )))
	 => " h\xfffd;\xfffd; i\xfffd;j")

  (check (utf8->string (u8-list->bytevector '(#x61 ; a
							#x20 #x20 #x20 #x62 ; ????b
							#xf0 #xFF #x63 ; .c
							)))
	 => "a\x20;\x20;\x20;b\xfffd;\xfffd;c")

  (check (utf8->string (u8-list->bytevector '(#x61 ; a
							#x80 #xc6 #x64 ; ??d
							#x80 #xc6 #x65 ; ??e
							#x80 #xc4 #x66 ; ??f
							)))
	 => "a\xfffd;\xfffd;d\xfffd;\xfffd;e\xfffd;\xfffd;f")
    
  (check (utf8->string (u8-list->bytevector '(#x61 ; a
					      #xc6 #x80 #x64 ; .d
					      #xc6 #x80 #x65 ; ?e
					      #xc4 #x80 #x66 ; ?f
					      )))
	 => "a\x00180;d\x00180;e\x00100;f")

  (check (utf8->string (u8-list->bytevector '(#x61 ; a
					      #xf4 #x8f #xbf #xbf #x62 ; .b
					      #xf4 #x90 #x80 #x80 #x63 ; ????c
					      )))
	 => "a\x10ffff;b\xfffd;\xfffd;\xfffd;\xfffd;c")

  (check (utf8->string (u8-list->bytevector '(#x61 ; a
					      #xf5 #x80 #x80 #x80 #x64 ; ????d
					      )))
	 => "a\xfffd;\xfffd;\xfffd;\xfffd;d")

  ;; ignores BOM signature
  
  (check (utf8->string (u8-list->bytevector '(#xef #xbb #xbf #x61 #x62 #x63 #x64)))
	 => "abcd")

  (test-roundtrip (random-bytevector 10) utf8->string string->utf8)

  (do ((i 0 (+ i 1)))
      ((= i *random-stress-tests*))
    (test-roundtrip (random-bytevector *random-stress-test-max-size*)
                    utf8->string string->utf8)))

(define-test-case utf-16 string-bytevectors-tests

  (check-that (string->utf16 "k\x007f;\x0080;\x07ff;\x0800;\xffff;")
	      (is bytevector=?
		  (u8-list->bytevector '(#x00 #x6b
					      #x00 #x7f
					      #x00 #x80
					      #x07 #xff
					      #x08 #x00
					      #xff #xff))))

  (check-that (string->utf16 "k\x007f;\x0080;\x07ff;\x0800;\xffff;"
			     (endianness little))
	      (is bytevector=?
		  (u8-list->bytevector '(#x6b #x00
					      #x7f #x00
					      #x80 #x00
					      #xff #x07
					      #x00 #x08
					      #xff #xff))))

  (check-that (string->utf16 "\x010000;\xfdcba;\x10ffff;")
	      (is bytevector=?
		  (u8-list->bytevector '(#xd8 #x00 #xdc #x00
					      #xdb #xb7 #xdc #xba
					      #xdb #xff #xdf #xff))))

  (check-that (string->utf16 "\x010000;\xfdcba;\x10ffff;" (endianness little))
	      (is bytevector=?
		  (u8-list->bytevector '(#x00 #xd8 #x00 #xdc
					      #xb7 #xdb #xba #xdc
					      #xff #xdb #xff #xdf))))

  (check-that (string->utf16 "ab\x010000;\xfdcba;\x10ffff;cd")
	      (is bytevector=?
		  (string->utf16 "ab\x010000;\xfdcba;\x10ffff;cd" (endianness big))))

  (check (utf16->string
	  (u8-list->bytevector '(#x00 #x6b
				      #x00 #x7f
				      #x00 #x80
				      #x07 #xff
				      #x08 #x00
				      #xff #xff))
	  (endianness big))
	 => "k\x007f;\x0080;\x07ff;\x0800;\xffff;")

  (check (utf16->string
	  (u8-list->bytevector '(#x00 #x6b
				      #x00 #x7f
				      #x00 #x80
				      #x07 #xff
				      #x08 #x00
				      #xff #xff))
	  (endianness big))
	 => "k\x007f;\x0080;\x07ff;\x0800;\xffff;")

  (check (utf16->string
	  (u8-list->bytevector '(#xfe #xff ; big-endian BOM
				      #x00 #x6b
				      #x00 #x7f
				      #x00 #x80
				      #x07 #xff
				      #x08 #x00
				      #xff #xff))
	  (endianness big))
	 => "k\x007f;\x0080;\x07ff;\x0800;\xffff;")

  (check (utf16->string
	  (u8-list->bytevector  '(#x6b #x00
				       #x7f #x00
				       #x80 #x00
				       #xff #x07
				       #x00 #x08
				       #xff #xff))
	  (endianness little))
	 => "k\x007f;\x0080;\x07ff;\x0800;\xffff;")

  (check (utf16->string
	  (u8-list->bytevector '(#xff #xfe ; little-endian BOM
				      #x6b #x00
				      #x7f #x00
				      #x80 #x00
				      #xff #x07
				      #x00 #x08
				      #xff #xff))
	  (endianness big))
	 => "k\x007f;\x0080;\x07ff;\x0800;\xffff;")

  (let ((tostring        (lambda (bv) (utf16->string bv (endianness big))))
        (tostring-big    (lambda (bv) (utf16->string bv (endianness big))))
        (tostring-little (lambda (bv) (utf16->string bv (endianness little))))
        (tobvec          string->utf16)
        (tobvec-big      (lambda (s) (string->utf16 s (endianness big))))
        (tobvec-little   (lambda (s) (string->utf16 s (endianness little)))))

    (do ((i 0 (+ i 1)))
        ((= i *random-stress-tests*))
      (test-roundtrip (random-bytevector2 *random-stress-test-max-size*)
                      tostring tobvec)
      (test-roundtrip (random-bytevector2 *random-stress-test-max-size*)
                      tostring-big tobvec-big)
      (test-roundtrip (random-bytevector2 *random-stress-test-max-size*)
                      tostring-little tobvec-little))))

(define-test-case utf-32 string-bytevectors-tests

  (check-that (string->utf32 "abc")
	      (is bytevector=?
		  (u8-list->bytevector '(#x00 #x00 #x00 #x61
					      #x00 #x00 #x00 #x62
					      #x00 #x00 #x00 #x63))))

  (check-that (string->utf32 "abc" (endianness big))
	      (is bytevector=?
		  (u8-list->bytevector '(#x00 #x00 #x00 #x61
					      #x00 #x00 #x00 #x62
					      #x00 #x00 #x00 #x63))))

  (check-that (string->utf32 "abc" (endianness little))
	      (is bytevector=?
		  (u8-list->bytevector '(#x61 #x00 #x00 #x00
					      #x62 #x00 #x00 #x00
					      #x63 #x00 #x00 #x00))))

  (check (string-contains ;;"a\xfffd;\xfffd;\xfffd;\xfffd;b\xfffd;\xfffd;\xfffd;c\xfffd;d\xfffd;\xfffd;\xfffd;\xfffd;e"
	  (utf32->string
	   (u8-list->bytevector '(#x00 #x00 #x00 #x61
				       #x00 #x00 #xd9 #x00
				       #x00 #x00 #x00 #x62
				       #x00 #x00 #xdd #xab
				       #x00 #x00 #x00 #x63
				       #x00 #x11 #x00 #x00
				       #x00 #x00 #x00 #x64
				       #x01 #x00 #x00 #x65
				       #x00 #x00 #x00 #x65))
	   (endianness big))
	  "a\xfffd;"))
       

  (check (string-contains ;;"a\xfffd;b\xfffd;c\xfffd;d\xfffd;e"
	  (utf32->string
	   (u8-list->bytevector '(#x00 #x00 #x00 #x61
				       #x00 #x00 #xd9 #x00
				       #x00 #x00 #x00 #x62
				       #x00 #x00 #xdd #xab
				       #x00 #x00 #x00 #x63
				       #x00 #x11 #x00 #x00
				       #x00 #x00 #x00 #x64
				       #x01 #x00 #x00 #x65
				       #x00 #x00 #x00 #x65))
	   (endianness big))
	  "a\xfffd;"))
        

  (check (string-contains ;;"a\xfffd;b\xfffd;c\xfffd;d\xfffd;e"
	  (utf32->string
	   (u8-list->bytevector '(#x00 #x00 #xfe #xff ; big-endian BOM
				       #x00 #x00 #x00 #x61
				       #x00 #x00 #xd9 #x00
				       #x00 #x00 #x00 #x62
				       #x00 #x00 #xdd #xab
				       #x00 #x00 #x00 #x63
				       #x00 #x11 #x00 #x00
				       #x00 #x00 #x00 #x64
				       #x01 #x00 #x00 #x65
				       #x00 #x00 #x00 #x65))
	   (endianness big))
	  "a\xfffd;"))
       

  (check (string-contains ;;"\xfeff;a\xfffd;b\xfffd;c\xfffd;d\xfffd;e"
	  (utf32->string
	   (u8-list->bytevector '(#x00 #x00 #xfe #xff ; big-endian BOM
				       #x00 #x00 #x00 #x61
				       #x00 #x00 #xd9 #x00
				       #x00 #x00 #x00 #x62
				       #x00 #x00 #xdd #xab
				       #x00 #x00 #x00 #x63
				       #x00 #x11 #x00 #x00
				       #x00 #x00 #x00 #x64
				       #x01 #x00 #x00 #x65
				       #x00 #x00 #x00 #x65))
	   (endianness big))
	  "\xfffd;"))
       

  (check (string-contains ;;"a\xfffd;b\xfffd;c\xfffd;d\xfffd;e"
	  (utf32->string
	   (u8-list->bytevector '(#x61 #x00 #x00 #x00
				       #x00 #xd9 #x00 #x00
				       #x62 #x00 #x00 #x00
				       #xab #xdd #x00 #x00
				       #x63 #x00 #x00 #x00
				       #x00 #x00 #x11 #x00
				       #x64 #x00 #x00 #x00
				       #x65 #x00 #x00 #x01
				       #x65 #x00 #x00 #x00))
	   (endianness little))
	  "a\xfffd;"))
       

  (check (string-contains ;;"a\xfffd;b\xfffd;c\xfffd;d\xfffd;e"
	  (utf32->string
	   (u8-list->bytevector '(#xff #xfe #x00 #x00 ; little-endian BOM
				       #x61 #x00 #x00 #x00
				       #x00 #xd9 #x00 #x00
				       #x62 #x00 #x00 #x00
				       #xab #xdd #x00 #x00
				       #x63 #x00 #x00 #x00
				       #x00 #x00 #x11 #x00
				       #x64 #x00 #x00 #x00
				       #x65 #x00 #x00 #x01
				       #x65 #x00 #x00 #x00))
	   (endianness big))
	  "a\xfffd;"))
       

  (check (string-contains ;;"a\xfffd;b\xfffd;c\xfffd;d\xfffd;e"
	  (utf32->string
	   (u8-list->bytevector '(#xff #xfe #x00 #x00 ; little-endian BOM
				       #x61 #x00 #x00 #x00
				       #x00 #xd9 #x00 #x00
				       #x62 #x00 #x00 #x00
				       #xab #xdd #x00 #x00
				       #x63 #x00 #x00 #x00
				       #x00 #x00 #x11 #x00
				       #x64 #x00 #x00 #x00
				       #x65 #x00 #x00 #x01
				       #x65 #x00 #x00 #x00))
	   (endianness little)) 
	  "a\xfffd;"))
        
  
  (let ((tostring        (lambda (bv) (utf32->string bv (endianness big))))
        (tostring-big    (lambda (bv) (utf32->string bv (endianness big))))
        (tostring-little (lambda (bv) (utf32->string bv (endianness little))))
        (tobvec          string->utf32)
        (tobvec-big      (lambda (s) (string->utf32 s (endianness big))))
        (tobvec-little   (lambda (s) (string->utf32 s (endianness little)))))

    (do ((i 0 (+ i 1)))
        ((= i *random-stress-tests*))
      (test-roundtrip (random-bytevector4 *random-stress-test-max-size*)
                      tostring tobvec)
      (test-roundtrip (random-bytevector4 *random-stress-test-max-size*)
                      tostring-big tobvec-big)
      (test-roundtrip (random-bytevector4 *random-stress-test-max-size*)
                      tostring-little tobvec-little))))

  

; Tests string <-> bytevector conversion on strings
; that contain every Unicode scalar value.

(define-test-case exhaustive-string-bytevector-tests string-bytevectors-tests
 
  ;; Tests throughout an inclusive range.

  (test-exhaustively "UTF-16BE"
		     (lambda (bv) (utf16->string bv (endianness big)))
		     (lambda (s) (string->utf16 s (endianness big))))

  (test-exhaustively "UTF-16LE"
		     (lambda (bv) (utf16->string bv (endianness little)))
		     (lambda (s) (string->utf16 s (endianness little))))

  (test-exhaustively "UTF-32" 
		     (lambda (bv) (utf32->string bv (endianness big)))
		     string->utf32)

  (test-exhaustively "UTF-32BE"
		     (lambda (bv) (utf32->string bv (endianness big)))
		     (lambda (s) (string->utf32 s (endianness big))))

  (test-exhaustively "UTF-32LE"
		     (lambda (bv) (utf32->string bv (endianness little)))
		     (lambda (s) (string->utf32 s (endianness little)))))



