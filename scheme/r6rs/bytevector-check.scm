; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Harald Glab-Plhak, Mike Sperber

; These are the examples from the R6RS document.

(define-test-suite basic-bytevectors-tests)
(define-test-suite ieee-bytevectors-tests)
(define-test-suite string-bytevectors-tests)
(define-test-suite r6rs-bytevectors-tests
  (basic-bytevectors-tests ieee-bytevectors-tests string-bytevectors-tests))

(define-test-case endian/simple-bytevect basic-bytevectors-tests
  (check (endianness big) => (endianness big))
  (check (endianness little) => (endianness little))

  (check (or (eq? (native-endianness) (endianness big))
	     (eq? (native-endianness) (endianness little))) => #t)

  (check (bytevector? (vector)) => #f)
  (check (bytevector? (make-bytevector 3)) => #t)

  (check (bytevector-length (make-bytevector 44)) => 44)

  (check (let ((b1 (make-bytevector 16 -127))
	       (b2 (make-bytevector 16 255)))
	   (list
	    (bytevector-s8-ref b1 0)
	    (bytevector-u8-ref b1 0)
	    (bytevector-s8-ref b2 0)
	    (bytevector-u8-ref b2 0))) => '(-127 129 -1 255))

  (check (let ((b (make-bytevector 16 -127)))
	   (bytevector-s8-set! b 0 -126)
	   (bytevector-u8-set! b 1 246)
	   (list
	    (bytevector-s8-ref b 0)
	    (bytevector-u8-ref b 0)
	    (bytevector-s8-ref b 1)
	    (bytevector-u8-ref b 1))) => '(-126 130 -10 246))

  (let ((b (make-bytevector 16 -127)))
    (bytevector-uint-set! b 0 (- (expt 2 128) 3) (endianness little) 16)

    (check (bytevector-uint-ref b 0 (endianness little) 16)
	   => #xfffffffffffffffffffffffffffffffd)

    (check (bytevector-sint-ref b 0 (endianness little) 16) => -3)

    (check (bytevector->u8-list b)
	   => '(253 255 255 255 255 255 255 255
		    255 255 255 255 255 255 255 255))

    (bytevector-uint-set! b 0 (- (expt 2 128) 3) (endianness big) 16)

    (check (bytevector-uint-ref b 0 (endianness big) 16)
	   => #xfffffffffffffffffffffffffffffffd)

    (check (bytevector-sint-ref b 0 (endianness big) 16) => -3)

    (check (bytevector->u8-list b)
	   => '(255 255 255 255 255 255 255 255
		    255 255 255 255 255 255 255 253))))

(define-test-case int16/int32/int64/to-list-revert  basic-bytevectors-tests
  
  (let ((b (u8-list->bytevector
            '(255 255 255 255 255 255 255 255
		  255 255 255 255 255 255 255 253))))

    (check (bytevector-u16-ref b 14 (endianness little)) => 65023)

    (check (bytevector-s16-ref b 14 (endianness little)) => -513)

    (check (bytevector-u16-ref b 14 (endianness big)) => 65533)

    (check (bytevector-s16-ref b 14 (endianness big)) => -3)

    (bytevector-u16-set! b 0 12345 (endianness little))

    (check (bytevector-u16-ref b 0 (endianness little)) => 12345)

    (bytevector-u16-native-set! b 0 12345)

    (check (bytevector-u16-native-ref b 0) => 12345))

         
  (let ((b  (u8-list->bytevector
	     '(255 255 255 255 255 255 255 255
		   255 255 255 255 255 255 255 253))))

    (check (bytevector-u32-ref b 12 (endianness little)) => 4261412863)

    (check (bytevector-s32-ref b 12 (endianness little)) => -33554433)

    (check (bytevector-u32-ref b 12 (endianness big)) => 4294967293)

    (check (bytevector-s32-ref b 12 (endianness big)) => -3))

       
  (let ((b (u8-list->bytevector
            '(255 255 255 255 255 255 255 255
		  255 255 255 255 255 255 255 253))))

    (check (bytevector-u64-ref b 8 (endianness little))
	   => 18302628885633695743)

    (check (bytevector-s64-ref b 8 (endianness little))
	   => -144115188075855873)

    (check (bytevector-u64-ref b 8 (endianness big))
	   => 18446744073709551613)

    (check (bytevector-s64-ref b 8 (endianness big)) => -3))

       
  (let (( b1 (u8-list->bytevector '(255 2 254 3 255)))
	(b2 (u8-list->bytevector '(255 3 254 2 255)))
	(b3 (u8-list->bytevector '(255 3 254 2 255)))
	(b4 (u8-list->bytevector '(255 3 255))))
    (check (bytevector=? b1 b2) => #f)
    (check (bytevector=? b2 b3) => #t)
    (check (bytevector=? b3 b4) => #f)
    (check (bytevector=? b4 b3) => #f)))

(define-test-case ieee-vect/fill/copy  basic-bytevectors-tests
  (let ((b (u8-list->bytevector
            '(63 240 0 0 0 0 0 0))))

    (check (bytevector-ieee-single-ref b 4 (endianness little)) => 0.0)

    (check (bytevector-ieee-double-ref b 0 (endianness big)) => 1.0)

    (bytevector-ieee-single-native-set! b 4 3.0)

    (check (bytevector-ieee-single-native-ref b 4) => 3.0)

    (bytevector-ieee-double-native-set! b 0 5.0)

    (check (bytevector-ieee-double-native-ref b 0) => 5.0)

    (bytevector-ieee-double-set! b 0 1.75 (endianness big))

    (check (bytevector->u8-list b) => '(63 252 0 0 0 0 0 0)))

  (let ((b (make-bytevector 7 12)))
    (bytevector-fill! b 127)
    (check (bytevector->u8-list b) => '(127 127 127 127 127 127 127)))

  (let ((b (u8-list->bytevector '(1 2 3 4 5 6 7 8))))
    (bytevector-copy! b 0 b 3 4)
    (check (bytevector->u8-list b) => '(1 2 3 1 2 3 4 8))
    (check (bytevector=? b (bytevector-copy b)) => #t))

  (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
    (check (bytevector->sint-list b (endianness little) 2)
	   => '(513 -253 513 513))
    (check (bytevector->uint-list b (endianness little) 2)
	   => '(513 65283 513 513))))


