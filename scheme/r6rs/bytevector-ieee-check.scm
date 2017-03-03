; Adapted from the R6RS reference implementation by Harald Glab-Phlak, which is:

; Copyright 2007 William D Clinger.

; Permission to copy this software, in whole or in part, to use this
; software for any lawful purpose, and to redistribute this software
; is granted subject to the restriction that all copies made of this
; software must include this copyright notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.

(define-test-case ieee/heavy-test ieee-bytevectors-tests
  (letrec ((roundtrip 
	    (lambda (x getter setter! k endness)
	      (let ((b (make-bytevector 400)))
		(setter! b k x endness)
		(check (getter b k endness) => x)))))
    (roundtrip
     +inf.0
     bytevector-ieee-single-ref bytevector-ieee-single-set! 0 (endianness big))
   
    (roundtrip
     -inf.0
     bytevector-ieee-single-ref bytevector-ieee-single-set! 0 (endianness big))

   
    (roundtrip
     1e10
     bytevector-ieee-single-ref bytevector-ieee-single-set! 0 (endianness little))
 
    (roundtrip
     -0.2822580337524414
     bytevector-ieee-single-ref bytevector-ieee-single-set! 1 (endianness big))

    (roundtrip
     1e10
     bytevector-ieee-single-ref bytevector-ieee-single-set! 0 (endianness big))

    (roundtrip
     +inf.0
     bytevector-ieee-single-ref bytevector-ieee-single-set! 1 (endianness big))
    (roundtrip
     -inf.0
     bytevector-ieee-single-ref bytevector-ieee-single-set! 1 (endianness big))
       
    (roundtrip
     1e10
     bytevector-ieee-single-ref bytevector-ieee-single-set! 1 (endianness big))
        
       
    (roundtrip
     -0.2822580337524414
     bytevector-ieee-single-ref bytevector-ieee-single-set! 1 (endianness big))

	
    (roundtrip
     +inf.0
     bytevector-ieee-single-ref bytevector-ieee-single-set! 1 (endianness little))

    (roundtrip
     -inf.0
     bytevector-ieee-single-ref bytevector-ieee-single-set! 1 (endianness little))
       
    (roundtrip
     1e10
     bytevector-ieee-single-ref bytevector-ieee-single-set! 1 (endianness little))
       
    (roundtrip
     -0.2822580337524414
     bytevector-ieee-single-ref bytevector-ieee-single-set! 1 (endianness little))
      
    (roundtrip
     +inf.0
     bytevector-ieee-single-ref bytevector-ieee-single-set! 2 (endianness big))

    (roundtrip
     -inf.0
     bytevector-ieee-single-ref bytevector-ieee-single-set! 2 (endianness big))

    (roundtrip
     1e10
     bytevector-ieee-single-ref bytevector-ieee-single-set! 2 (endianness big))
    (roundtrip
     -0.2822580337524414
     bytevector-ieee-single-ref bytevector-ieee-single-set! 2 (endianness big))
    (roundtrip
     +inf.0
     bytevector-ieee-single-ref bytevector-ieee-single-set! 2 (endianness little))
    (roundtrip
     -inf.0
     bytevector-ieee-single-ref bytevector-ieee-single-set! 2 (endianness little))
    (roundtrip
     1e10
     bytevector-ieee-single-ref bytevector-ieee-single-set! 2 (endianness little))

    ;; Double precision, offset 0, big-endian
    (roundtrip
     +inf.0
     bytevector-ieee-double-ref bytevector-ieee-double-set! 0 (endianness big))

 
    (roundtrip
     -inf.0
     bytevector-ieee-double-ref bytevector-ieee-double-set! 0 (endianness big))

  
    (roundtrip
     1e10
     bytevector-ieee-double-ref bytevector-ieee-double-set! 0 (endianness big))

  
    (roundtrip
     -0.2822580337524414
     bytevector-ieee-double-ref bytevector-ieee-double-set! 0 (endianness big))

    ;; Double precision, offset 0, little-endian
    (roundtrip
     +inf.0
     bytevector-ieee-double-ref bytevector-ieee-double-set! 0 (endianness little))

  
    (roundtrip
     -inf.0
     bytevector-ieee-double-ref bytevector-ieee-double-set! 0 (endianness little))


 
    (roundtrip
     1e10
     bytevector-ieee-double-ref bytevector-ieee-double-set! 0 (endianness little))

  
    (roundtrip
     -0.2822580337524414
     bytevector-ieee-double-ref bytevector-ieee-double-set! 0 (endianness little))

    ;; Double precision, offset 1, big-endian
    (roundtrip
     +inf.0
     bytevector-ieee-double-ref bytevector-ieee-double-set! 1 (endianness big))

    (roundtrip
     -inf.0
     bytevector-ieee-double-ref bytevector-ieee-double-set! 1 (endianness big))
  
    (roundtrip
     1e10
     bytevector-ieee-double-ref bytevector-ieee-double-set! 1 (endianness big))
  
    (roundtrip
     -0.2822580337524414
     bytevector-ieee-double-ref bytevector-ieee-double-set! 1 (endianness big))

    ;; Double precision, offset 1, little-endian

    (roundtrip
     +inf.0
     bytevector-ieee-double-ref bytevector-ieee-double-set! 1 (endianness little))
     
    (roundtrip
     -inf.0
     bytevector-ieee-double-ref bytevector-ieee-double-set! 1 (endianness little))
 
    (roundtrip
     -0.2822580337524414
     bytevector-ieee-double-ref bytevector-ieee-double-set! 2 (endianness big))

    ;; Double precision, offset 2, little-endian
  
    (roundtrip
     +inf.0
     bytevector-ieee-double-ref bytevector-ieee-double-set! 2 (endianness little))
 
    (roundtrip
     -inf.0
     bytevector-ieee-double-ref bytevector-ieee-double-set! 2 (endianness little))
 
    (roundtrip
     1e10
     bytevector-ieee-double-ref bytevector-ieee-double-set! 2 (endianness little))
  
    (roundtrip
     -0.2822580337524414
     bytevector-ieee-double-ref bytevector-ieee-double-set! 2 (endianness little))
    
    ;; Double precision, offset 3, big-endian
 
    (roundtrip
     +inf.0
     bytevector-ieee-double-ref bytevector-ieee-double-set! 3 (endianness big))

    (roundtrip
     -inf.0
     bytevector-ieee-double-ref bytevector-ieee-double-set! 3 (endianness big))

    (roundtrip
     1e10
     bytevector-ieee-double-ref bytevector-ieee-double-set! 3 (endianness big))

    (roundtrip
     -0.2822580337524414
     bytevector-ieee-double-ref bytevector-ieee-double-set! 3 (endianness big))

    ;; Double precision, offset 3, little-endian
 
    (roundtrip
     +inf.0
     bytevector-ieee-double-ref bytevector-ieee-double-set! 3 (endianness little))

    (roundtrip
     -inf.0
     bytevector-ieee-double-ref bytevector-ieee-double-set! 3 (endianness little))
 
    (roundtrip
     1e10
     bytevector-ieee-double-ref bytevector-ieee-double-set! 3 (endianness little))

    (roundtrip
     -0.2822580337524414
     bytevector-ieee-double-ref bytevector-ieee-double-set! 3 (endianness little))))
