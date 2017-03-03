; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Harald Glab-Phlak, Mike Sperber

(define (string->utf8 string)
  (enc:string->utf-8 string))

; If the bytevector begins with the three-byte sequence #xef #xbb
; #xbf, then those bytes are ignored.  (They are conventionally used
; as a signature to indicate UTF-8 encoding.  The string->utf8
; procedure does not emit those bytes, but UTF-8 encodings produced by
; other sources may contain them.)
  
(define (replacement-character) 
  (integer->char #xfffd))

(define (begins-with-utf8-bom? bv)
  (let* ((n (bytevector-length bv)))
    (and (<= 3 n)
	 (= #xef (bytevector-u8-ref bv 0))
	 (= #xbb (bytevector-u8-ref bv 1))
	 (= #xbf (bytevector-u8-ref bv 2)))))

(define (utf8->string bv)
  (if (begins-with-utf8-bom? bv)
      (let ((start 3)
	    (count (- (bytevector-length bv) 3)))
	(enc:utf-8->string-n bv start count (replacement-character)))
      (enc:utf-8->string bv (replacement-character))))

(define string->utf16 
  (opt-lambda (string (endness #f))
    (let ((text-codec
	   (case endness
	     ((#f big) utf-16be-codec)
	     ((little) utf-16le-codec)
	     (else (endianness-violation 'string->utf16 endness)))))
      (enc:string->bytes text-codec string))))

(define (maybe-utf16-bom bytevector n)
  (and (<= 2 n)
       (let ((b0 (bytevector-u8-ref bytevector 0))
	     (b1 (bytevector-u8-ref bytevector 1)))
	 (or (and (= b0 #xfe) (= b1 #xff) (endianness big))
	     (and (= b0 #xff) (= b1 #xfe) (endianness little))))))

(define utf16->string 
  (opt-lambda (bytevector endness (endianness-mandatory? #f))
    (let ((n (bytevector-length bytevector)))
      (call-with-values
	  (lambda ()
	    (cond
	     (endianness-mandatory? (values endness 0))
	     ((maybe-utf16-bom bytevector n) 
	      => (lambda (endness)
		   (values endness 2)))
	     (else (values endness 0))))
	(lambda (endness start)
	  (let ((text-codec (case endness
			      ((big) utf-16be-codec)
			      ((little) utf-16le-codec)
			      (else
			       (endianness-violation 'utf16->string endness))))
		(conv-len (- n start)))
	    (if (not (zero? (remainder n 2)))
		(assertion-violation 'utf16->string "Bytevector has bad length." bytevector))
	    (enc:bytes->string-n text-codec bytevector start conv-len (replacement-character))))))))

; There is no utf-32-codec, so we can't use textual i/o for this.

(define string->utf32 
  (opt-lambda (string (endness #f))
    (let ((text-codec (case endness
			((#f big) utf-32be-codec)
			((little) utf-32le-codec)
			(else (endianness-violation 'string->utf32 endness)))))
      (enc:string->bytes text-codec string))))

(define (maybe-utf32-bom bytevector n)
  (and (<= 4 n)
       (let ((b0 (bytevector-u8-ref bytevector 0))
	     (b1 (bytevector-u8-ref bytevector 1))
	     (b2 (bytevector-u8-ref bytevector 2))
	     (b3 (bytevector-u8-ref bytevector 3)))
	 (or (and (= b0 0) (= b1 0) (= b2 #xfe) (= b3 #xff)
		  (endianness big))
	     (and (= b0 #xff) (= b1 #xfe) (= b2 0) (= b3 0)
		  (endianness little))))))
  
(define utf32->string 
  (opt-lambda (bytevector endness (endianness-mandatory? #f))
    (let ((n (bytevector-length bytevector)))
      (call-with-values
	  (lambda ()
	    (cond
	     (endianness-mandatory? (values endness 0))
	     ((maybe-utf32-bom bytevector n)
	      => (lambda (endness)
		   (values endness 4)))
	     (else (values endness 0))))
	(lambda (endness start)
	  (let ((text-codec (case endness
			      ((big) utf-32be-codec)
			      ((little) utf-32le-codec)
			      (else
			       (endianness-violation 'utf32->string endness))))
		(conv-len (- n start)))
	    (if (not (zero? (remainder n 4)))
		(assertion-violation 'utf32->string "Bytevector has bad length." bytevector))
	    (enc:bytes->string-n text-codec bytevector start conv-len (replacement-character))))))))

(define (endianness-violation who what)
  (assertion-violation who "bad endianness" what))
