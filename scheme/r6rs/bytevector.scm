; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Harald Glab-Phlak, Mike Sperber, Will Clinger

; This is taken from the R6RS reference implementation by Mike
; Sperber, modified by Will Clinger.

(import-dynamic-externals "=scheme48external/r6rs")

(define-enumeration endianness
  (little big)
  endianness*)

(define bytevector? byte-vector?)

(define make-bytevector 
  (opt-lambda (size (fill 0)) 
    (if (and (>= fill -128) 
	     (<= fill 255))
	(make-byte-vector size fill)
	(error #f "wrong value to fill a byte vector must be octet" fill))))

(define bytevector-length byte-vector-length)

(define bytevector=? byte-vector=?)

(define (bytevector-fill! vector fill)
  (let loop ((index 0))
    (if (< index (bytevector-length vector))
	(begin (bytevector-u8-set! vector index fill)
	       (loop (+ index 1))))))


;; may be we need a few checks to fit the need -- look carefully
(define (bytevector-copy! source source-start target target-start count)
  (copy-bytes! source source-start target target-start count))

(define (bytevector-copy vector)
  (let* ((size (byte-vector-length vector))
	 (copy (make-byte-vector size 0)))
    (bytevector-copy! vector 0 copy 0 size)
    copy))

;; now the stuff with the typed bytevectors begins
(define (u8->s8 val)
  (if (> val  127)
      (- val 256)
      val))

(define (s8->u8 val)
  (if (negative? val)
      (+ val 256)
      val))

(define (bytevector-u8-ref vector k)
  (check-index vector k)
  (byte-vector-ref vector k))    

(define (bytevector-u8-set! vector k val)
  (check-index vector k)
  (check-range val 1 #f)
  (byte-vector-set! vector k val))  

(define (bytevector-s8-ref vector k)    
  (check-index vector k)
  (u8->s8 (bytevector-u8-ref vector k)))

(define (bytevector-s8-set! vector k val)
  (check-index vector k)
  (check-range val 1 #t)
  ( bytevector-u8-set! vector k (s8->u8 val)))

(define (bytevector->u8-list octets)
  (let loop ((n (bytevector-length octets)) (r '()))
    (if (zero? n)
	r
	(loop (- n 1)
	      (cons (bytevector-u8-ref octets (- n 1)) r)))))

(define (u8-list->bytevector list)
  (let ((vect (make-bytevector (length list))))
    (let loop ((l list)
	       (index 0))
      
      (if (not (eq? l '())) 
	  (begin  (bytevector-u8-set! vect index (car l))
		  (loop (cdr l) 
			(+ index 1)))))
    vect))

;; the integer view (native integers) to a bytevector
(define (bytevector-uint-ref bytevector index endness size)
  (case endness
    ((big)
     (do ((i 0 (+ i 1))
	  (result 0 (+ (arithmetic-shift result 8)
		       (bytevector-u8-ref bytevector (+ index i)))))
	 ((>= i size)
	  result)))
    ((little)
     (do ((i (- size 1) (- i 1))
	  (result 0 (+ (arithmetic-shift result 8)
		       (bytevector-u8-ref bytevector (+ index i)))))
	 ((< i 0)
	  result)))
    (else
     (error 'bytevector-uint-ref "Invalid endianness: " endness))))

(define (bytevector-sint-ref bytevector index endness size)
  (let* ((high-byte (bytevector-u8-ref bytevector
				       (if (eq? endness (endianness big))
					   index
					   (+ index size -1))))
         (uresult (bytevector-uint-ref bytevector index endness size)))
    (if (> high-byte 127)
        (- uresult (expt 256 size))
	uresult)))

(define (bytevector-uint-set! bytevector index val endness size)
  (check-range val size #f)
  (case endness
    ((little)
     (do ((i 0 (+ i 1))
	  (val val (quotient val 256)))
	 ((>= i size))
       (bytevector-u8-set! bytevector (+ index i) (remainder val 256))))
    ((big)
     (do ((i (- size 1) (- i 1))
	  (val val (quotient val 256)))
	 ((< i 0))
       (bytevector-u8-set! bytevector (+ index i) (remainder val 256))))
    (else
     (error 'bytevector-uint-set! "Invalid endianness: " endness))))

(define (bytevector-sint-set! bytevector index val endness size)
  (check-range val size #t)
  (let ((uval (if (< val 0)
                  (+ val (* 128 (expt 256 (- size 1))))
                  val)))
    (bytevector-uint-set! bytevector index uval endness size)))

(define (bytevector->uint-list vector endness size)
  ((make-bytevect->int-list bytevector-uint-ref) 
   vector endness size))

(define (bytevector->sint-list vector endness size)  
  ((make-bytevect->int-list bytevector-sint-ref) 
   vector endness size))

(define (uint-list->bytevector list endness size)
  ((make-int-list->bytevect bytevector-uint-set!) 
   list endness size))

(define (sint-list->bytevector list endness size)
  ((make-int-list->bytevect bytevector-sint-set!) 
   list endness size))

(define (make-uint-ref size)
  (lambda (bytevector k endianness)
    (bytevector-uint-ref bytevector k endianness size)))

(define (make-sint-ref size)
  (lambda (bytevector k endianness)
    (bytevector-sint-ref bytevector k endianness size)))

(define (make-uint-set! size)
  (lambda (bytevector k n endianness)
    (bytevector-uint-set! bytevector k n endianness size)))

(define (make-sint-set! size)
  (lambda (bytevector k n endianness)
    (bytevector-sint-set! bytevector k n endianness size)))

(define (make-ref/native base base-ref)
  (lambda (bytevector index)
    (ensure-aligned index base)
    (base-ref bytevector index (native-endianness))))

(define (make-set!/native base base-set!)
  (lambda (bytevector index val)
    (ensure-aligned index base)
    (base-set! bytevector index val (native-endianness))))


;; uint16
(define bytevector-u16-ref  (make-uint-ref 2))

(define bytevector-s16-ref (make-sint-ref 2))

(define bytevector-u16-native-ref (make-ref/native 2 bytevector-u16-ref))

(define bytevector-s16-native-ref (make-ref/native 2 bytevector-s16-ref)) 

(define bytevector-u16-set!  (make-uint-set! 2))

(define bytevector-s16-set!  (make-sint-set! 2))

(define bytevector-u16-native-set! (make-set!/native 2 bytevector-u16-set!))

(define bytevector-s16-native-set! (make-set!/native 2 bytevector-s16-set!))

;; uint32
(define bytevector-u32-ref  (make-uint-ref 4))

(define bytevector-s32-ref (make-sint-ref 4))

(define bytevector-u32-native-ref (make-ref/native 4 bytevector-u32-ref))

(define bytevector-s32-native-ref (make-ref/native 4 bytevector-s32-ref)) 

(define bytevector-u32-set!  (make-uint-set! 4))

(define bytevector-s32-set!  (make-sint-set! 4))

(define bytevector-u32-native-set! (make-set!/native 4 bytevector-u32-set!))

(define bytevector-s32-native-set! (make-set!/native 4 bytevector-s32-set!))

;; uint64
(define bytevector-u64-ref  (make-uint-ref 8))

(define bytevector-s64-ref (make-sint-ref 8))

(define bytevector-u64-native-ref (make-ref/native 8 bytevector-u64-ref))

(define bytevector-s64-native-ref (make-ref/native 8 bytevector-s64-ref)) 

(define bytevector-u64-set!  (make-uint-set! 8))

(define bytevector-s64-set!  (make-sint-set! 8))

(define bytevector-u64-native-set! (make-set!/native 8 bytevector-u64-set!))

(define bytevector-s64-native-set! (make-set!/native 8 bytevector-s64-set!))

;; helper procedures

(define (make-bytevect->int-list bytevect-ref)
  (lambda (vect endness size)
    (let ((length (bytevector-length vect)))
      (let loop ((i 0) (r '()))
	(if (>= i length)
	    (reverse r)
	    (loop (+ i size)
		  (cons (bytevect-ref vect i endness size) r)))))))

(define (make-int-list->bytevect bytevect-set!)
  (lambda (l endness size)
    (let ((bytevect (make-bytevector (* size (length l)))))
      (let loop ((i 0) (l l))
	(if (null? l)
	    bytevect
	    (begin
	      (bytevect-set! bytevect i (car l) endness size)
	      (loop (+ i size) (cdr l))))))))
;; general checks

(define (ensure-aligned index base)
  (if (not (zero? (remainder index base)))
      (assertion-violation  'ensure-aligned "non-aligned bytevector access" index base)))


(define (check-range value byte-count signed-check?)
  (let* ((bits (* byte-count 8))
	 (unsigned-low 0)
	 (unsigned-high (- (expt 2 bits) 1))
	 (signed-low (* -1 (expt 2 (- bits 1))))
	 (signed-high (- (expt 2 (- bits 1)) 1)))
    (if signed-check?
	(if (not (and (>= value signed-low) (<= value signed-high)))
	    (assertion-violation  'check-range  
				  "range check for value failed / signed - value does not fit into " 
				  byte-count 'bytes 'checked-value: value))
	(if (not (and (>= value unsigned-low) (<= value unsigned-high)))
	    (assertion-violation  'check-range  
				  "range check for value failed / unsigned - value does not fit into " 
				  byte-count 'bytes 'checked-value: value)))))

(define (check-index b i)
  (if (or (> i (- (bytevector-length b) 1)) (< i 0))
      (assertion-violation  'check-index 
			    "invalid index forr  vector must be in the range of" 
			    0 'to  (- (bytevector-length b) 1))))

(define (native-endianness)
  (if (external-r6rs-big-endian?)
      (endianness big)
      (endianness little)))

;; external fun definition

(import-lambda-definition-2 external-r6rs-big-endian?
			    ()
			    "r6rs_is_big_endian")

