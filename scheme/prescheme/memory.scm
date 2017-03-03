; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Marcus Crestani
; David Frese, Taylor Campbell


; An implementation of Pre-Scheme's memory interface that can detect some
; stray reads and writes.  It has numerous limitiations:
;    Allocations are always on page boundaries.
;    No more than 16 megabytes can be allocated at once.
;    More than 32 or 64 or so allocations result in addresses being
;       bignums (dealloctions have no effect on this).
;
; Memory is represented as a vector of byte-vectors, with each byte-vector
; representing a 16-megabyte page.  Allocations are always made on page
; boundaries, so the byte-vectors only need be as large as the allocated
; areas.  Pages are never re-used.
;
; (Scheme 48 still calls byte-vectors code-vectors.)


; Addresses are distinct from integers.

(define-record-type address :address
  (make-address index)
  address?
  (index address-index))
  
(define-record-discloser :address
  (lambda (addr) (list 'address (address-index addr))))

; We add 100000000 to addresses to make them

(define address-offset 100000000)

(define (address->integer addr)
  (+ (address-index addr) address-offset))

(define (integer->address int)
  (make-address (- int address-offset)))

(define (address+ address integer)
  (make-address (+ (address-index address) integer)))

(define (address- address integer)
  (make-address (- (address-index address) integer)))

(define (address-binop op)
  (lambda (address1 address2)
    (op (address-index address1) (address-index address2))))

(define address-difference (address-binop -))
(define address= (address-binop =))
(define address< (address-binop <))
(define address<= (address-binop <=))
(define address> (address-binop >))
(define address>= (address-binop >=))

(define null-address (make-address -1))

(define (null-address? address)
  (address= address null-address))

; Memory

(define *memory* (make-vector 16 #f))    ; vector of pages
(define log-max-size 25)                 ; log of page size
(define address-shift (- log-max-size))  ; turns addresses into page indices

(define max-size (arithmetic-shift 1 log-max-size))  ; page size

(define address-mask                     ; mask to get address within page
  (- (arithmetic-shift 1 log-max-size) 1))

(define *next-index* 0)                  ; next available page

(define (reinitialize-memory)
  (set! *memory* (make-vector 16 #f))
  (set! *next-index* 0))

; Extend the page vector if necessary, and then make a page of the
; appropriate size.

(define (allocate-memory size)
  (cond ((> size max-size)
	 null-address)  ; error result
	(else
	 (if (>= *next-index* (vector-length *memory*))
	     (let ((new (make-vector (* 2 (vector-length *memory*)))))
	       (do ((i 0 (+ i 1)))
		   ((>= i (vector-length *memory*)))
		 (vector-set! new i (vector-ref *memory* i)))
	       (set! *memory* new)))
	 (let ((index *next-index*))
	   (set! *next-index* (+ *next-index* 1))
	   (vector-set! *memory* index (make-code-vector size 0))
	   (make-address (arithmetic-shift index log-max-size))))))

; Turning an address into a page or page index

(define (address->vector address)
  (vector-ref *memory* (arithmetic-shift address address-shift)))

(define (address->vector-index address)
  (bitwise-and address address-mask))

; Throw away the page containing ADDRESS, which must be the first address in
; that page, 

(define (deallocate-memory address)
  (let ((address (address-index address)))
    (let ((vector (address->vector address))
	  (byte-address (address->vector-index address)))
      (if (and vector (= byte-address 0))
	  (vector-set! *memory* (arithmetic-shift address address-shift) #f)
	  (assertion-violation 'deallocate-memory "bad deallocation address" address)))))

; Various ways of accessing memory

(define (unsigned-byte-ref address)
  (let ((address (address-index address)))
    (code-vector-ref (address->vector address)
		     (address->vector-index address))))

(define (signed-code-vector-ref bvec i)
  (let ((x (code-vector-ref bvec i)))
    (if (< x 128)
	x
	(bitwise-ior x -128))))

(define (word-ref address)
  (let ((address (address-index address)))
    (let ((vector (address->vector address))
	  (byte-address (address->vector-index address)))
      (if (not (= 0 (bitwise-and byte-address (- bytes-per-cell 1))))
	  (assertion-violation 'word-ref "unaligned address error" address)
	  (do ((byte-offset 0 (+ byte-offset 1))
	       (shift-offset (- bits-per-cell bits-per-byte) 
			     (- shift-offset bits-per-byte))
	       (word 0
		     (+ word
			(arithmetic-shift ((if (= 0 byte-offset)
					       signed-code-vector-ref
					       code-vector-ref)
					   vector
					   (+ byte-address byte-offset))
					  shift-offset))))
		((or (>= byte-offset bytes-per-cell) (< shift-offset 0))
		 word))))))

(define (unsigned-byte-set! address value)
  (let ((address (address-index address)))
    (code-vector-set! (address->vector address)
		      (address->vector-index address)
		      (bitwise-and 255 value))))
	    
(define (word-set! address value)
  (let ((address (address-index address)))
    (let ((vector (address->vector address))
	  (byte-address (address->vector-index address)))
      (if (not (= 0 (bitwise-and byte-address 3)))
	  (assertion-violation 'word-set! "unaligned address error" address))
      (do ((byte-offset 0 (+ byte-offset 1))
	   (shift-offset (- bits-per-cell bits-per-byte) 
			 (- shift-offset bits-per-byte)))
	  ((or (>= byte-offset bytes-per-cell) (< shift-offset 0)))
	(code-vector-set! vector 
			  (+ byte-address byte-offset)
			  (bitwise-and 255 
				       (arithmetic-shift value 
							 (- shift-offset))))))))

; With the right access to the flonum bits we could actually make these
; work.  Something to do later.

(define (flonum-ref address)
  (if #t					; work around type checker bug
      (assertion-violation 'flonum-ref "call to FLONUM-REF" address)))

(define (flonum-set! address value)
  (if #t					; work around type checker bug
      (assertion-violation 'flonum-set! "call to FLONUM-SET!" address value)))

; Block I/O procedures.

(define (write-block port address count)
  (let ((address (address-index address)))
    (let ((vector (address->vector address))
	  (byte-address (address->vector-index address)))
      (do ((i 0 (+ i 1)))
	  ((>= i count))
	(write-byte (code-vector-ref vector (+ i byte-address))
		    port))
      (enum errors no-errors))))

(define (read-block port address count)
  (let ((address (address-index address)))
    (cond ((not (byte-ready? port))
	   (values 0 #f (enum errors no-errors)))
	  ((eof-object? (peek-byte port))
	   (values 0 #t (enum errors no-errors)))
	  (else
	   (let ((vector (address->vector address))
		 (byte-address (address->vector-index address)))
	     (let loop ((i 0))
	       (if (or (= i count)
		       (not (byte-ready? port)))
		   (values i #f (enum errors no-errors))
		   (let ((b (read-byte port)))
		     (cond ((eof-object? b)
			    (values i #f (enum errors no-errors)))
			   (else
			    (code-vector-set! vector
					      (+ i byte-address)
					      b)
			    (loop (+ i 1))))))))))))

(define (copy-memory! from to count)
  (let ((from (address-index from))
	(to (address-index to)))
    (let ((from-vector (address->vector from))
	  (from-address (address->vector-index from))
	  (to-vector (address->vector to))
	  (to-address (address->vector-index to)))
      (if (>= from-address to-address)
	  (do ((i 0 (+ i 1)))
	      ((>= i count))
	    (code-vector-set! to-vector
			      (+ i to-address)
			      (code-vector-ref from-vector
					       (+ i from-address))))
	  (do ((i (- count 1) (- i 1)))
	      ((negative? i))
	    (code-vector-set! to-vector
			      (+ i to-address)
			      (code-vector-ref from-vector
					       (+ i from-address))))))))
  
(define (memory-equal? from to count)
  (let ((from (address-index from))
	(to (address-index to)))
    (let ((from-vector (address->vector from))
	  (from-address (address->vector-index from))
	  (to-vector (address->vector to))
	  (to-address (address->vector-index to)))
      (let loop ((i 0))
	(cond ((>= i count)
	       #t)
	      ((= (code-vector-ref to-vector (+ i to-address))
		  (code-vector-ref from-vector (+ i from-address)))
	       (loop (+ i 1)))
	      (else
	       #f))))))

; Turn the LENGTH bytes starting from ADDRESS into a string.

(define (char-pointer->string address length)
  (let ((address (address-index address)))
    (let ((vector (address->vector address))
	  (byte-address (address->vector-index address))
	  (string (make-string length)))
      (do ((i 0 (+ i 1)))
	  ((= i length))
	(string-set! string
		     i
		     (ascii->char (code-vector-ref vector (+ byte-address i)))))
      string)))

; Turn the bytes from ADDRESS to the next nul (byte equal to 0) into a
; string.  This is a trivial operation in C.

(define (char-pointer->nul-terminated-string address)
  (let ((index (address-index address)))
    (let ((vector (address->vector index))
	  (byte-address (address->vector-index index)))
      (char-pointer->string address (index-of-first-nul vector byte-address)))))
  
(define (index-of-first-nul vector address)
  (let loop ((i address))
    (cond ((= i (code-vector-length vector))
	   (assertion-violation 'char-pointer->string "CHAR-POINTER->STRING called on pointer with no nul termination"))
	  ((= 0 (code-vector-ref vector i))
	   (- i address))
	  (else
	   (loop (+ i 1))))))
	  
