; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Marcus Crestani, David Frese,
; Mike Sperber, Martin Gasbichler

; Pre-Allocation
;
; Preallocation and keys are used to ensure that for every call to MAKE-STOB
; there is a corresponding call to ENSURE-SPACE to see if there is sufficient
; heap space.  ENSURE-SPACE returns a key and MAKE-STOB checks that the
; key it is passed is the most recently allocated key and that the space
; needed is no greater than the argument to ENSURE-SPACE.
; 
; Another solution would be to make ENSURE-SPACE and MAKE-STOB a single
; procedure.  The difficulty is that ENSURE-SPACE may trigger a garbage
; collection, which in turn requires that all live data be reachable
; from the VM's registers.  The VM solves this by only calling ENSURE-SPACE
; at the beginning of an instruction, before any values have been removed
; from the stack or any of the registers.  Once the key has been obtained
; the instruction is free to make any number of calls to MAKE-STOB, as long
; as the total heap space required is no more than what was checked for.
; 
; There is a flag, CHECK-PREALLOCATION?, that determines whether MAKE-STOB
; actually checks the keys.  In the VM as seen by the Pre-Scheme compiler
; this flag is defined to be #f and never set, so all of the key code is
; constant-folded into oblivion.
; 
; The main virtue of the keys is not that they can be checked but
; that they exist at all.  MAKE-STOB requires a key argument, and
; if there is none available you know that you forgot an ENSURE-SPACE.
; Occasionally I run the VM in Scheme with checking enabled, just
; to see if it all still works.

(define check-preallocation? #f)

(define (checking-preallocation?)
  check-preallocation?)

(define *heap-key* 0)
(define *okayed-space* 0)

(define (ensure-space cells)
  (s48-make-available+gc (cells->bytes cells))
  (cond (check-preallocation?
	 (set! *heap-key* (+ *heap-key* 1))
	 (set! *okayed-space* cells)
	 *heap-key*)
	(else
	 0)))

(define (allocate-space len key)	;len is in bytes
  (if check-preallocation?
      (let ((cells (bytes->cells len)))
	(if (not (and (= key *heap-key*)
		      (>= *okayed-space* cells)))
	    (error "invalid heap key" key cells))
	(set! *okayed-space* (- *okayed-space* cells))))
  (s48-allocate-small len))

;----------------

(define max-stob-size-in-cells
  (+ max-stob-contents-size-in-cells
     stob-overhead))

(define (make-stob type len key)
  (let ((addr (allocate-space (+ len
				 (cells->bytes stob-overhead))
			      key)))
    (initialize-stob addr type len)))

(define (make-d-vector type len key)
  (make-stob type (cells->bytes len) key))

(define make-b-vector make-stob)

; Versions of the above two procedures that can be used to allocate large
; objects.  These may trigger a GC and will return false if insufficient
; space is available after the GC.

(define (maybe-make-stob type len-in-bytes allocator)
  (let ((addr (allocator (+ len-in-bytes (cells->bytes stob-overhead)))))
    (if (null-address? addr)
	false
	(initialize-stob addr type len-in-bytes))))

(define (maybe-make-b-vector+gc type len)
  (maybe-make-stob type len s48-allocate-untraced+gc))

;; Hint: consider looking at s48-gc-can-allocate-unmovable? 
;; before calling maybe-make-unmovable-b-vector+gc because the
;; twospace-gc version of s48-allocate-(un)traced-unmovable+gc will stop
;; the program with an error message.
(define (maybe-make-unmovable-b-vector+gc type len)
  (maybe-make-stob type len s48-allocate-untraced-unmovable+gc))

(define (maybe-make-d-vector+gc type len)
  (maybe-make-stob type (cells->bytes len) s48-allocate-traced+gc))

(define (maybe-make-unmovable-d-vector+gc type len)
  (maybe-make-stob type (cells->bytes len) s48-allocate-traced-unmovable+gc))

; we can't go through ensure-space, as weak pointers might live in
; a separate area
(define (make-weak-pointer weak-pointer-size)
  (let ((addr (s48-allocate-weak+gc (cells->bytes weak-pointer-size))))
    (initialize-stob addr 
		     (enum stob weak-pointer) 
		     (cells->bytes (- weak-pointer-size 1)))))

; Add the header to a stob and add the tag to the address.

(define (initialize-stob addr type len)
  (store! addr (make-header type len))
  (address->stob-descriptor (address+ addr
				      (cells->bytes stob-overhead))))

; Used to copy stuff from the stack to the heap.

(define (header+contents->stob header contents key)
  (let* ((addr (allocate-space (+ (header-length-in-bytes header)
				      (cells->bytes stob-overhead))
				   key))
	 (data-addr (address+ addr (cells->bytes stob-overhead))))
    (store! addr header)
    (copy-memory! contents data-addr (header-length-in-bytes header))
    (address->stob-descriptor data-addr)))

;----------------

(define (stob-type obj)
  (header-type (stob-header obj)))

(define (stob-of-type? obj type)
  (and (stob? obj)
       (= (stob-type obj) type)))

;----------------
; Immutability

(define (immutable? thing)
  (or (not (stob? thing))
      (immutable-header? (stob-header thing))))

(define (make-immutable! thing)
  (if (not (immutable? thing))
      (stob-header-set! thing (make-header-immutable (stob-header thing)))))

;----------------
; D-vectors (vectors of descriptors)

(define (d-vector? obj)
  (and (stob? obj)
       (< (header-type (stob-header obj)) least-b-vector-type)))

; The type in these routines is used only for internal error checking.

(define (d-vector-length x)
  (assert (d-vector? x))
  (header-length-in-cells (stob-header x)))

(define (d-vector-ref x index)
  (assert (valid-index? index (d-vector-length x)))
  (fetch (address+ (address-after-header x) (cells->a-units index))))

(define (d-vector-set! x index value)
  (assert (valid-index? index (d-vector-length x)))
  (let ((addr (address+ (address-after-header x) (cells->a-units index))))
    (s48-write-barrier x addr value)
    (store! addr value)))

(define (d-vector-init! x index value)
  (assert (valid-index? index (d-vector-length x)))
  (store! (address+ (address-after-header x) (cells->a-units index))
          value))

;----------------
; B-vector = vector of bytes.

(define (b-vector? obj)
  (and (stob? obj)
       (>= (header-type (stob-header obj))
	   least-b-vector-type)))

(define (b-vector-length x)
  (assert (b-vector? x))
  (header-length-in-bytes (stob-header x)))

(define (b-vector-ref b-vector index)
  (assert (valid-index? index (b-vector-length b-vector)))
  (fetch-byte (address+ (address-after-header b-vector) index)))

(define (b-vector-set! b-vector index value)
  (assert (valid-index? index (b-vector-length b-vector)))
  (store-byte! (address+ (address-after-header b-vector) index) value))

; Various utilities

(define (valid-index? index len)
  (and (>= index 0) (< index len)))


