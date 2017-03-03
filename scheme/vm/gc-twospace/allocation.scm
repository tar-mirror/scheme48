; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, David Frese, Marcus Crestani, 
; Mike Sperber

; Interface to the VM for allocation

; (pre)allocates space for a small, fixed-sized objects
(define (s48-make-available+gc len)
  (if (not (bytes-available? len))
      (s48-collect #f))
  (if (not (bytes-available? len))
      (error "Scheme 48 heap overflow")))

; actually allocate a small, fixed-sized object, with no heap check and no GC
(define s48-allocate-small allocate)

; allocate a weak-pointer object, GCing to get room if necessary
(define (s48-allocate-weak+gc len)
  (s48-allocate-traced+gc len))

; tell the GC not to collect in any case
; mainly used on startup during reading the image
(define (s48-forbid-gc!) 
  (unspecific))

; the GC may collect again
(define (s48-allow-gc!)
    (unspecific))

; allocate a potentially large object containing pointers,
; GCing to get room if necessary
(define (s48-allocate-traced+gc len)
  (if (not (bytes-available? len))
      (s48-collect #f))
  (if (not (bytes-available? len))
      null-address
      (allocate len)))

; allocate a potentially large object not containing pointers,
; GCing to get room if necessary

;; Same again.  Just doing (define x y) for exported procedures X and Y
;; causes the Pre-Scheme compiler to emit bad code.
(define (s48-allocate-untraced+gc len)
  (s48-allocate-traced+gc len))

; unmovable objects are not supported.
(define (s48-gc-can-allocate-unmovable?) #f)
(define (s48-allocate-traced-unmovable+gc len)
  (= len 0) ;; for the type-checker
  (error "twospace gc does not support unmovable objects")
  null-address ;; for the correct signature
  )

(define (s48-allocate-untraced-unmovable+gc len)
  (= len 0) ;; for the type-checker
  (error "twospace gc does not support unmovable objects")
  null-address ;; for the correct signature
  )

(define (s48-unmovable? stob) 
  (= stob 0) ;; for the type-checker
  #f)

;; For allocation done outside the VM.

(define (s48-allocate-stob type size)
  (let* ((traced? (< type least-b-vector-type))
	 (length-in-bytes (if traced?
			      (cells->bytes size)
			      size))
	 (needed (+ length-in-bytes (cells->bytes stob-overhead)))
	 (thing (if traced?
		    (s48-allocate-traced+gc needed)
		    (s48-allocate-untraced+gc needed))))
    (if (null-address? thing)
	(error "insufficient heap space for external allocation"))
    (store! thing (make-header type length-in-bytes))
    (address->stob-descriptor (address+ thing
                                        (cells->bytes stob-overhead)))))

(define (s48-allocate-weak-stob type size)
  (s48-allocate-stob type size))

(define (s48-allocate-unmovable-stob type size)
  (= type 0) ;; for the type-checker
  (= size 0) ;; for the type-checker
  (error "twospace gc does not support unmovable objects")
  0 ;; for the correct signature
  )
