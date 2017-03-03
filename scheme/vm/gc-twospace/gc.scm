; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, David Frese, Mike Sperber,
; Martin Gasbichler, Marcus Crestani

; Collector

; (S48-TRACE-LOCATIONS! start end)	; trace all roots
; (S48-TRACE-VALUE value) => copied value
; (S48-TRACE-STOB-CONTENTS! stob)

(define *gc-count* 0)
(define (s48-gc-count) *gc-count*)

(define *gc-seconds* 0)
(define *gc-mseconds* 0)

(define (s48-gc-run-time)
  (values *gc-seconds* *gc-mseconds*))

(define (s48-collect force-major?)
  (receive (start-seconds start-mseconds) (run-time)
    (set! *from-begin* (heap-begin))
    (set! *from-end* (heap-limit))
    (swap-spaces)
    (set-heap-pointer! (heap-begin))
    (set! *weak-pointer-hp* null-address)
    (s48-gc-root)                      ; trace the interpreter's roots
    (do-gc)
    (clean-weak-pointers)
    (s48-post-gc-cleanup #t           ; it's always a major collection
                         (in-trouble?))
    (set! *gc-count* (+ *gc-count* 1))
    (receive (end-seconds end-mseconds) (run-time)
      (if (>= end-mseconds start-mseconds)
          (begin
            (set! *gc-seconds* (+ *gc-seconds*
                                  (- end-seconds start-seconds)))
            (set! *gc-mseconds* (+ *gc-mseconds*
                                   (- end-mseconds start-mseconds))))
          (begin
            (set! *gc-seconds* (+ *gc-seconds*
                                  (- (- end-seconds start-seconds) 1)))
            (set! *gc-mseconds* (+ *gc-mseconds*
                                   (- (+ 1000 end-mseconds)
                                      start-mseconds))))))))
  

(define *from-begin*)
(define *from-end*)

(define (in-oldspace? descriptor)
  (and (stob? descriptor)
       (let ((a (address-after-header descriptor)))
         (and (address>= a *from-begin*)
              (address< a *from-end*)))))

(define (s48-trace-value stob)
  (if (in-oldspace? stob)
      (copy-object stob)
      stob))

; Scan the heap, copying pointed to objects, starting from START.  Quit once
; the scanning pointer catches up with the heap pointer.

(define (do-gc)
  (let loop ((start (heap-begin)))
    (let ((end (heap-pointer)))
      (s48-trace-locations! start end)
      (cond ((< (s48-available) 0)
	     (error "GC error: ran out of space in new heap"))
	    ((address< end (heap-pointer))
	     (loop end))))))

(define (s48-trace-stob-contents! stob)
  (let ((start (address-after-header stob))
	(size (bytes->a-units (header-length-in-bytes (stob-header stob)))))
    (s48-trace-locations! start (address+ start size))))

; Copy everything pointed to from somewhere between START (inclusive)
; and END (exclusive).

(define (s48-trace-locations! start end)
  (let loop ((addr start) (frontier (heap-pointer)))
    (if (address< addr end)
	(let ((thing (fetch addr))
	      (next (address1+ addr)))
	  (cond ((header? thing)
		 (cond ((b-vector-header? thing)
			(loop (address+ next (header-length-in-a-units thing))
			      frontier))
		       ((continuation-header? thing)
			(let ((size (header-length-in-a-units thing)))
			  (set-heap-pointer! frontier)
			  (trace-continuation next size)
			  (loop (address+ next size)
				(heap-pointer))))
		       ((transport-link-cell-header? thing)
			(let ((size (header-length-in-a-units thing)))
			  (begin
			    (set-heap-pointer! frontier)
			    (trace-transport-link-cell next size)
			    (loop (address+ next size)
				  (heap-pointer)))))
		       (else
			(loop next frontier))))
		((in-oldspace? thing)
		 (receive (new-thing frontier)
		     (real-copy-object thing frontier)
		   (store! addr new-thing)
		   (loop next frontier)))
		(else
		 (loop next frontier))))
	(set-heap-pointer! frontier))))
;  0)  ; for the type-checker

; Copy THING if it has not already been copied.

(define (copy-object thing)
  (receive (new-thing new-hp)
      (real-copy-object thing (heap-pointer))
    (set-heap-pointer! new-hp)
    new-thing))

; Non-heap-pointer version for better code in TRACE-LOCATIONS

(define (real-copy-object thing frontier)
  (let ((h (stob-header thing)))
    (cond ((stob? h)            ;***Broken heart
	   ;; (assert (in-newspace? h))
	   (values h frontier))
	  ((and (vm-eq? weak-pointer-header h)
		(in-oldspace? (fetch (address-after-header thing))))
	   (copy-weak-pointer thing frontier))
	  (else
	   (store! frontier h)
	   (let* ((data-addr (address+ frontier (cells->a-units stob-overhead)))
		  (new (address->stob-descriptor data-addr)))
	     (stob-header-set! thing new) ;***Break heart
	     (copy-memory! (address-after-header thing)
			   data-addr
			   (header-length-in-bytes h))
	     (values new
		     (address+ data-addr (header-length-in-a-units h))))))))

(define (s48-extant? thing)
  (or (not (stob? thing))
      (not (in-oldspace? thing))
      (stob? (stob-header thing))))

;----------------
; Weak pointers
;
; Weak pointers are copied into contiguous blocks so that they can be
; scanned after the main GC has finished.  They have their own heap pointer
; and heap limit.

(define *weak-pointer-hp*)
(define *weak-pointer-limit*)

; header + one slot
(define weak-pointer-size 2)

; The number of weak pointers in each block.
(define weak-pointer-alloc-count 128)

; The size of a block of weak pointers.
(define weak-pointer-alloc-quantum
  (cells->a-units (* weak-pointer-alloc-count weak-pointer-size)))

; Used both to detect weak pointers and for setting the headers when the
; weak-pointer blocks are scanned.
(define weak-pointer-header
  (make-header (enum stob weak-pointer) (cells->bytes (- weak-pointer-size 1))))

; A header used to stop the GC from scanning weak-pointer blocks.
(define weak-alloc-area-header
  (make-header (enum stob byte-vector)
	       (cells->bytes (- (* weak-pointer-alloc-count weak-pointer-size)
				1))))  ; don't count the header

(define (copy-weak-pointer weak frontier)
  (let ((frontier (if (or (null-address? *weak-pointer-hp*)
			  (address>= *weak-pointer-hp* *weak-pointer-limit*))
		      (allocate-more-weak-pointer-space frontier)
		      frontier)))
    (let ((new (address->stob-descriptor
		(address+ *weak-pointer-hp* (cells->a-units stob-overhead)))))
      (store! (address1+ *weak-pointer-hp*) (fetch (address-after-header weak)))
      (set! *weak-pointer-hp* (address2+ *weak-pointer-hp*))
      (stob-header-set! weak new) ;***Break heart
      (values new frontier))))

; The weak pointer blocks are linked in their third slot (= the header space
; of the second weak pointer).  The header for the first weak pointer contains
; a header for the block, and the value slots contain the (untraced) values.

(define (allocate-more-weak-pointer-space frontier)
  (let ((old *weak-pointer-hp*)
	(new-frontier (address+ frontier weak-pointer-alloc-quantum)))
    (set! *weak-pointer-hp* frontier)
    (set! *weak-pointer-limit* new-frontier)
    (store! *weak-pointer-hp* weak-alloc-area-header)
    (store! (address2+ *weak-pointer-hp*) (address->integer old))
    new-frontier))

; If any weak pointers were found, then get the limits of the most recently
; allocated block and scan it and the rest of the blocks.  Put a string header
; on the unused portion of the block the most recent block.

(define (clean-weak-pointers)
  (if (not (null-address? *weak-pointer-hp*))
      (let ((start (address- *weak-pointer-limit* weak-pointer-alloc-quantum))
	    (end *weak-pointer-hp*))
	(scan-weak-pointer-blocks start end)
	(if (not (address>= end *weak-pointer-limit*))
	    (let ((unused-portion (address-difference *weak-pointer-limit*
						      (address1+ end))))
	      (store! end (make-header (enum stob byte-vector)
				       (cells->bytes
					(a-units->cells unused-portion)))))))))

(define (scan-weak-pointer-blocks start end)
  (let loop ((start start) (end end))
    (let ((next (integer->address (fetch (address2+ start)))))
      (scan-weak-pointer-block start end)
      (if (not (null-address? next))
	  (loop (address- next weak-pointer-alloc-quantum) next)))))

; Go from START to END putting headers on the weak pointers and seeing if
; their contents were traced.

(define (scan-weak-pointer-block start end)
  (do ((scan start (address2+ scan)))
      ((address>= scan end))
    (store! scan weak-pointer-header)
    (let ((value (fetch (address1+ scan))))
      (if (and (in-oldspace? value)
	       (stob? value))
	  (store! (address1+ scan)
		  (let ((h (stob-header value)))
		    (if (stob? h) h false)))))))

; transport link cells

;; Due to circular dependencies when this file tries to open the
;; structure primitivies where the setters and mutators for PAIRs and
;; TRANSPORT-LINK-CELLs are.  So there is no other way than defining
;; the needed setters, mutators, and predicates another time:

;; record selector, mutator, and predicate maker
(define (make-gc-selector index)
  (lambda (record)
    (fetch (address+ (address-after-header record) (cells->a-units index)))))

(define (make-gc-mutator index)
  (lambda (record object)
    (store! (address+ (address-after-header record) 
		      (cells->a-units index)) object)))

(define (make-gc-predicate stob-type)
  (lambda (thing)
    (if (stob? thing)
	(= (header-type (stob-header thing))
	   stob-type)
	#f)))

(define gc-pair? (make-gc-predicate (enum stob pair)))
(define gc-car (make-gc-selector 0))
(define gc-cdr (make-gc-selector 1))
(define gc-set-car! (make-gc-mutator 0))
(define gc-set-cdr! (make-gc-mutator 1))

(define gc-tlc-key (make-gc-selector 0))
(define gc-tlc-tconc (make-gc-selector 2))
(define gc-set-tlc-tconc! (make-gc-mutator 2))

;; follow already forwarded objects
(define (follow thing)
  (if (and (stob? thing)
	   (stob? (stob-header thing)))
      (stob-header thing)
      thing))

;; trace a transport link cell
(define (trace-transport-link-cell contents-pointer size)
  (s48-trace-locations! contents-pointer (address+ contents-pointer size))
  (let* ((tlc (address->stob-descriptor contents-pointer))
	 (tconc (gc-tlc-tconc tlc)))
    (if (and (gc-pair? tconc) (stob? (gc-tlc-key tlc)))
	;; The tconc's car and cdr may already be forwarded, so
	;; follow them, if needed.
	(let ((tconc-car (follow (gc-car tconc)))
	      (tconc-cdr (follow (gc-cdr tconc))))
	  (if (and (gc-pair? tconc-car)
		   (gc-pair? tconc-cdr))
	      ;; the tlc's tconc field is a valid tconc queue
	      ;; now allocate a new pair in the new space
	      ;; that will become the tconc's new last element
	      (let ((pair-address (heap-pointer)))
		(store! pair-address (make-header (enum stob pair) (cells->bytes 2)))
		(let* ((newpair (address->stob-descriptor 
			      (address+ pair-address (cells->a-units stob-overhead))))
		       (new-frontier (address+ pair-address (cells->a-units 3))))
		  (if (gc-pair? newpair)
		      (begin
			;; initialize the new pair's fields with #f
			(gc-set-car! newpair (enter-boolean #f))
			(gc-set-cdr! newpair (enter-boolean #f))
			;; enqueue the tlc in the tconc queue
			(gc-set-car! tconc-cdr tlc)
			(gc-set-cdr! tconc-cdr newpair)
			(gc-set-cdr! tconc newpair)
			;; reset the tlc's tconc field
			(gc-set-tlc-tconc! tlc (enter-boolean #f))
			(set-heap-pointer! new-frontier))))))))))

(define (transport-link-cell-header? x)
  (= (header-type x) (enum stob transport-link-cell)))
