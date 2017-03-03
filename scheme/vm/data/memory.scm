; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, David Frese, Taylor Campbell

(define (address1+ x)
  (address+ x addressing-units-per-cell))

(define (address2+ x)
  (address1+ (address1+ x)))

; Memory access

(define *memory*)
(define *memory-begin* 0)
(define *memory-end* 0)

(define (memory-begin)
  *memory-begin*)

; Size of memory in cells.
(define (memory-size)
  (a-units->cells (address-difference *memory-end* *memory-begin*)))

(define (create-memory size initial-value)   ;size in cells
  (let ((size (cells->a-units size)))
    (cond ((not (= size (address-difference *memory-end* *memory-begin*)))
	   (if (not (= *memory-end* 0))
	       (deallocate-memory *memory*))
           (set! *memory* (allocate-memory size))
	   (if (null-address? *memory*)
	       (error "out of memory, unable to continue"))
	   (set! *memory-begin* *memory*)
           (set! *memory-end* (+ *memory* size))))))

(define fetch word-ref)
(define fetch-byte unsigned-byte-ref)
(define fetch-flonum flonum-ref)
(define store! word-set!)
(define store-byte! unsigned-byte-set!)
(define store-flonum! flonum-set!)

(define fetch-string char-pointer->string)
(define fetch-nul-terminated-string char-pointer->nul-terminated-string)

;----------------------------------------------------------------

(define (address->stob-descriptor addr)
  (add-stob-tag (address->integer addr)))

(define stob-overhead 1)  ; header uses up one descriptor

(define (offset-after-header stob)
  (assert (stob? stob))
  (remove-stob-tag stob))

(define (address-after-header stob)
  (integer->address (offset-after-header stob)))

; Note that first converting to an address and then doing arithmetic
; will result in C undefined behavior when the target address is NULL.
(define (address-at-header stob)
  (integer->address (- (offset-after-header stob)
		       (cells->bytes 1))))

(define (stob-header stob)
  (fetch (address-at-header stob)))

(define (stob-header-set! stob header)
  (store! (address-at-header stob) header))
