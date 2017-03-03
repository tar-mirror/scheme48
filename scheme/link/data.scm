; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Marcus Crestani


; Scheme 48's data representations, for writing heap images.
; Defines things needed by TRANSPORT (which is called by WRITE-IMAGE, which
; is called by the static linker).
; Adapted from vm/util/vm-utilities.scm and vm/data/data.scm.


(define (low-bits n k)
  (bitwise-and n (- (arithmetic-shift 1 k) 1)))

(define (high-bits n k)
  (arithmetic-shift n (- 0 k)))

(define (adjoin-bits high low width)
  (bitwise-ior (arithmetic-shift high width) low))


; Data descriptions copied from DATA.SCM

(define-enumeration tag
  (fixnum
   immediate
   header
   stob))

(define-enumeration imm
  (false      ; #f
   true       ; #t
   char
   unspecific
   undefined
   eof
   null))

(define bits-per-fixnum
  (- (if (< bits-per-cell s48-useful-bits-per-word)
	 bits-per-cell
	 s48-useful-bits-per-word)
     tag-field-width))

(define	   least-fixnum-value (- 0 (arithmetic-shift 1 (- bits-per-fixnum 1))))
(define greatest-fixnum-value (-   (arithmetic-shift 1 (- bits-per-fixnum 1))
				   1))

(define (fixnum? x)
  (and (integer? x)
       (>= x least-fixnum-value)
       (<= x greatest-fixnum-value)))

(define (immediate? x)
  (or (fixnum? x)
      (char? x)
      (eq? x #t)
      (eq? x '())    ; I think order is important here as () is a literal while
      (eq? x #f)     ; #F is generated with an instruction.  If () = #f then
		     ; we still lose on something like '(#F) (which is
		     ; present in the system). 
      ;; (eq? thing (unspecific))
      ))

(define car-offset 0)
(define cdr-offset 1)

(define closure-template-offset 0)
(define closure-env-offset	1)

(define location-id-offset     0)

; Procedures for manipulating bits

(define (make-descriptor tag data)
  (adjoin-bits data tag tag-field-width))

(define (make-immediate type info)
  (make-descriptor (enum tag immediate)
		   (adjoin-bits info type immediate-type-field-width)))

(define vm-true	 (make-immediate (enum imm true)  0))
(define vm-false (make-immediate (enum imm false) 0))
(define vm-null	 (make-immediate (enum imm null)  0))
(define vm-unspecific  (make-immediate (enum imm unspecific) 0))
(define vm-unbound     (make-immediate (enum imm undefined) 1))
(define vm-unassigned  (make-immediate (enum imm undefined) 2))

(define header-type-field-width (- immediate-type-field-width 1))

(define (make-header type length-in-bytes)
  (make-descriptor (enum tag header) (adjoin-bits length-in-bytes
					   type
					   (+ 1 header-type-field-width))))

(define (make-header-immutable header)
  (bitwise-ior header (arithmetic-shift 1 (+ tag-field-width
					     header-type-field-width))))

(define (make-stob-descriptor addr)
  (bitwise-ior (enum tag stob) addr))

(define (bytes->cells bytes)
  (quotient (+ bytes (- bytes-per-cell 1))
	    bytes-per-cell))

(define (cells->bytes cells)
  (* cells bytes-per-cell))

(define (cells->a-units cells)
  (adjoin-bits cells 0 unused-field-width))

(define (a-units->cells cells)
  (high-bits cells unused-field-width))

(define (bytes->a-units byte-count)
  (cells->a-units (bytes->cells byte-count)))

;; Unicode code points
(define bytes-per-scalar-value-unit 4) ; must be >= 3

(define (bytes->scalar-value-units byte-count)
  (quotient byte-count bytes-per-scalar-value-unit))

(define (scalar-value-units->bytes units)
  (* units bytes-per-scalar-value-unit))
