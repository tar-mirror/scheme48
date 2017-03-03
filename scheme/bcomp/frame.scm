; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Martin Gasbichler, Mike Sperber

; A frame contains information about a procedure's current stack frame.  It
; also has a list of the literals that will go in the procedure's template
; and the debugging data for the template.
;
; template-index - the location of this procedure's template in the frame
;                  (#f if the template is not needed)
; env-index      - the location of this procedure's environment in the frame
;                  (#f if the procedure does not use its environment)
; closure-index  - the location of this procedure's closure in the frame
;                  (#f if the closure is not needed)
; size		 - largest size reached by the frame, in descriptors
; literals	 - list of literals and bindings referenced
; count		 - length of LITERALS
; debug-data	 - debug information (see ddata.scm)

(define-record-type frame :frame
  (really-make-frame literals count debug-data template-index env-index closure-index size)
  frame?
  (template-index frame-template-index)
  (env-index      frame-env-index)
  (closure-index  frame-closure-index)
  (size           frame-size     set-frame-size!)
  (literals       frame-literals set-frame-literals!)
  (count	  frame-count    set-frame-count!)
  (debug-data     frame-debug-data))

; SIZE is the number of values on the stack when the procedure is
; entered (typically the number of arguments).  ENV? is true if the
; environment was pushed on after the arguments, TEMPLATE? is true if
; the template was pushed as well.  CLOSURE? is true if the closure
; was pushed as well.

(define (make-frame parent name size template? env? closure?)
  (let* ((ddata (new-debug-data (adjust-procedure-name name)
				(if parent
				    (frame-debug-data parent)
				    #f))))
    
    (define (allocate-index really?)
      (and really?
	   (let ((index size))
	     (set! size (+ 1 size))
	     index)))

    (let* ((closure-index (allocate-index closure?))
	   (env-index (allocate-index env?))
	   (template-index (allocate-index template?)))

      (really-make-frame '()
			 0
			 ddata
			 template-index env-index closure-index
			 size))))

(define (adjust-procedure-name name)
  (cond ((string? name)			; only files have strings for names
	 (if (keep-file-names?)
	     name
	     #f))
	((and (keep-procedure-names?)
	      (name? name))
	 (name->symbol name))
	(else
	 #f)))

; Convert an index, which is relative to the base of the frame, to an offset
; from the current stack pointer.

(define (index->offset index depth)
  (- depth (+ index 1)))

; Offsets for the template and environment.

(define (template-offset frame depth)
  (if (frame-template-index frame)
      (index->offset (frame-template-index frame)
                     depth)
      #f))

(define (environment-offset frame depth)
  (index->offset (frame-env-index frame)
		 depth))

; Note that FRAME reaches a size of DEPTH.

(define (depth-check! frame depth)
  (if (< (frame-size frame)
	 depth)
      (set-frame-size! frame depth)))

; These two procedures look up bindings and literals in the list of values
; to go in the template.  They're added if not already present.  The returned
; index is that of template, not the frame's list.

(define (binding->index frame binding name assigned?)
  (let loop ((i 0) (l (frame-literals frame)))
    (cond ((null? l)
	   (really-literal->index frame 
				  (make-thingie binding name assigned?)
				  #f))
	  ((and (thingie? (car l))
		(eq? binding (thingie-binding (car l)))
		(eq? name (thingie-name (car l))))
	   (if assigned?
	       (set-thingie-assigned?! (car l) #t))
	   (really-literal->index frame #f i))
	  (else
	   (loop (+ i 1) (cdr l))))))

(define (literal->index frame thing)
  (really-literal->index frame thing
			 (position thing (frame-literals frame))))

(define (really-literal->index frame thing probe)
  (let ((count (frame-count frame)))
    (if probe
	;; +++  Eliminate duplicate entries.
	;; Not necessary, just a modest space saver [how much?].
	;; Measurably slows down compilation.
	;; when 1 thing, lits = (x), count = 1, probe = 0, want 2
	(+ (- count probe)
	   (- template-overhead 1))
	(begin
	  (if (>= count two-byte-limit)
	      (assertion-violation 'literal->index
				   "compiler bug: too many literals"
				   thing))
	  (set-frame-literals! frame
			       (cons thing
				     (frame-literals frame)))
	  (set-frame-count! frame (+ count 1))
	  ;; when 1st thing, count = 0, want 2
	  (+ count template-overhead)))))

(define (position elt list)
  (let loop ((i 0) (l list))
    (cond ((null? l)
	   #f)
	  ((equal? elt (car l))
	   i)
	  (else
	   (loop (+ i 1) (cdr l))))))

