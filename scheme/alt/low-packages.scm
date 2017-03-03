; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber

; Alternate implementations of the low-structures.
; Cf. low-structures-interface in ../packages.scm and ../alt-structures.scm.

; Most of the low-structures are assumed to be inherited or obtained
; elsewhere (probably from a running Scheme 48).  This only defines
; structures that export privileged operations.

(define-structure escapes escapes-interface
  (open scheme-level-2 define-record-types low-exceptions)
  (files escape))

(define-structures ((primitives primitives-interface)
		    (primitives-internal (export maybe-handle-interrupt
						 raise-exception
						 get-exception-handler
						 ?start)))
  (open scheme-level-2
	define-record-types
	bitwise	    ;Only for re-export
	features    ;Only for re-export
	low-exceptions
	;; templates -- unneeded now?
	)
  (files primitives
	 weak
	 contin))

; NB: Our syntax-rules implementation really wants a quote here that
; preserves sharing.  Scheme 48 doesn't traditionally do this.
(define-structure code-quotation (export (code-quote :syntax))
  (open scheme-level-2)
  (begin
    (define-syntax code-quote
      (lambda (e r c)
	e)))) ; this version should never get called

(define-structure syntax-transformers syntax-transformers-interface
  (open scheme-level-2)
  (files "../rts/low-syntax"))
