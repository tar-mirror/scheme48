; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Mike Sperber


; This configuration file provides alternative implementations of the
; low, run-time, and run-time-internals structures.  Cf. the interface
; definitions in packages.scm.

(define-structure scheme-level-0 scheme-level-0-interface
  (open scheme))

(define-structure escapes escapes-interface ;cf. alt/low-packages.scm
  (open scheme-level-2 define-record-types low-exceptions)
  (files (alt escape)))

(define-structures ((low-level low-level-interface)
		    (source-file-names (export (%file-name% :syntax)))
		    (structure-refs (export (structure-ref :syntax))))
  (open scheme-level-2 low-exceptions)
  (files (alt low)))

(define-structure closures closures-interface
  (open scheme-level-1 records)
  (files (alt closure)))

(define-structure locations locations-interface
  (open scheme-level-2 records)
  (files (alt locations)))

(define-structure loopholes (export (loophole :syntax))
  (open scheme-level-2)
  (files (alt loophole)))

(define-structure silly (export reverse-list->string)
  (open scheme-level-1)
  (files (alt silly)))

(define-structure write-images (export write-image)
  (open scheme-level-2
	tables			;Forward reference
	features bitwise ascii enumerated
	architecture
	templates
	closures
	low-exceptions)
  (files (link data)
	 (link transport)
	 (link write-image)))

; --------------------
; run-time (generally speaking, things needed by the linker)

; Same as in rts-packages.scm:
(define-structure architecture vm-architecture-interface
  (open scheme-level-1 low-exceptions enumerated)
  (files (vm/interp arch)))

(define-structure define-record-types define-record-types-interface
  (open scheme-level-1 records)
  (files (alt jar-defrecord)))

; Same as in rts-packages.scm:
(define-structure enumerated enumerated-interface
  (open scheme-level-1 low-exceptions)
  (files (rts defenum scm)))

(define-structure fluids fluids-interface
  (open scheme-level-1 low-exceptions)
  (files (alt fluid)))

(define-structures ((scheme-level-2 scheme-level-2-interface)
		    (scheme-level-1 scheme-level-1-interface))
  (open scheme))

(define-structure templates templates-interface
  (open scheme-level-1)
  (files (alt template)
	 (rts template)))

(define-structure util util-interface
  (open scheme-level-1)
  (files (rts util)))

(define-structure syntax-rules-data syntax-rules-data-interface
  (open scheme-level-1)
  (files (rts syntax-rules-data)))

(define-structure syntax-rules-apply syntax-rules-apply-interface
  (open scheme-level-1)
  (files (rts syntax-rules-apply)))

(define-structure weak weak-interface
  (open scheme-level-1 low-exceptions)
  (files (alt weak)
	 (rts population)))

(define-structure debug-messages (export debug-message)
  (open scheme-level-2)
  (files (alt debug-message)))

; --------------------
; run-time internals (generally speaking, things not needed by the linker)

; * = mentioned in more-packages.scm
;   conditions
;   continuations
; * exceptions
; * fluids-internal
;   methods
;   meta-methods
;   interrupts
;   low-level
;   more-types
; * number-i/o
; * ports
; * reading
; * records-internal
;   usual-resumer
; * wind
;   writing

(define-structure wind wind-interface
  (open scheme-level-2)
  (files (alt reroot)))

(define-structure continuations continuations-interface
  (open scheme-level-1 templates methods)
  (files (alt contin)
	 (rts continuation))
  (optimize auto-integrate))

; --------------------
; These don't really belong here, but I sure don't know where they
; ought to go.

(define-structure environments (export *structure-ref)
  (open )				;Assume flatloading
  (files (alt environments)))


; Procedure annotations

(define-structure annotations
    (export annotate-procedure procedure-annotation)
  (open scheme-level-1)
  (files (alt annotate)))
