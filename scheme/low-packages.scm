; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Robert Ransom


; Things provided by the byte compiler / VM, together with a few
; things with rather sensitive definitions (low.scm).

(define-structures ((scheme-level-0 scheme-level-0-interface)
		    (primitives primitives-interface)
		    (bitwise bitwise-interface)
		    (closures closures-interface)
		    (byte-vectors byte-vectors-interface)
		    (code-vectors code-vectors-interface)
		    (write-images (export write-image))	;for linker
		    (source-file-names (export (%file-name% :syntax)))
		    (loopholes (export (loophole :syntax)))
		    (low-level low-level-interface)
		    (escapes escapes-interface)
		    (vm-exposure vm-exposure-interface)
		    (all-operators (export all-operators))
		    (ascii ascii-interface)
		    (unicode unicode-interface)
		    (locations locations-interface)
		    (records records-interface)
		    (cells cells-interface)
		    (channels channels-interface)
		    (ports ports-interface)
		    (shared-bindings shared-bindings-interface)
		    (low-proposals low-proposals-interface)
		    (low-exceptions low-exceptions-interface)
		    (low-exceptions-internal low-exceptions-internal-interface)
		    (signal-conditions signal-conditions-interface)
		    (debug-messages (export debug-message))
		    (silly (export reverse-list->string))
		    (code-quotation (export (code-quote :syntax)))
		    (syntax-transformers syntax-transformers-interface)
		    (structure-refs (export (structure-ref :syntax))))
  (define-all-operators)		; Primitive Scheme, in the LSC paper
  (usual-transforms and cond do let let* or)
  (files (rts low)
	 (rts low-exception)
	 (rts low-syntax))
  (optimize auto-integrate))

