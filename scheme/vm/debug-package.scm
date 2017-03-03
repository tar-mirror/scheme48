; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber

; For debugging

(define-structure vm-disassembler (export disassemble write-instruction)
  (open scheme
	struct
        enumerated              ; enumerand->name
        vm-architecture
	bitwise                 ; arithmetic-shift
	signals)         	; error
  (files (util disasm)))

; Independent byte-code compiler for testing (from Michael Sperber).

(define-structure scan-test scan-interface
  (open scheme-level-2
	packages syntactic
	usual-macros		; for dealing with (usual-transforms ...)
	meta-types
	packages-internal
	simple-signals fluids tables util
	features		;force-output current-noise-port
	filenames)		;translate
  (files (bcomp scan)
	 (bcomp undefined)))

(define-structure expander-test expander-interface
  (open scheme-level-2
	syntactic packages scan-test meta-types reconstruction
	define-record-types
	util simple-signals tables fluids strong
	features)  ; string-hash
  (files (opt expand)
         (opt sort)
	 (opt flatten)))

(define-structure compiler-test compiler-interface
  (open scheme-level-2 syntactic scan-test meta-types
	segments
	reconstruction
	packages
	packages-internal       ;only for structure-package ?
	locations		;make-undefined-location
	architecture
	enumerated		;enumerand->name
	simple-signals tables util fluids 
	features)		;force-output
  (files (bcomp comp)
	 (bcomp cprim)
	 (bcomp ctop)))

(define-interface evaluation-test-interface
  (export test-eval
	  test-load
	  test-load-into
	  test-eval-from-file
	  test-eval-scanned-forms))

(define-structure evaluation-test evaluation-test-interface
  (open scheme-level-2
	compiler-test
	packages			;package-uid
	environments			;package-for-load
	closures			;make-closure
	vm-exposure			;invoke-closure
	scan				;noting-undefined-variables
	i/o				;current-noise-port
	simple-signals fluids)
  (files (rts eval))
  (begin
    (define test-eval eval)
    (define test-load load)
    (define test-load-into load-into)
    (define test-eval-from-file eval-from-file)
    (define test-eval-scanned-forms eval-scanned-forms)))

