; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Marcus Crestani


; Static linker.  Doesn't work very well this way (debug info is
; screwed up), so it's probably better to continue using linker.image
; instead.

(define-structure linker linker-interface
  (open scheme-level-2
	compiler write-images
	platform
	debuginfo
	segments		;with-fresh-compiler-state
	packages
	packages-internal       ;set-package-integrate?!
	names			;name->symbol
	scan-package		;collect-packages
	compile-packages	;compile-package
	reification
	closures		;make closure to pass to write-image
	filenames
	debug-data		;with-fresh-compiler-state
	locations
	tables fluids exceptions util)
  (files (link link)))

(define-structure reification (export reify-structures)
  (open scheme-level-2
	packages
	packages-internal ;?
	usual-macros		;find-free-names-in-syntax-rules
	names bindings
	meta-types		;usual-variable-type
	locations
	primops
	tables records
	exceptions		;error
	features		;force-output
	util			;filter
	inline)			;name->extrinsic
  (files (link reify)))

; Database of procedure names 'n' stuff.
; (copy in env-packages.scm ...)

(define-structure debuginfo debuginfo-interface
  (open scheme-level-2
	tables
	debug-data
	packages
	packages-internal
	syntactic
	segments	;debug-data-table
	features)       ;make-immutable!
  (files (env debuginfo)))


; Mumble.

(define-structure flatloading (export flatload all-file-names)
  (open scheme packages packages-internal filenames)
  (files (env flatload)))

(define-structure loadc (export load-configuration
				;; (structure-ref :syntax)
				)
  (open scheme
	environments		; *structure-ref
	syntactic		; $source-file-name
	fluids)
  (files (link loadc)))


; Everything.

(define-structure link-config (export )  ;dummy structure...
  (open scheme-level-2
	linker
	names ; name?
	syntax-rules-apply
	defpackage
	types
	analysis
	loadc
	flatloading
	interfaces
	exceptions)    ;warning
  ;; (files (alt init-defpackage.scm))  -- or (env ...), depending
  )
