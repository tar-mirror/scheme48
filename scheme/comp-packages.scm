; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Robert Ransom, Marcel Turino

; Various data structures used by the compiler, module system, etc.

; Type system

(define-structure meta-types meta-types-interface
  (open scheme-level-2
	define-record-types tables bitwise
	features ;make-immutable!
	util low-exceptions)
  (files (bcomp mtype))
  (optimize auto-integrate))

; Bindings

(define-structure bindings bindings-interface
  (open scheme-level-2
	define-record-types
	meta-types
	locations)
  (files (bcomp binding))
  (optimize auto-integrate))

; Names & Transforms

(define-structures ((names names-interface)
		    (transforms transforms-interface))
  (open scheme-level-2
	define-record-types tables
	low-exceptions
	meta-types	;sexp->type
	bindings	;same-denotation?
	features	;make-immutable! string-hash
	syntax-transformers
	compiler-envs)
  (files (bcomp name)
	 (bcomp transform))
  (optimize auto-integrate))

; A thingie (placecard?) is used to hold a spot for a location that is to be
; found later.  The compiler sticks them in templates and the module system
; later replaces them with locations.
;
; We can't use (BEGIN ...) for this trivial package because it is loaded
; by flatload, which can't handle them.

(define-structure thingies (export make-thingie
				   thingie?
				   thingie-binding
				   thingie-name
				   thingie-assigned?
				   set-thingie-assigned?!)
  (open scheme-level-2 define-record-types)
  (optimize auto-integrate)
  (files (bcomp thingie)))

; Nodes

(define-structure compiler-envs compiler-envs-interface
  (open scheme-level-2 define-record-types
	meta-types bindings)
  (files (bcomp cenv))
  (optimize auto-integrate))

(define-structure nodes nodes-interface
  (open scheme-level-2
	meta-types names packages packages-internal
	compiler-envs bindings transforms
	low-exceptions define-record-types tables
	util)
  (files (bcomp node)
	 (bcomp schemify))
  (optimize auto-integrate))

;--------------------------------
; Macros

(define-structure syntactic syntactic-interface
  (open scheme-level-2 util
	meta-types names bindings
	nodes compiler-envs
	low-exceptions tables fluids
	var-utilities
	transforms
        code-vectors
	features)		;make-immutable!
  (files (bcomp syntax))
  (optimize auto-integrate))

(define-structure syntax-rules-compiler (export compile-rules)
  (open scheme-level-2 (subset util (receive)) names syntax-rules-data)
  (files (bcomp syntax-rules-compiler)))

(define-structure usual-macros usual-macros-interface
  (open scheme-level-2
	names		;name?
	fluids		;used in definition of %file-name%
	code-quotation
	syntax-rules-compiler
	util
	tables 
	low-exceptions
	syntax-transformers)
  (files (bcomp usual)
	 (bcomp syntax-rules)))

; Little utilities to be phased out by changing the format of lambda var lists
; in nodes.

(define-structure var-utilities (export n-ary?
					normalize-formals
					number-of-required-args)
  (open scheme-level-2)
  (files (bcomp var-util))) ; can't use (BEGIN ...) because this is flatloaded

;--------------------------------
; Byte-code compiler

; Lexical environment layout info for debugging

(define-structures ((debug-data debug-data-interface)
		    (debug-data-internal debug-data-internal-interface))
  (open scheme-level-2 
	define-record-types
	tables
	fluids
	record-types		;for debug-flags randomness
	features)		;make-immutable!
  (files (bcomp ddata)
	 (bcomp state))
  (optimize auto-integrate))

; Determining stack usage.  No longer used.
;
;(define-structure stack-check (export maximum-stack-use)
;  (open scheme-level-2 architecture code-vectors low-exceptions)
;  (files (bcomp stack-check))
;  (optimize auto-integrate))

; Compiler back end

(define-structure segments segments-interface
  (open scheme-level-2 util tables low-exceptions fluids
	define-record-types
	bitwise vm-data
	code-vectors
	templates
	architecture
	features		;make-immutable!
	debug-data debug-data-internal
	frames)
  (files (bcomp segment))
  (optimize auto-integrate))

; Primops

(define-structure primops primops-interface
  (open scheme-level-2 tables define-record-types
	meta-types
	low-exceptions)
  (files (bcomp primop))
  (optimize auto-integrate))

; Type reconstruction.

(define-structure reconstruction (export node-type reconstruct-type)
  (open scheme-level-2 tables
	meta-types nodes names bindings
	primops
	var-utilities		;n-ary?
	util			;last
	low-exceptions)
  (files (bcomp recon)))

; The compiler itself.

(define-structures ((compiler compiler-interface)
		    (bc-generation bc-generation-interface))
  (open scheme-level-2 util low-exceptions
	features		;force-output
	enumerated		;enumerand->name
	ascii
	architecture
	meta-types names bindings
	transforms
	nodes var-utilities
	primops
	segments
	debug-data-internal	; keep-source-code?
	flat-environments
	frames
	reconstruction)
  (files (bcomp comp-exp)
	 (bcomp comp-lambda)
	 (bcomp comp-prim)
	 (bcomp comp))
  (optimize auto-integrate))

(define-structure frames frames-interface
  (open scheme-level-2
	define-record-types
	names
	architecture			; two-byte-limit
	templates			; template-overhead
	debug-data-internal		; new-debug-data
	low-exceptions			; error
	thingies)
  (files (bcomp frame))
  (optimize auto-integrate))

;----------------
; Reading the forms in a file.
; This is used by scan-package and rts/eval.scm.

(define-structure reading-forms (export read-forms $note-file-package)
  (open scheme-level-2
	fluids filenames cells
	features		;current-noise-port force-output
	low-exceptions          ;error
	(subset packages-internal (package-reader))
	)
  (files (bcomp read-form)))

;----------------
; Live-variable analysis for closures.

(define-structure flat-environments (export flatten-form)
  (open scheme-level-2 nodes low-exceptions
	optimizer primops
	util			;every
	var-utilities)
  (files (bcomp flatten)))

;----------------
; Module system

(define-structure interfaces interfaces-interface
  (open scheme-level-2
	define-record-types tables
	util			;filter every receive symbol-append
	low-exceptions		;error
	weak			;populations
	meta-types)
  (files (bcomp interface))
  (optimize auto-integrate))

(define-structures ((packages packages-interface)
		    (packages-internal packages-internal-interface)
		    (undefined undefined-interface))
  (open scheme-level-2
	define-record-types tables fluids low-exceptions cells
	util features locations weak
        meta-types interfaces
	names bindings
	compiler-envs
	templates
	thingies)
  (files (bcomp package)
	 (bcomp package-undef))
  (optimize auto-integrate))

(define-structure scan-package scan-package-interface
  (open scheme-level-2 util
	packages packages-internal
	meta-types bindings
	compiler-envs
	reading-forms
	filenames
	low-exceptions
	features		;current-noise-port force-output
	)
  (files (bcomp scan-package)))

(define-structure optimizer optimizer-interface
  (open scheme-level-2
	low-exceptions tables
	util)
  (files (bcomp optimize)))

(define-structure compile-packages (export compile-package)
  (open scheme-level-2 util tables
	syntactic
	packages
	packages-internal	;package-name
	optimizer
	compiler
	primops			;walk-primops
	compiler-envs
	nodes
	scan-package
	usual-macros		;for usual-transforms
	transforms		;for usual-transforms
	meta-types)		;for usual-transforms and define-all-operators
  (files (bcomp comp-package)))

;----------------
; DEFINE-STRUCTURE and friends

(define-structure defpackage defpackage-interface
  (open scheme-level-2
	packages 
	(subset packages-internal (set-package-reader!))
	syntactic usual-macros types
	interfaces
	source-file-names	;%file-name%
	low-exceptions		;error
	tables)
  (files (bcomp module-language)
	 (bcomp config)))

(define-structure types types-interface  ;Typing language
  (open scheme-level-2 meta-types syntactic loopholes)
  (files (bcomp type))
  ;; (optimize auto-integrate)  - doesn't work
  )

(define-structure module-system (compound-interface defpackage-interface
						    types-interface)
  (open defpackage types))

;----------------
; Code analysis and inlining

(define-structure usages usages-interface
  (open scheme-level-2
	meta-types names nodes
	packages
	packages-internal	;package-refine-type!
	reconstruction
	var-utilities
	define-record-types
	util low-exceptions tables strong)
  (files (opt usage)
         (opt sort)))

(define-structure analysis (export analyze-forms)
  (open scheme-level-2
	meta-types bindings nodes primops
	packages-internal	;package-add-static!
	inline
	usages
	reconstruction
	var-utilities
	transforms
	syntactic               ;static-value
	packages
	low-exceptions
	features		;force-output
	optimizer		;set-optimizer!
	util)
  (files (opt analyze)))

(define-structure inline inline-interface
  (open scheme-level-2 util var-utilities
	meta-types names bindings nodes
	compiler-envs
	transforms
	packages
	usages
	low-exceptions)
  (files (opt inline)))

(define-structure strong (export strongly-connected-components)
  (open scheme-level-2 define-record-types low-exceptions)
  (files (big strong))) ;!


;----------------
; Two basic structures needed to support the compiler.

(define-structure tables general-tables-interface
  (open scheme-level-1
	define-record-types
	low-exceptions
	features)    ; string-hash, make-immutable!
  (files (big general-table))
  (optimize auto-integrate))

(define-structure filenames filenames-interface
  (open scheme-level-1 low-exceptions fluids cells)
  (files (big filename)))
