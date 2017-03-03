; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Michael Zabka,
; Robert Ransom, Marcel Turino, Manuel Dietrich, Marcus Crestani,
; Harald Glab-Phlak


; More and more packages.  Some of these get loaded into the initial
; image to create scheme48.image; those that aren't can be loaded later
; using ,load-package.

; Things to load into initial.image to make scheme48.image.

(define-structure usual-features (export )  ;No exports
  (open analysis		;auto-integration
        command-processor
        debuginfo
	disclosers
        floatnums
	more-vm-exceptions
	;; pp
	;; Choose either innums, floatnums, or neither
	;; innums			;Silly inexact numbers
	;; bignums		; now in the VM
	;; Choose any combination of bignums, ratnums, recnums
	ratnums recnums
	;; The following is listed because this structure is used to
	;; generate a dependency list used by the Makefile...
	usual-commands
	unicode-char-maps
	))

; Large integers and rational and complex numbers.

(define-structure extended-numbers extended-numbers-interface
  (open scheme-level-2
        methods meta-methods
        define-record-types
        primitives
        architecture
	exceptions
	(subset vm-exceptions (extend-opcode!))
	util
        number-i/o)
  (files (rts xnum)))

(define-structure innums (export )    ;inexact numbers
  (open scheme-level-2
        extended-numbers
        methods exceptions
        number-i/o)             ;string->integer
  (files (rts innum)))

(define-structure ratnums (export )    ;No exports
  (open scheme-level-2
        extended-numbers
        methods exceptions
        number-i/o)             ;string->integer
  (files (rts ratnum)))

(define-structure recnums (export )    ;No exports
  (open scheme-level-2
        extended-numbers
        methods exceptions
        number-i/o)             ;really-number->string
  (files (rts recnum)))

(define-structure floatnums
  (export floatnum? exp log sin cos tan asin acos atan sqrt)
  (open scheme-level-2
        extended-numbers
        code-vectors
        methods exceptions
	enumerated
	loopholes
	more-types		;<double>
        primitives)             ;vm-extension double?
  (files (rts floatnum))
  (optimize auto-integrate))

(define-structure unicode-char-maps unicode-char-maps-interface
  (open (modify scheme (hide string-ci=? string-ci<?))
	set-text-procedures
	unicode
	finite-types
	define-record-types
	tables
	bitwise)
  (files (env unicode-category)
	 (env unicode-info)
	 (env unicode-charmap)))

(define-structure time time-interface
  (open scheme-level-1 primitives architecture enumerated)
  (begin
    (define (real-time)
      (time (enum time-option real-time) #f))

    (define (run-time)
      (time (enum time-option run-time) #f))))

(define-structure placeholders placeholder-interface
  (open scheme-level-1 proposals queues
	(subset util (unspecific))
	threads threads-internal
	interrupts
	exceptions)
  (files (big placeholder))
  (optimize auto-integrate))

(define-structure locks locks-interface
  (open scheme-level-2 queues
	threads threads-internal
	interrupts
	proposals)
  (optimize auto-integrate)
  (files (big lock)))

;--------
; Unicode

(define-structure text-codec-utils text-codec-utils-interface
  (open scheme-level-2
	ports
	i/o
	text-codecs)
  (files (big text-codec-util)))

(define-structure unicode-normalizations unicode-normalizations-interface
  (open scheme
	unicode
	bitwise)
  (files (big unicode-normalization-info)
	 (big unicode-normalization)))

; --------------------
; Transport Link Cell Tables

(define-structure tconc-queues tconc-queues-interface
  (open scheme-level-1 exceptions)
  (files (big tconc-queue))
  (optimize auto-integrate))

(define-structure tlc-tables tlc-tables-interface
  (open scheme-level-1 
        exceptions
	features  ; string-hash, make-immutable!
        define-record-types
        tconc-queues
	unicode-char-maps
        tables
        variable-argument-lists
        (subset primitives   (make-transport-link-cell
                              transport-link-cell?
                              transport-link-cell-key
                              transport-link-cell-value
                              set-transport-link-cell-value!
                              transport-link-cell-next
                              set-transport-link-cell-next!
                              transport-link-cell-tconc
                              set-transport-link-cell-tconc!
                              memory-status))
        (subset architecture (memory-status-option))
        enumerated)
  (files (big tlc-table))
  (optimize auto-integrate))

; --------------------
; Standards 

(define-structure r5rs r5rs-interface
  (open scheme))

;----------------
; Big Scheme

(define-structure random (export make-random)
  (open scheme-level-2 bitwise
	exceptions)
  (files (big random)))

(define-structure sort (export sort-list sort-list!)
  (open scheme-level-2
	vector-heap-sort list-merge-sort)
  (begin
    (define (sort-list l obj-<)
      (let ((v (list->vector l)))
	(vector-heap-sort! obj-< v)
	(vector->list v)))
    (define (sort-list! l obj-<)
      (list-merge-sort! obj-< l))))

(define-structure pp (export p pretty-print define-indentation)
  (open scheme-level-2
        tables
        (subset methods (disclose)))
  (files (big pp)))

(define-structure formats (export format)
  (open scheme-level-2 ascii exceptions
	extended-ports)
  (files (big format)))

(define-structure extended-ports extended-ports-interface
  (open scheme-level-2 define-record-types ascii byte-vectors
	ports
	i/o i/o-internal
	proposals
	util				; unspecific
	exceptions
	(subset primitives      (copy-bytes! write-byte char->utf utf->char))
	(subset architecture    (text-encoding-option))
	enumerated
	encodings
	(subset text-codecs
		(set-port-text-codec! utf-8-codec define-text-codec)))
  (files (big more-port)))

(define-structure destructuring (export (destructure :syntax))
  (open scheme-level-2)
  (files (big destructure)))

(define-structure mvlet (export ((mvlet mvlet*) :syntax))
  (open scheme-level-2)
  (files (big mvlet)))

(define-structure reduce (export ((reduce iterate)
				  :syntax)
				 ((list* list%
					 list-spine* list-spine%
					 list-spine-cycle-safe*
					 list-spine-cycle-safe%
					 vector* vector%
					 string* string%
					 count* count%
					 bits* bits%
					 input* input%
					 stream* stream%)
				  :syntax))
  (open scheme-level-2
	bitwise
	exceptions)
  (files (big iterate)))

(define-structure arrays arrays-interface
  (open scheme-level-2 define-record-types exceptions)
  (files (big array)))

(define-structure lu-decompositions lu-decompositions-interface
  (open scheme receiving arrays floatnums exceptions)
  (files (big lu-decomp)))

(define-structure compact-tables compact-tables-interface
  (open scheme)
  (files (big compact-table)))

(define-structure inversion-lists inversion-lists-interface
  (open scheme
	bitwise
	define-record-types
	exceptions)
  (files (big inversion-list)))

(define-structure constant-tables constant-tables-interface
  (open scheme
	bitwise
	define-record-types)
  (files (big constant-table)))

(define-structure receiving (export (receive :syntax))
  (open scheme-level-2
	util))

(define-structure defrecord defrecord-interface
  (open scheme-level-1 records record-types loopholes
	primitives)			; unspecific, low-level record ops
  (files (big defrecord)))

(define-structures ((masks masks-interface)
		    (mask-types mask-types-interface))
  (open scheme-level-1 define-record-types
	bitwise
	util			; every
	number-i/o		; number->string
	exceptions)		; assertion-violation
  (files (big mask)))

(define-structures ((enum-sets enum-sets-interface)
		    (enum-sets-internal enum-sets-internal-interface))
  (open scheme define-record-types
	finite-types
	bitwise 
	util
	exceptions
	external-calls)
  (optimize auto-integrate)
  (files (big enum-set)))

(define general-tables tables)    ; backward compatibility

(define-structure big-util big-util-interface
  (open scheme-level-2
	formats
	features		; immutable? make-immutable!
	(modify exceptions
		(rename (error rts-error))
		(expose error assertion-violation))
	(modify debugging	(rename (breakpoint rts-breakpoint))
		                (expose breakpoint))
	(subset primitives	(copy-bytes!))
	(subset util (filter)))
  (files (big big-util)))

(define-structure big-scheme big-scheme-interface
  (open scheme-level-2
	formats
	sort
        extended-ports
	pp
	enumerated
        bitwise
        ascii
	big-util
        tables
        destructuring
        receiving))

; Things needed for connecting with external code.

(define-structure external-calls (export call-imported-binding
					 call-imported-binding-2
					 lookup-imported-binding
					 define-exported-binding
					 shared-binding-ref
					 ((import-definition
					   import-lambda-definition
					   import-lambda-definition-2)
					  :syntax)
					 add-finalizer!
					 define-record-resumer
					 call-external-value
					 call-external-value-2)
  (open scheme-level-2 define-record-types
	primitives
	os-strings
        architecture ; includes ENUM
	enum-case
	vm-exceptions interrupts exceptions conditions
	placeholders
	shared-bindings
	byte-vectors
					;bitwise		;for {enter|extract}_integer() helpers
	(subset record-types		(define-record-resumer))
	(subset records-internal	(:record-type)))
  (files (big import-def)
	 (big callback)))

(define-structure shared-objects shared-objects-interface
  (open scheme-level-2
	define-record-types
	exceptions
	external-calls
	os-strings text-codecs)
  (files (big shared-object)))

(define-structure load-dynamic-externals load-dynamic-externals-interface
  (open scheme-level-2
	define-record-types
	shared-objects
	(subset usual-resumer (add-initialization-thunk!))
	(subset big-util (delq delete any))
	filenames
	(subset exceptions (assertion-violation)))
  (files (big dynamic-external)))

(define-structure c-system-function (export have-system? system)
  (open scheme-level-2 byte-vectors os-strings external-calls exceptions)
  (begin
    (import-lambda-definition-2 s48-system (string) "s48_system_2")

    (define (have-system?)
      (not (= 0 (s48-system #f))))

    ;; Kludge
    (define (system cmd-line)
      (s48-system (x->os-byte-vector cmd-line)))))
    
; Rudimentary object dump and restore

(define-structure dump/restore dump/restore-interface
  (open scheme-level-1
        number-i/o
        tables
        records record-types
        exceptions          	;error
        locations               ;make-undefined-location
        closures
        code-vectors            ;code vectors
        fluids
        ascii
        bitwise
        (subset methods (disclose))
        templates)              ;template-info
  (files (big dump)))

; Pipes containing values.

(define-structure value-pipes value-pipes-interface
  (open scheme queues
        proposals
        threads-internal
	exceptions)		;assertion-violation
  (optimize auto-integrate)
  (files (big value-pipe)))

; Heap traverser

(define-structure traverse
  (export traverse-depth-first traverse-breadth-first trail
	  set-leaf-predicate! usual-leaf-predicate)
  (open scheme-level-2
	primitives
        queues tables
        bitwise locations closures code-vectors
        features                ; string-hash
        low-level               ; vector-unassigned?
	more-types loopholes)
  (files (env traverse)))

; Reinitializing upon image resumption

(define-structure reinitializers reinitializers-interface
  (open scheme-level-2
	define-record-types
	(subset record-types (define-record-resumer)))
  (files (big reinitializer)))

; Profiler.

(define-structure profiler profiler-interface
  (open scheme
	architecture
	cells
	closures
	continuations
	debug-data
	debugging
	define-record-types
	disclosers
	environments
	escapes
	interrupts
	locks
	exceptions
	(modify primitives (prefix primitives:)
		(expose collect time memory-status
			continuation-length continuation-ref
			unspecific))
	session-data
	sort
	tables
	templates
	command-processor
	)
  (files (env profile)))

(define-structure profile-commands (export)
  (open scheme
	command-processor
	profiler
	profiler-instrumentation ; make sure it gets loaded
	(subset environments (environment-define!)))
  (files (env profile-command)))

(define-structure profiler-instrumentation (export instrument-form)
  (open scheme
	bindings
	compiler-envs
	environments
	features
	exceptions
	nodes
	optimizer
	package-commands-internal
	packages
	packages-internal
	primops
	profiler
	util)
  (files (env profile-instr)))

; Space analyzer

(define-structure spatial (export space vector-space record-space)
  (open scheme
	architecture primitives assembler packages enumerated 
	features sort locations display-conditions)
  (files (env space)))

; Listing what is in an interface.  Here because it needs sort.

(define-structure list-interfaces (export list-interface)
  (open scheme-level-2 interfaces packages meta-types sort bindings)
  (files (env list-interface)))

; red-black balanced binary search trees

(define-structure search-trees search-trees-interface
  (open scheme-level-2 define-record-types)
  (optimize auto-integrate)
  (files (big search-tree)))

; vectors that grow as large as they need to

(define-structure sparse-vectors sparse-vectors-interface
  (open scheme
	bitwise
	define-record-types)
  (files (big hilbert)))

; utilities for dealing with variable argument lists

(define-structure variable-argument-lists variable-argument-lists-interface
  (open scheme-level-2)
  (files (big vararg)))

; record types with a fixed number of instances

(define-structure finite-types (export ((define-finite-type
					 define-enumerated-type) :syntax))
  (open scheme-level-2 code-quotation define-record-types
	enumerated
	features)		; make-immutable
  (files (big finite-type)))

; nondeterminism via call/cc

(define-structure nondeterminism (export with-nondeterminism
					 ((either one-value all-values) :syntax)
					 fail)
  (open scheme-level-2
	fluids cells
	exceptions
	(subset exceptions (error)))
  (files (big either)))

; test suites

(define-structure matchers matchers-interface
  (open scheme
	define-record-types
	big-util)
  (files (big matcher)))

(define-structure test-suites test-suites-interface
  (open scheme
	cells
	(subset big-util (any delete))
	matchers
	exceptions
	define-record-types
	exceptions conditions
	display-conditions
	escapes continuations previews
	(subset i/o (current-error-port))
        (subset i/o-internal (output-port-forcers))
	fluids)
  (files (big test-suite)))

(define-structure libscheme48 (export dump-libscheme48-image)
  (open scheme
 	(subset escapes (with-continuation))
	build)
  (files (big libscheme48)))

;----------------
; Obsolete packages

; Bignums and bitwise logical operators on bignums.  These are now handled
; by the VM.   These packages are here to keep from breaking scripts that
; load them.  They will be removed in a later release.

(define-structure bignums (export)
  (open scheme-level-2))

(define-structure bigbit (export)
  (open scheme-level-2))

; The old signals

(define-structure signals signals-interface
  (open scheme-level-2
	signal-conditions
	conditions)
  (files (big signal)))

; ... end of package definitions.

; Temporary compatibility stuff
(define-syntax define-signature
  (syntax-rules () ((define-signature . ?rest) (define-interface . ?rest))))
(define-syntax define-package
  (syntax-rules () ((define-package . ?rest) (define-structures . ?rest))))
(define table tables)
(define record records)
; It used to be called `code-quote', so this is the name the linker imports.
(define code-quote code-quotation)

; Time
(define-interface os-time-interface
  (export current-utc-time
          timezone-offset
          time-seconds
          time-microseconds
          time?))

(define-structure os-time os-time-interface
  (open scheme
        define-record-types
        os-strings
	external-calls
        shared-bindings)
  (files (big os-time)))
