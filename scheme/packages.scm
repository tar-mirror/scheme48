; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Michael Zabka,
; Robert Ransom, Marcus Crestani, Ivan Shmakov, Martin Gasbichler,
; David van Horn, Harald Glab-Phlak


; Meta-modules: the big picture.

; Various implementations of Primitive Scheme.

(define-structure low-structures low-structures-interface
  (open meta-module-system the-interfaces)
  (files low-packages))

(define-structure debug-low-structures low-structures-interface
  (open meta-module-system the-interfaces
	;; built-in-structures
	)
  (files (alt low-packages)))


; Usual Scheme 48 run-time system.

(define (make-run-time low-structures)
  (structures (run-time-structures-interface
	       run-time-internals-structures-interface
	       features-structures-interface)
    (open meta-module-system the-interfaces
	  low-structures)
    (files rts-packages)))


; Alternatives to the usual Scheme 48 run-time system.

(define-structure alt-features-structures features-structures-interface
  (open meta-module-system the-interfaces)
  (files (alt features-packages)))

(define-structure cheat-features-structures features-structures-interface
  (open meta-module-system the-interfaces)
  (begin (define-structures ((ascii ascii-interface)
			     (bitwise bitwise-interface)
			     (code-vectors code-vectors-interface)
			     (features features-interface)
			     (records records-interface)
			     (record-types record-types-interface)
			     (low-exceptions low-exceptions-interface))
	   ;; Assumes use of FLATLOAD.  The implementations of these
	   ;; structures will become available via some miracle, e.g.
	   ;; a command ",open low-exceptions ... code-vectors" or an
	   ;; explicit LOAD of something.  Cf. the rule for
	   ;; link/linker.image in the Makefile.
	   )))

(define-module (make-alt-run-time features-structures)
  (structures (low-structures-interface
	       run-time-structures-interface)
    (open meta-module-system the-interfaces
	  features-structures)
    (files alt-packages
	   (alt low-packages))))


; Byte-code compiler and related things.

(define-module (make-compiler-structures run-time-structures
					 features-structures)

  (define-structure compiler-structures compiler-structures-interface
    (open meta-module-system the-interfaces
	  run-time-structures
	  features-structures)
    (files comp-packages))

  compiler-structures)


; The initial system (initial.image).  Cf. the rule for initial.image
; in the Makefile.

(define (make-initial-structures low-structures
				 run-time-structures
				 run-time-internals-structures
				 features-structures
				 compiler-structures)
  (structure initial-structures-interface
    (open meta-module-system the-interfaces
	  low-structures		;Cf. desirable-structures
	  run-time-structures
	  features-structures
	  run-time-internals-structures
	  compiler-structures)
    (files initial-packages)))


; Small systems.

(define-structure (make-debug-structures low-structures
					 run-time-structures
					 run-time-internals-structures
					 features-structures
					 initial-structures)
  (structure debug-structures-interface
    (open meta-module-system the-interfaces
	  low-structures
	  run-time-structures
	  run-time-internals-structures
	  features-structures
	  initial-structures)
    (files debug-packages)))


; Static linker.

(define-module (make-linker-structures features-structures
				       run-time-structures
				       compiler-structures)

  (define-structure linker-structures linker-structures-interface
    (open meta-module-system the-interfaces
	  features-structures
	  run-time-structures
	  compiler-structures)
    (files link-packages))

  linker-structures)



; The following definition of THE-INTERFACES assumes that we're
; "flatloading."  If we were really using the module system, then its
; interface would have to include all of the interfaces defined in
; interfaces.scm, and it would need a (files interfaces) clause.

(define-structure the-interfaces the-interfaces-interface
  (open )
  ;; (files interfaces)
  )
(define-interface the-interfaces-interface
  (export scheme-level-0-interface
	  primitives-interface
	  ;; ... etc. etc. ad nauseum
	  for-reification-interface))

; This definition of META-MODULE-SYSTEM assumes that we're flatloading.
; If we weren't, it would have to be
;   (def meta-module-system module-system)
; instead.

(define-structure meta-module-system (export ) (open ))  ;Kludge



; --------------------
; Particular assemblies:

; The usual run-time system (for initial.image, etc.).

(def run-time-structures run-time-internals-structures features-structures
  (make-run-time low-structures))


; The byte-code compiler as constituted for initial.image and friends.

(def compiler-structures
  (make-compiler-structures run-time-structures
			    features-structures))


; The initial system made in the usual way.

(def initial-structures
  (make-initial-structures low-structures
			   run-time-structures
			   run-time-internals-structures
			   features-structures
			   compiler-structures))


; Debug systems.

(def debug-structures
  (make-debug-structures low-structures
			 run-time-structures
			 run-time-internals-structures
			 features-structures
			 initial-structures))


; The usual development environment (scheme48.image).

(define-structure usual-structures (export (usual-features :structure))
  (open meta-module-system
	run-time-structures
	compiler-structures
	initial-structures
	(make-linker-structures features-structures
				run-time-structures
				compiler-structures))
  (files ;; more-interfaces, when not flatloading
	 ;; (sort interfaces), when not flatloading
	 (sort packages)
         env-packages
	 more-packages))


; The linker running in a random Scheme system (Lucid, Scheme->C, or
; old version of Scheme 48).  If running in Scheme 48, this becomes
; link/linker.image.

(def alt-low-structures alt-run-time-structures
  (make-alt-run-time cheat-features-structures))

(def linker-structures
  (make-linker-structures cheat-features-structures
			  alt-run-time-structures
			  (make-compiler-structures cheat-features-structures
						    alt-run-time-structures)))



; --------------------
; Meta-interfaces.
; These are ignored by FLATLOAD, but DESIRABLE-STRUCTURES (in
; initial.scm) extracts the list of structures to be reified from
; them.

(define-interface features-structures-interface
  (export ((ascii
	    bitwise
	    code-vectors
	    features
	    records)
	   :structure)))

(define-interface low-structures-interface
  (export ((all-operators
	    ascii
	    bitwise
	    byte-vectors
	    cells
	    code-vectors
	    features
	    records
	    cells
	    channels
	    closures
	    code-quotation
	    escapes
	    locations
	    loopholes
	    low-level
	    ports
	    primitives
	    low-proposals
	    scheme-level-0
	    shared-bindings
	    signal-conditions
	    silly
	    source-file-names
	    structure-refs
	    debug-messages
	    syntax-transformers
	    unicode
	    vm-exposure
	    write-images)
	   :structure)))

(define-interface run-time-structures-interface
  (export ((architecture
	    channel-i/o
	    condvars
	    define-record-types
	    encodings
	    enum-case
	    enumerated
	    fluids
	    os-strings
	    proposals
	    queues
	    record-types
	    scheduler
	    scheme-level-1
	    scheme-level-2
	    set-text-procedures
	    templates
	    text-codecs
	    threads
	    util
	    vm-data
	    weak)
	   :structure)))

(define-interface run-time-internals-structures-interface
  (export ((channel-ports
	    conditions
	    continuations
	    ;; escapes
	    exceptions
	    exceptions-internal
	    fluids-internal
	    handle
	    i/o
	    i/o-internal
	    methods
	    meta-methods
	    interrupts
	    external-events
	    low-level
	    more-types
	    number-i/o
	    ;; primitives
	    queues-internal
	    reading
	    records-internal
	    root-scheduler
	    session-data
	    syntax-rules-apply
	    threads-internal
	    vm-exceptions
	    usual-resumer
	    ;; silly
	    ;; structure-refs
	    ;; vm-exposure
	    wind
	    writing)
	   :structure)))
  
(define-interface compiler-structures-interface
  (export ((analysis
	    bc-generation
	    bindings
	    compiler
	    compiler-envs
	    compile-packages
	    debug-data
	    debug-data-internal
	    defpackage
	    filenames
	    flat-environments
	    frames
	    inline
	    meta-types
	    interfaces
	    module-system
	    names
	    nodes
	    optimizer
	    packages
	    packages-internal
	    primops
	    reading-forms
	    reconstruction
	    segments
	    scan-package
	    syntactic
	    strong
	    tables
	    transforms
	    types
	    undefined
	    usual-macros)
	   :structure)))

(define-interface initial-structures-interface
  (export ((environments
	    load-filenames
	    evaluation
	    ensures-loaded
	    ;; for-reification is in there, but shouldn't get reified.
	    )
	   :structure)
	  ((make-scheme
	    make-mini-command
	    make-initial-system)
	   :procedure)))


; Initial + usual (scheme48.image).

(define-interface linker-structures-interface
  (export ((analysis
	    debuginfo
	    expander
	    flatloading
	    linker
	    link-config
	    loadc
	    reification
	    usages)
	   :structure)))

(define debug-structures-interface
  (export ((mini-eval
	    mini-environments
	    mini-scheme
	    little-system
	    mini-for-reification
	    mini-packages
	    mini-system
	    run
	    medium-scheme
	    medium-system)
	   :structure)
	  mini-eval
	  mini-environments))

; Must list all the packages in the various package files that are to
; be visible in the command processor's config package.

(define-interface more-structures-interface
  (export ((more-structures
	    usual-features
	    arrays
	    assembler
	    assembling
	    general-tables
	    bigbit
	    bignums ratnums recnums floatnums
	    build
	    callback
	    code-quote ; compatibility to earlier versions
	    command-levels
	    command-processor
	    command-state
	    usual-commands
	    compact-tables
	    constant-tables
	    conditions
	    c-system-function
	    debugging
	    define-record-types
	    defrecord
	    destructuring
	    disassembler
	    disclosers
	    display-conditions
	    dump/restore
	    dynamic-externals
	    enum-case
	    enum-sets enum-sets-internal
	    extended-numbers
	    extended-ports
	    externals
	    external-calls
	    finite-types
	    floatnums
	    formats
	    inspector
	    inspector-internal
	    inversion-lists
	    list-interfaces
	    load-dynamic-externals
	    locks
	    lu-decompositions
	    masks
	    mask-types
	    mvlet
	    nondeterminism
	    net-addresses
	    net-sockets
            os-time
	    package-commands-internal
	    package-mutation
	    parse-bytecode
	    placeholders
	    pp
	    previews
	    profiler
	    profile-commands
	    profiler-instrumentation
	    profiler-internals
	    queues
	    shared-objects
	    tconc-queues
	    time
	    tlc-tables
	    random
	    receiving
	    reduce
	    search-trees
	    signals
	    sockets
	    unicode-char-maps

	    delete-neighbor-duplicates
	    binary-searches
	    sorted
	    list-merge-sort
	    vector-merge-sort
	    vector-heap-sort
	    vector-insertion-sort
	    vector-quick-sort vector-quick-sort3
	    sorting
	    sort

	    sparse-vectors
	    reinitializers
	    signals
	    spatial
	    strong
	    text-codec-utils
	    traverse
	    udp-sockets
	    unicode-normalizations
	    value-pipes
	    variable-argument-lists

	    big-scheme
	    big-util
	    ;; From link-packages.scm:
	    analysis
	    debuginfo
	    expander
	    flatloading
	    linker
	    link-config
	    reification			;?
	    shadowing
	    ;; Compatibility
	    record table

	    ; CML packages (see scheme/cml/packages.scm)
	    rendezvous
	    rendezvous-channels
	    rendezvous-async-channels
	    rendezvous-placeholders
	    rendezvous-jars
	    rendezvous-time
	    ; do-it-yourself
	    make-rendezvous
	    trans-ids

	    r5rs

	    ; R6RS packages

	    r6rs-base-comparisons
	    r6rs-bitwise
	    r6rs-conditions
	    r6rs-records-procedural
	    r6rs-records-inspection
	    r6rs-records-internal
	    r6rs-records-commands
	    r6rs-records-syntactic
	    r6rs-records-syntactic-internal
	    r6rs-unicode
	    r6rs-lists
	    r6rs-enums
	    r6rs-sorting
	    r6rs-reader
	    r6rs-reader-command
	    r6rs-reader-internals
            r6rs-equal
            r6rs-control
	    r6rs-bytevectors
	    r6rs-hashtables

	    ; POSIX packages (see scheme/posix/packages.scm)
	    posix-files
	    posix-time
	    posix-users
	    posix-process-data
	    posix-platform-names
	    posix-processes
	    posix-regexps
	    posix-i/o
	    posix-errnos
	    posix-syslog
	    regexps regexps-internal
	    posix

	    ; SRFI packages
	    srfi-1 srfi-2 srfi-4 srfi-5 srfi-6 srfi-7 srfi-8 srfi-9
	    srfi-11 srfi-13 srfi-14 srfi-16 srfi-17 srfi-19
	    srfi-23 srfi-25 srfi-26 srfi-27 srfi-28
	    srfi-31 srfi-34 srfi-37
	    srfi-39 srfi-40 srfi-42 srfi-43 srfi-45
            srfi-60 srfi-61 srfi-62 srfi-63 srfi-66 srfi-67
	    srfi-71 srfi-74 srfi-78 srfi-95

            libscheme48
	    test-suites
	    matchers
	    )
	   :structure)
	  ((define-signature define-package) :syntax)))
