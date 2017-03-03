; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Marcel Turino,
; Marcus Crestani, Robert Ransom, Martin Gasbichler, Sebastian Rheinnecker

; Packages for the programming environment: the command processor, inspector,
; and disassembler and assembler.

; Command processor

(define-structures ((command-processor command-processor-interface)
		    (command (export command-processor)))
  (open scheme ;;-level-2     ; eval, interaction-environment
	tables fluids cells
	conditions
	os-strings
	define-record-types
	handle
	command-levels
	command-state
	menus
	reading			; gobble-line, with-sharp-sharp
	i/o                     ; current-error-port
	display-conditions	; display-condition
	methods
	util			; unspecific
	undefined		; $note-undefined
	features		; force-output
	interrupts		; set-enabled-interrupts!, all-interrupts
	vm-exposure		; primitive-catch
	fluids-internal         ; get-dynamic-env, set-dynamic-env!
	nodes			; for ## kludge
	exceptions signal-conditions
	debug-messages		; for debugging

        (subset silly (reverse-list->string))

	(subset evaluation (load-script-into))
	(subset packages-internal (package-reader))
	(subset environments (environment-ref))
	(subset shared-bindings (shared-binding-ref lookup-imported-binding))

	(subset root-scheduler   (scheme-exit-now))
	(subset threads          (thread? thread-uid))
	(subset threads-internal (thread-continuation))
	(subset continuations    (continuation?)))

  (files (env version-info)
	 (env command)
	 (env read-command)))

(define-structures ((command-levels command-levels-interface)
		    (command-state command-state-interface))
  (open scheme
	enumerated enum-case 
	os-strings
	tables queues
	session-data
	define-record-types
	threads threads-internal
	scheduler
	interrupts
	handle
	display-conditions	; display-condition
	weak
	debug-messages		; for debugging
	exceptions signal-conditions
	i/o			; current-error-port
	(subset filenames (with-translations make-translations))
	util                    ; unspecific
	channel-i/o             ; steal-channel-port
	fluids
	fluids-internal         ; get-dynamic-env, set-dynamic-env!
	environments		; with-interaction-environment,
				;   interaction-environment
	root-scheduler          ; call-when-deadlocked!
	conditions)

  (files (env user)
	 (env command-level)))

(define-structure basic-commands basic-commands-interface
  (open scheme-level-2
        command-processor
	command-levels
	command-state
        undefined               ; noting-undefined-variables
	environments		; with-interaction-environment
	(modify evaluation (hide load)) ; eval, load-into
        ;; packages		; package?
	root-scheduler		; scheme-exit-now
	i/o			; silently
	)
  (files (env basic-command)))

; Usual command set
               
(define-structure usual-commands usual-commands-interface
  (open basic-commands
        build-commands
        package-commands
        debug-commands
        inspect-commands
        disassemble-commands
	))

; Image builder.

(define-structures ((build (export build-image stand-alone-resumer))
		    (build-commands build-commands-interface))
  (open scheme-level-2
        command-processor
	command-levels
	command-state
	menus			; write-line
        conditions
	exceptions signal-conditions
	handle
        usual-resumer
        filenames               ; translate
        display-conditions      ; display-condition
        evaluation              ; package-for-load, eval
	environments		; with-interaction-environment
	i/o			; current-error-port
        write-images
 	os-strings)
  (files (env build)))

; Package commands.

(define-structures ((package-commands package-commands-interface)
		    (package-commands-internal
		       package-commands-internal-interface))
  (open scheme
        command-processor
	command-levels
	command-state
	methods
        undefined               ; noting-undefined-variables
        packages                ; for creating a user package
        packages-internal       ; set-package-integrate?!, etc.
        package-mutation        ; package-system-sentinel
        environments            ; *structure-ref, etc.
	compiler-envs		; syntactic-tower
        ensures-loaded          ; ensure-loaded
	debug-messages
	interfaces
	ascii
	i/o			; force-output, current-error-port, silently
	exceptions
	util			; every
	tables			; table->entry-list
        meta-types              ; type->sexp
	os-strings
        fluids)
  (files (env pacman)))


; Debugging aids.

; Disclosers: this makes objects and conditions print nicely.

(define-structure disclosers disclosers-interface
  (open scheme-level-1
        methods more-types
        tables
        conditions
        display-conditions
        locations
        code-vectors
        closures
        packages-internal       ; location-info-tables
        debug-data
	segments                ; get-debug-data
        enumerated              ; enumerand->name
        weak                    ; weak-pointer?
	cells
	(subset i/o-internal (disclose-port))
	low-level		; cell-unassigned?
        templates continuations channels
        architecture)
  (files (env disclosers)))

(define-structure more-vm-exceptions (export construct-vm-exception)
  (open scheme
	conditions
	signal-conditions
	enumerated enum-case
	vm-exceptions
	architecture
	disclosers
	(subset primitives (os-error-message))
	os-strings)
  (files (env vm-exception)))

; For printing procedures with their names, etc.

(define-structure debuginfo debuginfo-interface
  (open scheme-level-2
        tables
        debug-data
        debug-data-internal	; debug-data-table make-debug-data
        ;; packages
        packages-internal	; package-name-table
        names			; generated?
	features
	weak)
  (files (env debuginfo)))

; Utility for displaying error messages

(define-structure display-conditions display-conditions-interface
  (open scheme-level-2
	writing
	methods
	conditions
	handle)	;ignore-errors
  (files (env dispcond)))

; Most of the debugging commands.

(define-structures ((debugging		;additional exports in future
		      (export breakpoint))
		    (previews
		     (export display-preview))
		    (debug-commands debug-commands-interface))
  (open scheme-level-2
        command-processor       ; define-command, etc.
	command-levels
	command-state
	menus			; write-carefully, with-limited-output
        fluids
        tables
	weak
        exceptions
        util                    ; filter
        evaluation              ; eval-from-file, eval
        environments            ; environment-define! (for ,trace)
	;; debug.scm has a procedure called condition, and it has to be called that
        (modify conditions	(prefix conditions:)
		                (expose condition))
	(modify conditions      (hide condition))
	display-conditions      ; for setting writing length and depth
        (subset filenames       (set-translation!))
        disclosers              ; template-name, debug-data-names
        packages                ; flush-location-names, package-integrate?
        packages-internal       ; [set-]package-integrate?[!], flush-location-names
	bindings
	meta-types
	(subset transforms (transform?))
	(subset primops (primop?))
	undefined		; noting-undefined-variables
        continuations           ; continuation-template, continuation-preview
        architecture            ; op/global, etc.
        interrupts              ; all-interrupts, set-enabled-interrupts!
        vm-exposure             ; fluid-let suppression kludge - fix later
        (subset nodes		(schemify))
        (subset reading-forms   ($note-file-package))
	(subset handle		(with-handler))
	debug-data		;  yucko
	debug-data-internal	;  yucko
	(modify filenames       (prefix filenames:)
		                (expose translate))
	(modify syntactic       (prefix syntactic:)
		                (expose expand expand-form))
        (modify primitives      (prefix primitives:)
		                (expose collect time memory-status)))
  (files (env debug)))

(define-structure menus menus-interface
  (open scheme-level-2
	command-levels
	command-state
	fluids
        display-conditions      ; limited-write
        util                    ; sublist
        exceptions
	handle			; ignore-errors
	conditions		; error?
	
	; the rest are for looking inside various types of objects
        closures                ; closure-template
        disclosers              ; template-debug-data, etc.
        debug-data
	segments                ; get-debug-data
        templates
        continuations
        records
	record-types
        low-level               ; vector-unassigned?
        locations
	cells
        weak
	(subset primitives      (transport-link-cell? 
				 transport-link-cell-key
				 transport-link-cell-value
				 transport-link-cell-tconc
				 transport-link-cell-next)))
  (files (env menu)))

; Inspector

(define-structure inspect-commands inspect-commands-interface
  (open scheme-level-2
        command-processor       ; define-command, etc.
	command-levels
	command-state
	menus
	exceptions
	
	; The following two structures are for ,where
        debug-data
	disclosers		; template-debug-data

	closures
	templates
	continuations

	debug-messages		; for debugging
        debugging               ; command-loop-continuation
	evaluation)		; eval
  (files (env inspect)))

; Package and interface mutation.

(define-structure package-mutation package-mutation-interface
  (open scheme-level-2 cells
        shadowing               ; shadow-location!
        packages
        interfaces
	bindings
        packages-internal
        defpackage              ; set-verify-later!
        locations
        disclosers              ; location-info
        handle
	debug-messages
        tables fluids weak exceptions)
  (files (env pedit)))

; The following hooks the compiler up with a VM exception handler for
; unbound variables.

(define-structure shadowing (export shadow-location!)
  (open scheme-level-1
        vm-exposure             ;primitive-catch
        continuations templates locations code-vectors
        vm-exceptions more-vm-exceptions exceptions
	signal-conditions
	enumerated
	disclosers
	conditions
	debug-messages
        architecture)   ;(enum op global)
  (files (env shadow)))     ;Exception handler to support package system

(define-structure parse-bytecode parse-bytecode-interface
  (open scheme
	(subset util (receive))
	bitwise
        templates
        code-vectors byte-vectors
        architecture
        enumerated
        exceptions
        fluids
        closures
        debug-data
        (subset disclosers (template-debug-data))
        continuations
        define-record-types)
  (files (env parse-bytecode)))

; Disassembler

(define-structures ((disassembler
		       (export disassemble write-instruction))
		    (disassemble-commands disassemble-commands-interface))
    (open scheme-level-2
        command-processor       ; define-command
	command-state		; focus-object
	disclosers              ; template-name
        enumerated              ; enumerand->name
        disclosers              ; location-name
        parse-bytecode
	evaluation		; eval
        templates
        continuations
        locations
        define-record-types
	bitwise
        closures
        architecture
        exceptions)
  (files (env disasm)))

; Assembler.

(define-structure assembling (export)	; No exports, this defines a compilator.
  (open scheme-level-2
	compiler		;define-compilator
	segments
	frames
	architecture
	bc-generation
	nodes			;node-form node-ref
	bindings		;binding? binding-place
        meta-types              ;value-type
        templates               ; for Richard's version
        exceptions
        enumerated              ;name->enumerand
        code-vectors)
  (files (env assem)))

; Foo

(define-structure assembler (export (lap :syntax))
  (open scheme-level-2)
  (for-syntax (open scheme-level-2 nodes meta-types assembling))
  (begin
    (define-syntax lap
      (lambda (e r c)
        (make-node (get-operator 'lap syntax-type) e)))))


