; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Robert Ransom, 
; Marcus Crestani, David Frese

(define-structures ((scheme-level-1 scheme-level-1-interface)
		    (util util-interface)
		    (set-text-procedures (export set-char-map-procedures!
						 set-string-ci-procedures!))
		    (syntax-rules-data syntax-rules-data-interface)
		    (syntax-rules-apply syntax-rules-apply-interface))
  (open scheme-level-0 ascii low-exceptions
	(subset primitives (unspecific))
        bitwise
	debug-messages
	code-quotation syntax-transformers)			; needed by SYNTAX-RULES
  (usual-transforms case quasiquote syntax-rules)
  (files (rts charmap)
	 (rts base)
	 (rts util)
	 (rts number)
	 (rts lize)	  ; Rationalize
	 (rts syntax-rules-data)
	 (rts syntax-rules-apply))
  (optimize auto-integrate))


; "Level 2"

(define-structures ((record-types record-types-interface)
		    (records-internal records-internal-interface))
  (open scheme-level-1 records low-exceptions
	primitives)
  (files (rts record))
  (optimize auto-integrate))

; The external code needs this to check the types of records.

(define-structure export-the-record-type (export)
  (open scheme-level-1 records-internal shared-bindings)
  (begin
    (define-exported-binding "s48-the-record-type" :record-type)))

(define-structures ((define-record-types define-record-types-interface)
		    (define-sync-record-types
		      (export (define-synchronized-record-type :syntax))))
  (open scheme-level-1
	records record-types records-internal
	loopholes
	low-proposals	;provisional-checked-record-{ref|set!}
	primitives)	;unspecific, record-type<=?
  (files (rts jar-defrecord)))

(define-structures ((methods methods-interface)
		    (meta-methods meta-methods-interface))
  (open scheme-level-1
	define-record-types
	records record-types records-internal
	bitwise util primitives
	low-exceptions)
  (files (rts method))
  (optimize auto-integrate))

(define-structure number-i/o number-i/o-interface
  (open scheme-level-1 methods low-exceptions ascii)
  (files (rts numio)))

(define-structures ((fluids fluids-interface)
		    (fluids-internal fluids-internal-interface))
  (open scheme-level-1 define-record-types primitives cells)
  (files (rts fluid))
  (optimize auto-integrate))

(define-structure wind wind-interface
  (open scheme-level-1 low-exceptions define-record-types
	fluids fluids-internal
	low-proposals
	escapes)
  (files (rts wind))
  (optimize auto-integrate))

(define-structure session-data (export make-session-data-slot!
				       initialize-session-data!
				       session-data-ref
				       session-data-set!)
  (open scheme-level-1
	primitives)
  (files (rts session))
  (optimize auto-integrate))

(define-structure text-codecs text-codecs-interface
  (open scheme-level-1
	define-record-types
	bitwise
	unicode
	byte-vectors
	(subset primitives (char->utf utf->char))
	(subset architecture (text-encoding-option))
	enumerated enum-case)
  (files (rts text-codec))
  (optimize auto-integrate))

(define-structure encodings encodings-interface
  (open scheme-level-1
	unicode
	byte-vectors
	(subset primitives (char->utf utf->char))
	(subset architecture (text-encoding-option))
	text-codecs
	enumerated
	conditions exceptions
	proposals
	(subset silly (reverse-list->string)))
  (optimize auto-integrate)
  (files (rts encoding)))

(define-structures ((os-strings os-strings-interface)
		    (os-strings-internal (export initialize-os-string-text-codec!)))
  (open scheme-level-1
	define-record-types
	exceptions
	byte-vectors
	(subset primitives (system-parameter make-immutable! copy-bytes!))
	(subset architecture (system-parameter-option))
	text-codecs encodings
	enumerated
	fluids)
  (files (rts os-string)))

(define-structures ((i/o i/o-interface)
		    (i/o-internal i/o-internal-interface))
  (open scheme-level-1 
	exceptions
	fluids
	architecture
	primitives
	ascii unicode
	ports byte-vectors bitwise
	define-record-types
	proposals
	(subset threads-internal (maybe-commit-no-interrupts))
	session-data
	debug-messages	; for error messages
	methods         ; &disclose <input-port> <output-port>
	number-i/o      ; number->string for debugging
	text-codecs
	handle		; report-errors-as-warnings
	vm-exceptions)     ; wrong-number-of-args stuff
  (files (rts port)
	 (rts port-buffer)
	 (rts current-port))
  (optimize auto-integrate))

(define-structure channel-i/o channel-i/o-interface
  (open scheme-level-1 byte-vectors cells
	channels
	i/o i/o-internal
	(subset primitives (os-error-message))
	os-strings
	conditions
	(subset threads-internal (maybe-commit-no-interrupts))
	proposals
	condvars condvars-internal
	interrupts
	architecture
	session-data
	debug-messages)	; for error messages
  (files (rts channel)))

(define-structure channel-ports channel-ports-interface
  (open scheme-level-1 byte-vectors define-record-types ascii
	ports
	i/o i/o-internal text-codecs
	channels channel-i/o
	os-strings
	proposals
	condvars
	exceptions conditions signal-conditions
	architecture		; channel-opening options
	(subset primitives      (channel-parameter))
	handle
	debug-messages		; for error messages
	(subset util		(unspecific))
	(subset primitives	(add-finalizer! os-error-message)))
  (files (rts channel-port)))

(define-structure conditions conditions-interface
  (open scheme-level-1 low-exceptions
	define-record-types
	record-types
	(subset records (record-type)))
  (files (rts condition)))

(define-structure writing writing-interface
  (open scheme-level-1
	unicode
	number-i/o
	(subset i/o             (write-char write-string))
	(subset i/o-internal    (output-port-option))
	(subset methods         (disclose))
	(subset i/o-internal	(open-output-port?))
	exceptions
	(subset channels	(channel? channel-id))
	(subset code-vectors	(code-vector?)))
  (files (rts write)))
	 
(define-structure reading reading-interface
  (open scheme-level-1
	number-i/o
	(subset i/o-internal (input-port-option))
	ascii		;for dispatch table
	unicode
	conditions	;define-condition-type
	exceptions      ;raise
	primitives	;make-immutable!
	silly)		;reverse-list->string
  (files (rts read)
	 (rts syntax-info))
  (optimize auto-integrate))

(define-structure scheme-level-2 scheme-level-2-interface
  (open scheme-level-1
	number-i/o
	writing
	reading
	wind
	i/o
	channel-ports))

(define-structure features features-interface
  (open primitives i/o))

; Hairier stuff now.

(define-structure templates templates-interface
  (open scheme-level-1 primitives)
  (files (rts template))
  (optimize auto-integrate))

(define-structure continuations continuations-interface
  (open scheme-level-1 primitives
	architecture code-vectors
	templates closures all-operators
	methods)
  (files (rts continuation))
  (optimize auto-integrate))

(define-structure more-types (export <closure> <code-vector> <location> <double>
				     <template> <channel> <port> <weak-pointer>
				     <shared-binding> <cell> <transport-link-cell>)
  (open scheme-level-1 methods
	closures code-vectors locations cells templates channels ports
	primitives shared-bindings)
  (begin (define-simple-type <closure>     (<value>) closure?)
	 (define-simple-type <code-vector> (<value>) code-vector?)
	 (define-simple-type <location>    (<value>) location?)
	 (define-simple-type <cell>        (<value>) cell?)
	 (define-simple-type <template>    (<value>) template?)
	 (define-simple-type <channel>     (<value>) channel?)
	 (define-simple-type <port>        (<value>) port?)
	 (define-simple-type <double>      (<rational>) double?)
	 (define-simple-type <weak-pointer> (<value>) weak-pointer?)
	 (define-method &disclose ((obj <weak-pointer>)) (list 'weak-pointer))
	 (define-simple-type <transport-link-cell> (<value>) transport-link-cell?)
	 (define-method &disclose ((obj <transport-link-cell>)) (list 'transport-link-cell))
	 (define-simple-type <shared-binding> (<value>) shared-binding?)
	 (define-method &disclose ((obj <shared-binding>))
	   (list (if (shared-binding-is-import? obj)
		     'imported-binding
		     'exported-binding)
		 (shared-binding-name obj)))))

(define-structure enumerated enumerated-interface
  (open scheme-level-1 low-exceptions)
  (files (rts defenum scm)))

(define-structure architecture vm-architecture-interface
  (open scheme-level-1 low-exceptions enumerated platform)
  (files (vm/interp arch)))

(define-structure vm-data vm-data-interface
  (open scheme-level-1 enumerated bitwise ascii
        architecture platform)
  (begin
    ; Scheme/Pre-Scheme differences
    (define (arithmetic-shift-right n k)
      (arithmetic-shift n (- k)))
    (define shift-left arithmetic-shift)
    
    ; From vm/vm-utilities.scm
    (define (adjoin-bits high low k)
      (+ (arithmetic-shift high k) low))
    
    (define (low-bits n k)
      (bitwise-and n (- (arithmetic-shift 1 k) 1)))
    
    (define high-bits arithmetic-shift-right)
    
    (define unsigned-high-bits high-bits)
    
    (define-syntax assert
      (syntax-rules ()
        ((assert foo) #t)))

    (define (integer->unsigned x) x)
    (define un> >)

    ; We just know this.
    (define useful-bits-per-word c-useful-bits-per-word))
  (files (vm/data data)))

(define-structure vm-exceptions vm-exceptions-interface
  (open scheme-level-1
	conditions
	enumerated enum-case
	architecture
	locations
	(subset primitives (set-exception-handlers! unspecific)))
  (files (rts vm-exception)))

(define-structures ((exceptions exceptions-interface)
		    (exceptions-internal exceptions-internal-interface)
		    (handle handle-interface))
  (open scheme-level-1
	low-exceptions-internal
	signal-conditions
	fluids cells
	conditions
	vm-exceptions
	primitives	  ;set-exception-handlers!, etc.
	wind		  ;CWCC
	methods
	meta-methods
	more-types
	architecture
	enumerated
	debug-messages	  ; for printing from last-resort-condition handler
	vm-exposure	  ;primitive-catch
	templates	  ;template-code, template-info
	continuations	  ;continuation-pc, etc.
	locations	  ;location?, location-id
	closures	  ;closure-template
	number-i/o)       ; number->string, for backtrace
  (files (rts exception)))  ; Needs generic, arch

(define-structure interrupts interrupts-interface
  (open scheme-level-1
	fluids
	conditions exceptions signal-conditions
	bitwise
	escapes
	session-data
	primitives
	architecture)
  (files (rts interrupt))
  (optimize auto-integrate)) ;mostly for threads package...

(define-structure external-events external-events-interface
  (open scheme-level-1
	(subset wind (dynamic-wind))
	enumerated
	condvars condvars-internal proposals
	session-data
	interrupts
	(subset primitives (new-external-event-uid
			    unregister-external-event-uid!)))
  (files (rts external-event)))

(define-structures ((threads threads-interface)
		    (threads-internal threads-internal-interface))
  (open (modify scheme-level-1 (hide min))
	enumerated queues cells
	(subset proposals            (define-synchronized-record-type))
	define-record-types
	interrupts
        wind
        fluids
	fluids-internal         ;get-dynamic-env
	proposals		;maybe-commit
        escapes                 ;primitive-cwcc
        conditions              ;error?
	signal-conditions
        handle                  ;with-handler
	exceptions
	loopholes               ;for converting #f to a continuation
	architecture            ;time-option
	session-data
	debug-messages
	(subset primitives	(find-all-records
				 current-thread set-current-thread!
				 unspecific
				 collect
				 time)))
  (optimize auto-integrate)
  (files (rts thread)
	 (rts sleep)))

(define-structure proposals proposals-interface
  (open scheme-level-1 low-proposals
        util
	define-record-types define-sync-record-types
	primitives)		 ;unspecific
  (files (rts proposal)))

(define-structure scheduler scheduler-interface
  (open scheme-level-1 threads threads-internal enumerated enum-case queues
	debug-messages
	exceptions)       		;error
  (files (rts scheduler)))

(define-structure root-scheduler (export root-scheduler
					 spawn-on-root
					 scheme-exit-now
					 call-when-deadlocked!)
  (open scheme-level-1 threads threads-internal scheduler queues
	session-data
	conditions	;warning?, error?
	writing			;display
	debug-messages		;for debugging
	(subset i/o		(current-error-port newline))
	(subset exceptions	(error))
	(subset handle		(with-handler))
	(subset i/o-internal	(output-port-forcers output-forcer-id))
	(subset fluids-internal (get-dynamic-env))
	(subset interrupts      (with-interrupts-inhibited
				 all-interrupts
				 set-enabled-interrupts!))
	(subset external-events (zap-external-event-orphans!))
	(subset wind            (call-with-current-continuation))
	(subset channel-i/o	(zap-i/o-orphans!
				 initialize-channel-i/o!
				 abort-unwanted-reads!))
	(modify primitives      (expose wait unspecific)))
  (files (rts root-scheduler)))

(define-structure enum-case (export (enum-case :syntax))
  (open scheme-level-1 enumerated util)
  (begin
    (define-syntax enum-case
      (syntax-rules (else)
	((enum-case enumeration (x ...) clause ...)
	 (let ((temp (x ...)))
	   (enum-case enumeration temp clause ...)))
	((enum-case enumeration value ((name ...) body ...) rest ...)
	 (if (or (= value (enum enumeration name)) ...)
	     (begin body ...)
	     (enum-case enumeration value rest ...)))
	((enum-case enumeration value (else body ...))
	 (begin body ...))
	((enum-case enumeration value)
	 (unspecific))))))

(define-structures ((queues queues-interface)
		    (queues-internal queues-internal-interface))
  (open scheme-level-1
        util
        proposals
        cells
        exceptions)
  (files (big queue))
  (optimize auto-integrate))

; No longer used
;(define-structure linked-queues (compound-interface 
;                                 queues-interface
;                                 (export delete-queue-entry!
;                                         queue-head))
;  (open scheme-level-1 define-record-types exceptions primitives)
;  (files (big linked-queue))
;  (optimize auto-integrate))

(define-structures ((condvars condvars-interface)
		    (condvars-internal (export condvar-has-waiters?)))
  (open scheme-level-1 queues
	proposals
	threads threads-internal)
  (optimize auto-integrate)
  (files (rts condvar)))

(define-structure usual-resumer (export usual-resumer
					make-usual-resumer
					add-initialization-thunk!)
  (open scheme-level-1
	os-strings
	(subset i/o-internal (initialize-i/o initialize-i/o-handlers!))
	(subset i/o (set-port-text-codec!))
	channel-i/o      ;initialize-channel-i/o
	channel-ports    ;{in,out}put-channel->port
	(subset text-codecs (find-text-codec))
	os-strings-internal
	session-data     ;initialize-session-data!
	fluids-internal	 ;initialize-dynamic-state!
	exceptions-internal
	vm-exceptions
	interrupts	 ;initialize-interrupts!
	(subset external-events (initialize-external-events!))
	records-internal ;initialize-records!
	shared-bindings	 ;find-undefined-imported-bindings
	debug-messages	 ;warn about undefined bindings 
	threads-internal ;start threads
	root-scheduler)  ;start a scheduler
  (files (rts init)))

; Weak pointers & populations

(define-structure weak weak-interface
  (open scheme-level-1 exceptions
	primitives)	;Open primitives instead of loading (alt weak)
  (files ;;(alt weak)   ;Only needed if VM's weak pointers are buggy
	 (rts population)))

