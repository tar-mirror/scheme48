; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber

; Miscellaneous things.

(define-structure reset/shift (export (reset :syntax)
				      (shift :syntax))
  (open scheme escapes exceptions)
  (files shift-reset))

(define-structure call-with-mumble-pipes
    (export call-with-input-pipe call-with-output-pipe)
  (open scheme
	posix-processes
	posix-i/o
	i/o)		; current-error-port
  (files pipe))

(define-structure engines (export (engine :syntax))
  (open scheme primitives interrupts)
  (files engine))

(define-structure integer-lengths (export integer-length)
  (open scheme)
  (files ilength))

(define-structure remote (export remote-repl
				 serve
				 make-socket)
  (open scheme
	;; For server
	closures
	handle conditions
	vm-exposure
	;; For client
	compiler
	command-processor
	interfaces
 	environments
	;; For both?
	packages
	package-commands-internal
	;; packages-internal ?
	syntactic
	built-in-structures
	dump/restore
	sockets
	exceptions features)
  (files remote))

(define-structure requirements (export (require :syntax))
  (open scheme-level-2
	packages
	environments
	ensures-loaded
	package-commands-internal
	exceptions)
  (files require))


; Procedure annotations

(define-structure annotations
    (export annotate-procedure procedure-annotation)
  (open scheme-level-1 assembler)
  (files annotate))


; DOODL

(define-structure doodl doodl-interface
  (open scheme
	(modify methods (rename (define-method really-define-method)))
	meta-methods
	annotations
	define-record-types
	records
	record-types
	;; handle	; ignore-errors
	;; conditions	; error?
	util 
	exceptions)
  (files doodl))


(define-interface doodl-interface
  (export ((method
	    define-class
	    define-method
	    define-generic-function
	    define-class
	    set)
	   :syntax)
	  <function>
	  <generic-function>
	  <method>
	  :class

	  <object>
	  <number>
	  <complex>
	  <real>
	  <rational>
	  <integer>
	  <pair>
	  <symbol>
	  <char>
	  <null>
	  <vector>
	  <string>
	  <eof-object>
	  <input-port>
	  <output-port>

	  <list>    ;foo
	  make
	  initialize
	  car-setter cdr-setter vector-ref-setter
	  id?
	  (bind :syntax)
	  ;; etc. etc.
	  ))

; SICP

(define-interface sicp-interface
  (export (cons-stream :syntax)
	  stream-car stream-cdr
	  the-empty-stream stream-null?
	  get put
	  error))

(define-structure sicp sicp-interface
  (open scheme
	(subset exceptions (error))
	tables)
  (files sicp))
