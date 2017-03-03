; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Marcus Crestani, David Frese

(define-structures ((prescheme prescheme-interface)
		    (ps-memory ps-memory-interface)
		    (memory-debug (export reinitialize-memory))
		    (ps-receive ps-receive-interface)
		    (external-constants (export make-external-constant
						external-constant?
						external-constant-enum-name
						external-constant-name
						external-constant-c-string)))
  (open (modify scheme
		(rename (open-input-file   scheme:open-input-file)
			(open-output-file  scheme:open-output-file)
			(close-input-port  scheme:close-input-port)
			(close-output-port scheme:close-output-port)
			(read-char         scheme:read-char)
			(peek-char         scheme:peek-char))
		(hide write-char newline))
	(subset i/o (byte-ready? read-byte peek-byte write-byte))
	code-vectors bitwise ascii 
	(subset primitives (unspecific))
	enumerated
	platform
	signals ; #### replace by EXCEPTIONS at some point in the future
	define-record-types
	receiving
	bigbit)   ; make sure that bignum bitwise operations are loaded
  (optimize auto-integrate)
  (begin
    ;; #### ditch when switching to EXCEPTIONS
    (define (assertion-violation who message . irritants)
      (apply error message irritants))
    )
  (files ps-defenum prescheme memory))

(define-structure ps-record-types (export define-record-type)
  (open scheme (modify define-record-types (prefix s48:)))
  (begin
    (define-syntax define-record-type
      (syntax-rules ()
	((define-record-type name type-name
	   constructor
	   (field type more ...) ...)
	 (s48:define-record-type name type-name
	   constructor
	   (field more ...) ...))))))

(define-structure ps-flonums ps-flonums-interface
  (open scheme)
  (optimize auto-integrate)
  (begin
    (define fl+ +) (define fl- -) (define fl* *) (define fl/ /)
    (define fl= =)
    (define fl< <) (define fl> >)
    (define fl<= <=) (define fl>= >=)))

(define-structure ps-unsigned-integers ps-unsigned-integers-interface
  (open scheme)
  (optimize auto-integrate)
  (begin
    (define un+ +) (define un- -) (define un* *)
    (define unquotient quotient)
    (define unremainder remainder)
    (define un= =)
    (define un< <) (define un> >)
    (define un<= <=) (define un>= >=)
    (define (unsigned->integer x) x)
    (define (integer->unsigned x) x)
    ))

;(define-structure byte-vectors byte-vector-interface
;  (open scheme code-vectors)
;  (optimize auto-integrate)
;  (files byte-vector))
;
;(define-interface byte-vector-interface
;  (export make-byte-vector
;          byte-vector?
;          byte-vector-ref byte-vector-set!
;          signed-byte-vector-ref signed-byte-vector-set!
;          byte-vector-ref32 byte-vector-set32!
;          signed-byte-vector-ref32 signed-byte-vector-set32!))
;
;(define-structure xmemory ps-memory-interface
;  (open scheme byte-vectors exceptions enumerated bitwise)
;  (files simple-memory))
