; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Harald Glab-Plhak, Marcus Crestani

;;test package definition for the ffi tests
(define-structure ffi-funcs 
  (export a-record-id
	  a-record-type
	  a-record-value
	  ffi-make-a-record
	  ffi-working-on-lists
	  ffi-get-cons-val
	  ffi-pair?
	  ffi-car
	  ffi-cdr
	  ffi-length
	  ffi-add-integer
	  ffi-record-set! 
	  ffi-record-ref
	  ffi-vector-set!
	  ffi-vector-ref
	  ffi-make-byte-vector
	  ffi-extract-byte-vector
	  ffi-extract-byte-vector-readonly
	  ffi-extract-and-modify-byte-vector
	  ffi-extract-twice-and-modify-byte-vector
	  ffi-extract-byte-vector-and-call-scheme
	  ffi-extract-byte-vector-assertion
	  ffi-make-vector
	  ffi-enums
	  ffi-get-color-enum-set
	  (color :syntax)
	  color?
	  color-set?
	  (color-set :syntax)
	  ffi-call-scheme
	  ffi-a-status-set-and-export!
	  ffi-a-status-set-by-binding!
	  ffi-a-status-set!
	  ffi-a-status
	  ffi-propagate-binding
	  ffi-propagate-binding-global
	  ffi-export-bindings
	  ffi-get-imp-binding-value
	  ffi-get-imp-binding
	  ffi-get-imp-value-by-binding
	  ffi-make-strange-value
	  ffi-strange-value->list
	  ffi-check-a-status-and-get-name
	  ffi-strange-value-free
	  external-ffi-make-local-buf
	  external-ffi-free-local-buf
	  external-ffi-free-local-buf-1
	  external-ffi-free-local-buf-2
	  external-ffi-free-local-buf-3
	  ffi-make-weak-pointer
	  ffi-weak-pointer?
	  ffi-weak-pointer-ref
	  ffi-check-string-latin-1
	  ffi-check-string-utf-8 
	  ffi-check-string-utf-16)
  (open scheme test-suites
	load-dynamic-externals
	external-calls primitives records 
	enum-sets finite-types define-record-types)
  (files ffi-funcs))

(define-structure ffi-base-test (export ffi-base-tests)
  (open scheme test-suites
	load-dynamic-externals ffi-funcs
	external-calls primitives records define-record-types)
  (files ffi-check))

(define-structure ffi-list-test (export ffi-list-tests)
  (open scheme test-suites
	load-dynamic-externals ffi-funcs
	external-calls shared-bindings
	primitives records define-record-types)
  (files ffi-list-check))

(define-structure ffi-binding-test (export ffi-binding-tests)
  (open scheme test-suites
	load-dynamic-externals ffi-funcs
	external-calls primitives records define-record-types)
  (files ffi-binding-check))

(define-structure ffi-aggregates-test (export ffi-aggregates-tests)
  (open scheme test-suites
	load-dynamic-externals ffi-funcs finite-types enum-sets     
	external-calls primitives records define-record-types)
  (files  ffi-aggregates-check))

(define-structure ffi-misc-test (export ffi-misc-tests)
  (open scheme test-suites
	load-dynamic-externals ffi-funcs finite-types
	byte-vectors enum-sets exceptions conditions    
	external-calls primitives records define-record-types)
  (files ffi-misc-check))

(define-structure ffi-trampoline-test (export ffi-trampoline-tests)
  (open scheme test-suites
	exceptions conditions
	external-calls threads locks)
  (files ffi-trampoline-check))

(define-structure ffi-buf-test (export ffi-buf-tests)
  (open scheme test-suites load-dynamic-externals 
	ffi-funcs external-calls)
  (files ffi-buf-check))

(define-structure ffi-test (export ffi-tests)
  (open scheme test-suites
	ffi-base-test ffi-list-test ffi-binding-test
	ffi-aggregates-test ffi-misc-test ffi-trampoline-test ffi-buf-test)
  (begin
    (define-test-suite ffi-tests
      (ffi-base-tests 
       ffi-list-tests 
       ffi-binding-tests 
       ffi-aggregates-tests 
       ffi-misc-tests 
       ffi-trampoline-tests
       ffi-buf-tests))))
