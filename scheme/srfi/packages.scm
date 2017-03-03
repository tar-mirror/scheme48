; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, David van Horn,
; Martin Gasbichler, Michael Zabka, Emilio Lopes

;----------------
; SRFI packages

; SRFI-0 - Doesn't work with the module system.

; Olin's list library.

(define-interface srfi-1-interface
  (export map for-each member assoc	; redefined from R5RS
	  xcons make-list list-tabulate cons* list-copy 
	  proper-list? circular-list? dotted-list? not-pair? null-list? list=
	  circular-list length+
	  iota
	  first second third fourth fifth sixth seventh eighth ninth tenth
	  car+cdr
	  take       drop       
	  take-right drop-right 
	  take!      drop-right!
	  split-at   split-at!
	  last last-pair
	  zip unzip1 unzip2 unzip3 unzip4 unzip5
	  count
	  append! append-reverse append-reverse! concatenate concatenate! 
	  unfold       fold       pair-fold       reduce
	  unfold-right fold-right pair-fold-right reduce-right
	  append-map append-map! map! pair-for-each filter-map map-in-order
	  filter  partition  remove
	  filter! partition! remove! 
	  find find-tail any every list-index
	  take-while drop-while take-while!
	  span break span! break!
	  delete delete!
	  alist-cons alist-copy
	  delete-duplicates delete-duplicates!
	  alist-delete alist-delete!
	  reverse! 
	  lset<= lset= lset-adjoin  
	  lset-union  lset-intersection  lset-difference  lset-xor
	  lset-diff+intersection
	  lset-union! lset-intersection! lset-difference! lset-xor!
	  lset-diff+intersection!))

(define-structure srfi-1 srfi-1-interface
  (open (modify scheme-level-2 (hide map for-each member assoc)) ; redefined
	receiving
	(subset exceptions (assertion-violation)))
  (files srfi-1))

; AND-LET*: an AND with local bindings, a guarded LET* special form

(define-structure srfi-2 (export (and-let* :syntax))
  (open scheme-level-2
	exceptions)		; error
  (files srfi-2))

; SRFI-3 - withdrawn

; SRFI 4: Homogeneous numeric vector datatypes
; Does not include hacks to the reader (intentionally).

(define-interface srfi-4-interface
  (export
   s8vector? make-s8vector s8vector s8vector-length
   s8vector-ref s8vector-set! s8vector->list list->s8vector
   u8vector? make-u8vector u8vector u8vector-length
   u8vector-ref u8vector-set! u8vector->list list->u8vector
   s16vector? make-s16vector s16vector s16vector-length
   s16vector-ref s16vector-set! s16vector->list list->s16vector
   u16vector? make-u16vector u16vector u16vector-length
   u16vector-ref u16vector-set! u16vector->list list->u16vector
   s32vector? make-s32vector s32vector s32vector-length
   s32vector-ref s32vector-set! s32vector->list list->s32vector
   u32vector? make-u32vector u32vector u32vector-length
   u32vector-ref u32vector-set! u32vector->list list->u32vector
   s64vector? make-s64vector s64vector s64vector-length
   s64vector-ref s64vector-set! s64vector->list list->s64vector
   u64vector? make-u64vector u64vector u64vector-length u64vector-ref
   u64vector-set! u64vector->list list->u64vector
   f32vector? make-f32vector f32vector f32vector-length f32vector-ref
   f32vector-set! f32vector->list list->f32vector
   f64vector? make-f64vector f64vector f64vector-length f64vector-ref
   f64vector-set! f64vector->list list->f64vector   
   ))

(define-structure srfi-4 srfi-4-interface
  (open scheme define-record-types 
	srfi-16 ; case-lambda
	srfi-60 ; Integers as Bits
	(modify srfi-66 ; Octet vectors
		(rename (make-u8vector srfi-66:make-u8vector)))
	srfi-74 ; blobs
	(subset big-util (no-op)))
  (files srfi-4))


; A compatible let form with signatures and rest arguments

(define-structure srfi-5 (export (let :syntax))
  (open (modify scheme-level-2 (rename (let standard-let))))
  (files srfi-5))

; Basic String Ports

(define-structure srfi-6 (export open-input-string
				 open-output-string
				 get-output-string)
  (open (modify extended-ports
		(rename (make-string-input-port    open-input-string)
			(make-string-output-port   open-output-string)
			(string-output-port-output get-output-string)))))

; Configuration language

(define-structure srfi-7 (export program)	; also defines a command
  (open scheme

	; for parsing programs
	receiving
	nondeterminism
	(subset exceptions (assertion-violation))

	(subset evaluation			(eval-from-file))

	(subset package-commands-internal	(config-package))
	ensures-loaded
	(subset packages			(note-structure-name!))

	; for defining the command
	(subset command-processor		(define-user-command-syntax
						 user-command-environment))
	(subset environments			(environment-define! environment-ref))
        
        ; for opening needed packages during evaluation
        (subset package-mutation                (package-open!))
        (subset packages                        (make-modified-structure)))

  (begin
    (define available-srfis
      '(srfi-1 srfi-2 srfi-4 srfi-5 srfi-6 srfi-7 srfi-8 srfi-9
	srfi-11 srfi-13 srfi-14 srfi-16 srfi-17 srfi-19
	srfi-23 srfi-25 srfi-26 srfi-27 srfi-28
	srfi-31 srfi-34 srfi-37
	srfi-39 srfi-40 srfi-42 srfi-43 srfi-45
        srfi-60 srfi-61 srfi-62 srfi-63 srfi-66 srfi-67
	srfi-71 srfi-74 srfi-78
	srfi-95))

    ; Some SRFI's redefine Scheme variables.
    (define shadowed
      '((srfi-1 map for-each member assoc)
	(srfi-5 let)
	(srfi-13 string->list string-copy string-fill!)
	(srfi-17 set!)
	(srfi-45 force delay)
	(srfi-43 vector-fill! vector->list list->vector)
        (srfi-61 cond)
        (srfi-63 equal?)
        (srfi-71 let let* letrec)))
    )
  
  (files srfi-7))

; receive: Binding to multiple values
; Identical to the version in UTIL

(define-structure srfi-8 (export (receive :syntax))
  (open scheme-level-2
	util))

; Defining Record Types
; SRFI-9 is a slight modification of DEFINE-RECORD-TYPE.

(define-structure srfi-9 (export (define-record-type :syntax))
  (open scheme-level-2 
	(with-prefix define-record-types sys:))
  (begin
    (define-syntax define-record-type
      (syntax-rules ()
	((define-record-type type-name . stuff)
	 (sys:define-record-type type-name type-name . stuff))))))

; SRFI-10 - no stand-alone interface.

; Syntax for receiving multiple values

(define-structure srfi-11 (export (let-values :syntax)
				  (let*-values :syntax))
  (open scheme-level-2)
  (files srfi-11))

; SRFI-12 - withdrawn

; Two more encyclopedias from Olin.

; String Library

(define-interface srfi-13-interface
  (export string-map string-map!
	  string-fold       string-unfold
	  string-fold-right string-unfold-right 
	  string-tabulate string-for-each string-for-each-index
	  string-every string-any
	  string-hash string-hash-ci
	  string-compare string-compare-ci
	  string=    string<    string>    string<=    string>=    string<>
	  string-ci= string-ci< string-ci> string-ci<= string-ci>= string-ci<> 
	  string-downcase  string-upcase  string-titlecase  
	  string-downcase! string-upcase! string-titlecase! 
	  string-take string-take-right
	  string-drop string-drop-right
	  string-pad string-pad-right
	  string-trim string-trim-right string-trim-both
	  string-filter string-delete
	  string-index string-index-right 
	  string-skip  string-skip-right
	  string-count
	  string-prefix-length string-prefix-length-ci
	  string-suffix-length string-suffix-length-ci
	  string-prefix? string-prefix-ci?
	  string-suffix? string-suffix-ci?
	  string-contains string-contains-ci
	  string-copy! substring/shared
	  string-reverse string-reverse! reverse-list->string
	  string-concatenate
	  string-concatenate/shared
	  string-concatenate-reverse
	  string-concatenate-reverse/shared
	  string-append/shared
	  xsubstring string-xcopy!
	  string-null?
	  string-join
	  string-tokenize
	  string-replace

	  string->list string-copy string-fill! 
	  string? make-string string-length string-ref string-set! 
	  string string-append list->string))

(define-structure srfi-13 srfi-13-interface
  (open (modify scheme-level-2
		(hide string-copy string-upcase string-downcase string-titlecase string-fill! string->list))
	bitwise
	srfi-8 srfi-14
	(modify unicode-char-maps
		(hide string-upcase string-downcase string-titlecase))
	(subset exceptions (assertion-violation)))
  (files srfi-13))

; Character-Set Library

(define-interface srfi-14-interface
  (export char-set? char-set=
	  char-set<= char-set-hash char-set-cursor char-set-ref
	  char-set-cursor-next end-of-char-set? char-set-fold char-set-unfold
	  char-set-unfold!  char-set-for-each char-set-map char-set-copy
	  char-set

	  list->char-set  string->char-set 
	  list->char-set! string->char-set! 

	  char-set-filter  ucs-range->char-set

	  ; the SRFI defines ->CHAR-SET, but that isn't a legal identifier
	  x->char-set
	  
	  char-set-filter! ucs-range->char-set!

	  char-set->list char-set->string

	  char-set-size char-set-count char-set-contains?
	  char-set-every char-set-any

	  char-set-adjoin  char-set-delete 
	  char-set-adjoin! char-set-delete!
 

	  char-set-complement  char-set-union  char-set-intersection  
	  char-set-complement! char-set-union! char-set-intersection! 

	  char-set-difference  char-set-xor  char-set-diff+intersection
	  char-set-difference! char-set-xor! char-set-diff+intersection!

	  char-set:lower-case	char-set:upper-case	char-set:title-case
	  char-set:letter	char-set:digit		char-set:letter+digit
	  char-set:graphic	char-set:printing	char-set:whitespace
	  char-set:iso-control	char-set:punctuation	char-set:symbol
	  char-set:hex-digit	char-set:blank		char-set:ascii
	  char-set:empty	char-set:full

	  ))

(define-structure srfi-14 srfi-14-interface
  (open scheme-level-2
	bitwise
	unicode
	byte-vectors
	(subset primitives (copy-bytes! unspecific))
	inversion-lists
	srfi-9
	(subset define-record-types (define-record-discloser))
	variable-argument-lists
	(subset big-util (partition-list))
	(subset features (make-immutable!))
	(subset exceptions (assertion-violation)))
  (optimize auto-integrate)
  (files srfi-14
	 srfi-14-base-char-sets ; auto-generated
	 srfi-14-char-sets))

; SRFI-15 - withdrawn

; Syntax for procedures of variable arity

(define-structure srfi-16 (export (case-lambda :syntax))
  (open scheme-level-2
	(subset exceptions (assertion-violation)))
  (files srfi-16))

; Generalized set!

(define-structure srfi-17 (export (set! :syntax) setter)
  (open (modify scheme-level-2 (rename (set! scheme-set!)))
	(subset exceptions (assertion-violation))
	(subset util (unspecific)))
  (files srfi-17))

; SRFI-18 - no implementation given

(define-interface srfi-19-interface
  (export 
   ;; Constants
   time-duration
   time-monotonic
   time-process
   time-tai
   time-thread
   time-utc
   ;; Current time and clock resolution
   current-date
   current-julian-day
   current-modified-julian-day
   current-time
   time-resolution
   ;; Time object and accessors
   make-time
   time?
   time-type
   time-nanosecond
   time-second
   set-time-type!
   set-time-nanosecond!
   set-time-second!
   copy-time
   ;; Time comparison procedures
   time<=?
   time<?
   time=?
   time>=?
   time>?
   ;; Time arithmetic procedures
   time-difference
   time-difference!
   add-duration
   add-duration!
   subtract-duration
   subtract-duration!
   ;; Date object and accessors
   make-date
   date?
   date-nanosecond
   date-second
   date-minute
   date-hour
   date-day
   date-month
   date-year
   date-zone-offset
   date-year-day
   date-week-day
   date-week-number
   ;; Time/Date/Julian Day/Modified Julian Day converters
   date->julian-day
   date->modified-julian-day
   date->time-monotonic
   date->time-tai
   date->time-utc
   julian-day->date
   julian-day->time-monotonic
   julian-day->time-tai
   julian-day->time-utc
   modified-julian-day->date
   modified-julian-day->time-monotonic
   modified-julian-day->time-tai
   modified-julian-day->time-utc
   time-monotonic->date
   time-monotonic->time-tai
   time-monotonic->time-tai!
   time-monotonic->time-utc
   time-monotonic->time-utc!
   time-tai->date
   time-tai->julian-day
   time-tai->modified-julian-day
   time-tai->time-monotonic
   time-tai->time-monotonic!
   time-tai->time-utc
   time-tai->time-utc!
   time-utc->date
   time-utc->julian-day
   time-utc->modified-julian-day
   time-utc->time-monotonic
   time-utc->time-monotonic!
   time-utc->time-tai
   time-utc->time-tai!
   ;; Date to string/string to date converters.
   date->string
   string->date))

(define-structure srfi-19 srfi-19-interface
  (open scheme
	srfi-9 ; DEFINE-RECORD-PROCEDURES
        srfi-23 ; ERROR
        ascii
        receiving
        (modify os-time (prefix os-time:))
        (modify extended-ports
                (rename (make-string-output-port open-output-string)
                        (string-output-port-output get-output-string)
                        (make-string-input-port open-input-string))))
  (files srfi-19))

; SRFI-20 - withdrawn
; SRFI-21 - no implementation given
; SRFI-22 - needs internals hacking

; Error reporting mechanism

(define-structure srfi-23 (export error)
  (open scheme
	(modify exceptions
		(rename (error exceptions:error))))
  (begin
    (define (error message . irritants)
      (apply exceptions:error #f message irritants))))

; Multi-dimensional Array Primitives 

(define-interface srfi-25-interface
  (export array? make-array shape array array-rank
	  array-start array-end array-ref array-set! share-array))

(define-structure srfi-25 srfi-25-interface
  (open scheme
	define-record-types
	(subset exceptions (assertion-violation)))
  (files srfi-25))

; Notation for Specializing Parameters without Currying

(define-interface srfi-26-interface
  (export ((cut cute) :syntax)))

(define-structure srfi-26 srfi-26-interface
  (open scheme)
  (files srfi-26))

; Sources of Random Bits

(define-interface srfi-27-interface
  (export random-integer
          random-real
          default-random-source
          make-random-source
          random-source?
          random-source-state-ref
          random-source-state-set!
          random-source-randomize!
          random-source-pseudo-randomize!
          random-source-make-integers
          random-source-make-reals))

(define-structure srfi-27 srfi-27-interface
  (open
   scheme-level-1
   floatnums
   external-calls load-dynamic-externals
   (subset srfi-9 (define-record-type)) 
   (subset srfi-23 (error)))
  (files srfi-27))

; Basic Format Strings

(define-interface srfi-28-interface
  (export format))

(define-structure srfi-28 srfi-28-interface
  (open scheme
	(subset exceptions (assertion-violation))
	srfi-6				; string ports
	)
  (files srfi-28))

; A special form for recursive evaluation

(define-interface srfi-31-interface
  (export (rec :syntax)))

(define-structure srfi-31 srfi-31-interface
  (open scheme)
  (begin
    ; Taken directly from the SRFI document.
    
    (define-syntax rec
      (syntax-rules ()
	((rec (NAME . VARIABLES) . BODY)
	 (letrec ( (NAME (lambda VARIABLES . BODY)) ) NAME))
	((rec NAME EXPRESSION)
	 (letrec ( (NAME EXPRESSION) ) NAME))))))

; Exception Handling for Programs

(define-interface srfi-34-interface
  (export with-exception-handler
	  raise
	  (guard :syntax)))

(define-structure srfi-34 srfi-34-interface
  (open scheme
	exceptions))

; SRFI 37: args-fold: a program argument processor

(define-interface srfi-37-interface
  (export args-fold
	  option-processor
	  option option-names option-required-arg? option-optional-arg?))

(define-structure srfi-37 srfi-37-interface
  (open scheme
	define-record-types
	srfi-11)
  (files srfi-37))

; SRFI 39: Parameter objects

(define-interface srfi-39-interface
  (export make-parameter
          ((parameterize) :syntax)))

(define-structure srfi-39 srfi-39-interface
  (open scheme
	exceptions
        fluids
        cells)
  (files srfi-39))
  
; SRFI 40: A Library of Streams

(define-interface srfi-40-interface
  (export stream-null (stream-cons :syntax) stream? stream-null? stream-pair?
          stream-car stream-cdr (stream-delay :syntax) stream stream-unfoldn
          stream-map stream-for-each stream-filter))

(define-structure srfi-40 srfi-40-interface
  (open (modify scheme (hide delay force))
        define-record-types
	cells
        (subset srfi-1 (any every))
        srfi-23 ; ERROR
	)
  (files srfi-40))

; SRFI 42: Eager Comprehensions

(define-interface srfi-42-interface
  (export ((do-ec
	    list-ec append-ec
	    string-ec string-append-ec
	    vector-ec vector-of-length-ec
	    sum-ec product-ec
	    min-ec max-ec
	    any?-ec every?-ec
	    first-ec last-ec
	    fold-ec fold3-ec) :syntax)
	  ((:
	    :list :string :vector
	    :integers
	    :range :real-range :char-range
	    :port
	    :dispatched) :syntax)
	  ((:do :let :parallel :while :until) :syntax)
	  :-dispatch-ref :-dispatch-set! make-initial-:-dispatch 
	  (:generator-proc :syntax)))

(define-structure srfi-42 srfi-42-interface
  (open scheme
	srfi-23)
  (files srfi-42))

; SRFI 43: Vector library

(define-interface srfi-43-interface
  (export make-vector vector vector-unfold vector-unfold-right
          vector-copy vector-reverse-copy vector-append vector-concatenate
          vector? vector-empty? vector= vector-ref vector-length
          vector-fold vector-fold-right vector-map vector-map!
          vector-for-each vector-count vector-index vector-skip
          vector-index-right vector-skip-right
          vector-binary-search vector-any vector-every
          vector-set! vector-swap! vector-fill! vector-reverse!
          vector-copy! vector-reverse-copy! vector-reverse!
          vector->list reverse-vector->list list->vector reverse-list->vector))

(define-structure srfi-43 srfi-43-interface
  (open (modify scheme
                (rename (vector-fill! %vector-fill!))
                (rename (vector->list %vector->list))
                (rename (list->vector %list->vector)))
        (modify util (rename (unspecific unspecified-value)))
        (subset srfi-8 (receive))
        (subset exceptions (assertion-violation)))
  (files srfi-43))

; SRFI 45: Primitives for Expressing Iterative Lazy Algorithms

(define-interface srfi-45-interface
  (export (lazy :syntax) force (delay :syntax)))

(define-structure srfi-45 srfi-45-interface
  (open (modify scheme (hide delay force))
	define-record-types)
  (files srfi-45))

; SRFI 60: Integers as Bits

(define-interface srfi-60-interface
  (export logand bitwise-and logior bitwise-ior logxor bitwise-xor
	  lognot bitwise-not bitwise-if bitwise-merge logtest 
	  any-bits-set? logcount bit-count integer-length 
	  log2-binary-factors first-set-bit logbit?  bit-set?  
	  copy-bit bit-field copy-bit-field ash arithmetic-shift 
	  rotate-bit-field reverse-bit-field integer->list 
	  list->integer booleans->integer))

(define-structure srfi-60 srfi-60-interface
  (open scheme bitwise)
  (files srfi-60))

; SRFI 61: A more general cond clause

(define-interface srfi-61-interface
  (export (cond :syntax)))

(define-structure srfi-61 srfi-61-interface
  (open (modify scheme (hide cond)))
  (files srfi-61))

; SRFI 62: S-expression comments

; note this modifies the reader
(define-structure srfi-62 (export)
  (open scheme
	reading)
  (begin
    (define-sharp-macro #\;
      (lambda (char port)
	;; The octothorpe reader leaves the semicolon
	;; in the input stream, so we consume it.
	(read-char port)
	(sub-read-carefully port)
	(sub-read port)))))

; SRFI 63: Homogeneous and Heterogeneous Arrays

(define-interface srfi-63-interface
  (export array? equal? array-rank array-dimensions make-array
          make-shared-array list->array array->list vector->array
          array->vector array-in-bounds? array-ref array-set!
          a:fixz8b a:fixz16b a:fixz32b a:fixz64b
          a:fixn8b a:fixn16b a:fixn32b a:fixn64b          
          a:floc16b a:floc32b a:floc64b a:floc128b
          a:flor16b a:flor32b a:flor64b a:flor128b          
          a:floq32d a:floq64d a:floq128d
          a:bool))

(define-structure srfi-63 srfi-63-interface
  (open (modify scheme-level-2 (hide equal?)) ; redefined
        define-record-types
        srfi-4 srfi-16
        (subset exceptions (assertion-violation)))
  (files srfi-63))

; SRFI 66: Octet Vectors

(define-interface srfi-66-interface
  (export make-u8vector
	  u8vector?
	  list->u8vector u8vector->list
	  u8vector
	  u8vector-length
	  u8vector-ref u8vector-set!
	  u8vector-copy! u8vector-copy
	  u8vector=?
	  u8vector-compare))

(define-structure srfi-66 srfi-66-interface
  (open scheme
	byte-vectors
	(subset primitives (copy-bytes!)))
  (files srfi-66))

; SRFI 67: Compare Procedures

(define-interface srfi-67-interface
  (export boolean-compare
	  char-compare char-compare-ci
	  string-compare string-compare-ci
	  symbol-compare
	  integer-compare rational-compare real-compare complex-compare
	  number-compare
	  vector-compare vector-compare-as-list
	  list-compare list-compare-as-vector
	  pair-compare-car pair-compare-cdr
	  pair-compare
	  default-compare
	  (refine-compare :syntax)
	  (select-compare :syntax)
	  (cond-compare :syntax)
	  (if3 :syntax)
	  ((if=? if<? if>? if<=? if>=? if-not=?) :syntax)
	  =? <? >? <=? >=? not=?
	  </<? </<=? <=/<? <=/<=? >/>? >/>=? >=/>? >=/>=?
	  chain=? chain<? chain>? chain<=? chain>=?
	  pairwise-not=?
	  min-compare max-compare
	  kth-largest
	  compare-by< compare-by> compare-by<= compare-by>= compare-by=/< compare-by=/>
	  debug-compare))

(define-structure srfi-67 srfi-67-interface
  (open scheme
	srfi-16 ; CASE-LAMBDA
	srfi-23 ; ERROR
	srfi-27 ; RANDOM
	)
  (files srfi-67))

; SRFI 71: Extended LET-syntax for multiple values

(define-interface srfi-71-interface
  (export ((let let* letrec) :syntax)
          ((values->list values->vector) :syntax)
          uncons
          uncons-2
          uncons-3
          uncons-4
          uncons-cons
          unlist
          unvector))

(define-structure srfi-71*
  (export ((srfi-let srfi-let* srfi-letrec) :syntax)
          ((values->list values->vector) :syntax)
          uncons
          uncons-2
          uncons-3
          uncons-4
          uncons-cons
          unlist
          unvector)  
  (open (modify scheme
                (rename (let r5rs-let)
                        (let* r5rs-let*)
                        (letrec r5rs-letrec))))
  (files srfi-71))

(define-structure srfi-71 srfi-71-interface
  (open (modify srfi-71*
                (rename (srfi-let let)
                        (srfi-let* let*)
                        (srfi-letrec letrec)))))

; SRFI 74: Octet-addressed binary objects

(define-interface srfi-74-interface
  (export (endianness :syntax)
	  blob? make-blob
	  blob-length
	  blob-u8-ref blob-u8-set!
	  blob-s8-ref blob-s8-set!
	  blob-uint-ref blob-uint-set!
	  blob-sint-ref blob-sint-set!

	  blob-u16-ref blob-s16-ref
	  blob-u16-native-ref blob-s16-native-ref
	  blob-u16-set! blob-s16-set!
	  blob-u16-native-set! blob-s16-native-set!

	  blob-u32-ref blob-s32-ref
	  blob-u32-native-ref blob-s32-native-ref
	  blob-u32-set! blob-s32-set!
	  blob-u32-native-set! blob-s32-native-set!

	  blob-u64-ref blob-s64-ref
	  blob-u64-native-ref blob-s64-native-ref
	  blob-u64-set! blob-s64-set!
	  blob-u64-native-set! blob-s64-native-set!

	  blob=?
	  blob-copy blob-copy!

	  blob->u8-list u8-list->blob
	  blob->uint-list blob->sint-list 
	  uint-list->blob sint-list->blob))

(define-structure srfi-74 srfi-74-interface
  (open scheme
	srfi-23 ; ERROR
	srfi-26 ; CUT
	bitwise
	srfi-66 ; U8VECTOR
	)
  (files srfi-74))

; SRFI 78: Lightweight testing

(define-interface srfi-78-interface
  (export (check :syntax)
          (check-ec :syntax)
          check-report
          check-set-mode!
          check-reset!
          check-passed?))

(define-structure srfi-78 srfi-78-interface
  (open scheme srfi-42
        (subset exceptions (assertion-violation)))
  (files srfi-78))

; SRFI 95: Sorting and Merging

(define-interface srfi-95-interface
  (export sorted? merge merge! sort sort!))

(define-structure srfi-95 srfi-95-interface
  (open (modify scheme (hide equal?)) srfi-63
	(modify sorting (prefix olin:))
	(modify list-merge-sort (prefix olin:))
	(modify vector-merge-sort (prefix olin:)))
  (files srfi-95))
