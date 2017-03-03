; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber


; Structures 'n' packages.

; --------------------
; Structures
;
; A structure is a map from names to binding records, determined by an
; interface (a set of names) and a package (a map from names to binding
; records).
;
; The interface is specified as a thunk.  This removes dependencies on the
; order in which structures are defined.  Also, if the interface is redefined,
; re-evaluating the thunk produces the new, correct interface (see
; env/pedit.scm).
;
; Clients are packages that import the structure's bindings.

(define-record-type structure :structure-type ; avoid name conflict with :STRUCTURE type
  (really-make-structure package interface-thunk interface clients name)
  structure?
  (interface-thunk structure-interface-thunk)
  (interface structure-interface-really set-structure-interface!)
  (package   structure-package)
  (clients   structure-clients)
  (name	     structure-name set-structure-name!))

(define-record-discloser :structure-type
  (lambda (structure)
    (list 'structure
	  (package-uid (structure-package structure))
	  (structure-name structure))))

; Get the actual interface, calling the thunk if necessary.

(define (structure-interface structure)
  (or (structure-interface-really structure)
      (begin (initialize-structure! structure)
	     (structure-interface-really structure))))

(define (initialize-structure! structure)
  (let ((int ((structure-interface-thunk structure))))
    (if (interface? int)
	(begin (set-structure-interface! structure int)
	       (note-reference-to-interface! int structure))
	(assertion-violation 'initialize-structure!
			     "invalid interface" structure))))

; Make a structure over PACKAGE and the interface returned by INT-THUNK.

(define (make-structure package int-thunk . name-option)
  (if (not (package? package))
      (assertion-violation 'make-structure
			   "invalid package" package int-thunk))
  (let ((struct (really-make-structure package
				       (if (procedure? int-thunk)
					   int-thunk
					   (lambda () int-thunk))
				       #f
				       (make-population)
				       #f)))
    (if (not (null? name-option))
	(note-structure-name! struct (car name-option)))
    (add-to-population! struct (package-clients package))
    struct))

; Make a structure by using COMMANDS to modify the STRUCTURE's interface.
; We parse the commands first so that errors are detected before the new
; structure is installed anywhere.

(define (make-modified-structure structure commands)
  (let* ((interface-maker (make-modified-interface-maker commands))
	 (new-struct (make-structure (structure-package structure)
				     (lambda ()
				       (interface-maker
				         (structure-interface structure)))
				    (structure-name structure))))
    (if (structure-unstable? structure)
	(add-to-population! new-struct (structure-clients structure)))
    new-struct))

; STRUCT has name NAME.  NAME can then also be used to refer to STRUCT's
; package.

(define (note-structure-name! struct name)
  (if (and name (not (structure-name struct)))
      (begin (set-structure-name! struct name)
	     (note-package-name! (structure-package struct) name))))

; A structure is unstable if its package is.  An unstable package is one
; where new code may be added, possibly modifying the exported bindings.

(define (structure-unstable? struct)
  (package-unstable? (structure-package struct)))

; The #F returned for compile-time environments is conservative.  You could
; look up the name of interest and see where it came from.  It might come
; from a lexical binding or a stable package or structure.  A procedure to
; do this could go in cenv.scm.

(define (environment-stable? env)
  (cond ((package? env)
         (not (package-unstable? env)))
        ((structure? env)
         (not (structure-unstable? env)))
        ((compiler-env? env)
         #f)                    ; conservative
        (else
         (assertion-violation 'environment-stable? "invalid environment" env))))

; Map PROC down the the [name type binding] triples provided by STRUCT.

(define (for-each-export proc struct)
  (let ((int (structure-interface struct)))
    (for-each-declaration
        (lambda (name base-name want-type)
	  (let ((binding (real-structure-lookup struct base-name want-type #t)))
	    (proc name
		  (if (and (binding? binding)
			   (eq? want-type undeclared-type))
		      (let ((type (binding-type binding)))
			(if (variable-type? type)
			    (variable-value-type type)
			    type))
		      want-type)
		  binding)))
	int)))

; --------------------
; Packages

(define-record-type package :package
  (really-make-package uid
		       opens-thunk opens accesses-thunk
		       definitions
		       undefineds
		       undefined-but-assigneds
		       get-location
		       cached
		       clients
		       unstable?
		       integrate?
		       file-name reader clauses loaded?)
  package?
  (uid	           package-uid)
  ;; #f if not initialized, then list of structures
  (opens           package-opens-really set-package-opens!)
  ;; name-table name -> binding
  (definitions     package-definitions)
  (unstable?       package-unstable?)
  ;; value of integrate clause; use integration in this packages
  (integrate?      package-integrate? set-package-integrate?!)

  ;; For EVAL and LOAD (which can only be done in unstable packages)
  ;; package name -> location
  (get-location    package-get-location set-package-get-location!)
  (file-name       package-file-name)
  (reader          package-reader set-package-reader!)
  (clauses         package-clauses)
  (loaded?         package-loaded? set-package-loaded?!)
  ;; compiler environment
  (env             package->environment set-package->environment!)

  ;; For package mutation
  (opens-thunk     package-opens-thunk set-package-opens-thunk!)
  ;; thunk -> (list (pair name struct))
  (accesses-thunk  package-accesses-thunk)
  ;; locations introduced for missing values
  ;; name-table name -> location
  (undefineds      package-real-undefineds set-package-undefineds!)
  ;; locations introduced for missing cells
  ;; name-table name -> location
  (undefined-but-assigneds
                   package-real-undefined-but-assigneds
		   set-package-undefined-but-assigneds!)
  (clients         package-clients)
  ;; locations used here that were supposed to have been provided by someone else
  ;; name-table name -> place, see binding.scm
  (cached	   package-cached))

(define-record-discloser :package
  (lambda (package)
    (let ((name (package-name package)))
      (if name
	  (list 'package (package-uid package) name)
	  (list 'package (package-uid package))))))

(define (make-package opens-thunk accesses-thunk unstable? tower file clauses
		      uid name)
  (let ((new (really-make-package
	       (if uid
		   (begin (if (>= uid *package-uid*)
			      (set! *package-uid* (+ uid 1)))
			  uid)
		   (new-package-uid))
	       opens-thunk
	       #f			;opens
	       accesses-thunk		;thunk returning alist
	       (make-name-table)	;definitions
	       #f			;undefineds
	       #f			;undefined-but-assigned
	       (fluid-cell-ref $get-location)
					;procedure for making new locations
	       (make-name-table)	;bindings cached in templates
	       (make-population)	;structures
	       unstable?		;unstable (suitable for EVAL)?
	       #t			;integrate?
	       file			;file containing DEFINE-STRUCTURE form
	       read
	       clauses			;misc. DEFINE-STRUCTURE clauses
	       #f)))			;loaded?
    (note-package-name! new name)
    (set-package->environment! new (really-package->environment new tower))
    new))

; TOWER is a promise that is expected to deliver, when forced, a
; pair (eval . env).

(define (really-package->environment package tower)
  (make-compiler-env (lambda (name)
		       (package-lookup package name))
		     (lambda (name type . maybe-static)
		       (cond
			((and (symbol? name) ; generated names are hopefully of no interest here
			      (opened-structure-for-name package name))
			 => (lambda (struct)
			      (warning 'package-define!
				       "name from opened structure redefined"
				       package name struct))))
		       (package-define! package
					name
					type
					#f
					(if (null? maybe-static)
					    #f
					    (car maybe-static))))
		     tower
		     package))	; interim hack

(define (opened-structure-for-name package name)
  (let loop ((opens (package-opens-really package)))
    (cond
     ((null? opens)
      #f)
     ((structure-lookup (car opens) name #t)
      (car opens))
     (else
      (loop (cdr opens))))))

; Two tables that we add lazily.

(define (lazy-table-accessor slot-ref slot-set!)
  (lambda (package)
    (or (slot-ref package)
	(let ((table (make-name-table)))
	  (slot-set! package table)
	  table))))

(define package-undefineds
  (lazy-table-accessor package-real-undefineds
		       set-package-undefineds!))

(define package-undefined-but-assigneds
  (lazy-table-accessor package-real-undefined-but-assigneds
		       set-package-undefined-but-assigneds!))

; Unique id's

(define (new-package-uid)
  (let ((uid *package-uid*))		;unique identifier
    (set! *package-uid* (+ *package-uid* 1))
    uid))

(define *package-uid* 0)

; Package names

(define package-name-table (make-table))

(define (package-name package)
  (table-ref package-name-table (package-uid package)))

(define (note-package-name! package name)
  (if name
      (let ((uid (package-uid package)))
	(if (not (table-ref package-name-table uid))
	    (table-set! package-name-table uid name)))))

(define (package-opens package)
  (initialize-package-if-necessary! package)
  (package-opens-really package))

(define (initialize-package-if-necessary! package)
  (if (not (package-opens-really package))
      (initialize-package! package)))

(define (package-accesses package)		;=> alist
  ((package-accesses-thunk package)))

; --------------------
; A simple package has no ACCESSes or other far-out clauses.

(define (make-simple-package opens unstable? tower . name-option)
  (if (not (list? opens))
      (assertion-violation 'make-simple-package "invalid package opens list" opens))
  (let ((package (make-package (lambda () opens)
			       (lambda () '()) ;accesses-thunk
			       unstable?
			       tower
			       ""	;file containing DEFINE-STRUCTURE form
			       '()	;clauses
			       #f	;uid
			       (if (null? name-option)
				   #f
				   (car name-option)))))
    (set-package-loaded?! package #t)
    package))

; --------------------
; The definitions table

; Each entry in the package-definitions table is a binding.

(define (package-definition package name)
  (initialize-package-if-necessary! package)
  (let ((probe (table-ref (package-definitions package) name)))
    (if probe
	(maybe-fix-place! probe)
	#f)))

(define (package-define! package name type place static)
  (let ((probe (table-ref (package-definitions package) name)))
    (if probe
	(begin
	  (clobber-binding! probe type place static)
	  (binding-place (maybe-fix-place! probe)))
	(let ((place (or place (get-new-location package name))))
	  (table-set! (package-definitions package)
		      name
		      (make-binding type place static))
	  place))))

(define (package-add-static! package name static)
  (let ((probe (table-ref (package-definitions package) name)))
    (if probe
	(clobber-binding! probe
			  (binding-type probe)
			  (binding-place probe)
			  static)
	(assertion-violation 'package-add-static!
			     "internal error: name not bound" package name))))

(define (package-refine-type! package name type)
  (let ((probe (table-ref (package-definitions package) name)))
    (if probe
	(clobber-binding! probe
			  type
			  (binding-place probe)
			  (binding-static probe))
	(assertion-violation 'package-refine-type!
			     "internal error: name not bound" package name))))

; --------------------
; Lookup

; Look up a name in a package.  Returns a binding if bound or #F if not.

(define (package-lookup package name)
  (really-package-lookup package name (package-integrate? package)))

(define (really-package-lookup package name integrate?)
  (let ((probe (package-definition package name)))
    (cond (probe
	   (if integrate?
	       probe
	       (forget-integration probe)))
	  ((generated? name)
	   ; Access path is (generated-parent-name name)
	   (generic-lookup (generated-env name)
			   (generated-name name)))
	  (else
	   (search-opens (package-opens-really package) name integrate?)))))

; Look for NAME in structures OPENS.

(define (search-opens opens name integrate?)
  (let loop ((opens opens))
    (if (null? opens)
	#f
	(or (structure-lookup (car opens) name integrate?)
	    (loop (cdr opens))))))

(define (structure-lookup struct name integrate?)
  (call-with-values
    (lambda ()
      (interface-ref (structure-interface struct) name))
    (lambda (base-name type)
      (if type
	  (real-structure-lookup struct base-name type integrate?)
	  #f))))

(define (real-structure-lookup struct name type integrate?)
  (impose-type type
	       (really-package-lookup (structure-package struct)
				      name
				      integrate?)
	       integrate?))

(define (generic-lookup env name)
  (cond ((package? env)
	 (package-lookup env name))
	((structure? env)
	 (or (structure-lookup env
			       name
			       (package-integrate? (structure-package env)))
	     (assertion-violation 'generic-lookup "not exported" env name)))
	((compiler-env? env)
	 (lookup env name))
	(else
	 (assertion-violation 'generic-lookup "invalid environment" env name))))

; --------------------
; Package initialization

(define (initialize-package! package)
  (let ((opens ((package-opens-thunk package))))
    (set-package-opens! package opens)
    (check-for-duplicates! package)
    (for-each (lambda (struct)
		(if (structure-unstable? struct)
		    (add-to-population! package (structure-clients struct))))
	      opens))
  (for-each (lambda (name+struct)
	      ;; Cf. CLASSIFY method for STRUCTURE-REF
	      (package-define! package 
			       (car name+struct)
			       structure-type
			       #f
			       (cdr name+struct)))
	    (package-accesses package)))

(define (check-for-duplicates! package)
  (let ((imported-names (make-symbol-table)) ; maps names to pair of first binding, lists of structures
	(duplicates '()))
    (for-each (lambda (struct)
		(for-each-export 
		 (lambda (name type binding)
		   (cond
		    ((table-ref imported-names name)
		     => (lambda (p)
			  (if (not (same-denotation? (car p) binding))
			      (begin
				(set! duplicates (cons name duplicates))
				(if (not (memq struct (cdr p)))
				    (set-cdr! p (cons struct (cdr p))))))))
		    (else
		     (table-set! imported-names name (cons binding (list struct))))))
		 struct))
	      (package-opens package))
    (for-each (lambda (duplicate)
		(apply warning 'check-for-duplicates!
		       "duplicate name in opened structure"
		       duplicate
		       package
		       (cdr (table-ref imported-names duplicate))))
	      duplicates)))

; (define (package->environment? env)
;   (eq? env (package->environment
;	        (extract-package-from-comp-env env))))


; --------------------
; For implementation of INTEGRATE-ALL-PRIMITIVES! in scanner, etc.

(define (for-each-definition proc package)
  (table-walk (lambda (name binding)
		(proc name (maybe-fix-place! binding)))
	      (package-definitions package)))

; --------------------
; Locations

(define (get-new-location package name)
  ((package-get-location package) package name))

; Default new-location method for new packages

(define (make-new-location package name)
  (let ((uid *location-uid*))
    (set! *location-uid* (+ *location-uid* 1))
    (table-set! location-info-table uid
		(make-immutable!
		 (cons (name->symbol name) (package-uid package))))
    (make-undefined-location uid)))

(define $get-location (make-fluid (make-cell make-new-location)))

(define *location-uid* 5000)  ; 1510 in initial system as of 1/22/94

(define location-info-table (make-table))


(define (flush-location-names)
  (set! location-info-table (make-table))
  ;; (set! package-name-table (make-table)) ;hmm, not much of a space saver
  )

; (put 'package-define! 'scheme-indent-hook 2)


