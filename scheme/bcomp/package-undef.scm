; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Taylor Campbell

; The entry point for all this.

(define (link! template package noise?)
  (if noise?
      (noting-undefined-variables package
				  (lambda ()
				    (really-link! template package)))
      (really-link! template package)))

(define (really-link! template package)  
  (do ((i 0 (+ i 1)))
      ((= i (template-length template)))
    (let ((x (template-ref template i)))
      (cond ((thingie? x)
	     (template-set! template
			    i
			    (get-location (thingie-binding x)
					  package
					  (thingie-name x)
					  (thingie-assigned? x))))
	    ((template? x)
	     (really-link! x package))))))

; GET-LOCATION returns a location to be stored away in a template.  If
; ASSGINED? (which is #t if the variable is SET!) matches the type of
; the binding, then we just return the binding's location after noting
; that we are doing so.
;
; If the type doesn't match then we make a location and remember that we
; have done so.  If correct location becomes available later we will replace
; the bogus one (see env/pedit.scm).

(define (get-location binding cenv name assigned?)
  (cond
   ((binding? binding)
    (let ((place (binding-place binding))
	  (type (binding-type binding)))
      (cond
       ((variable-type? type)
	(note-caching! cenv name place)
	place)
       (assigned?
	(get-location-for-unassignable cenv name))
       ((value-type? type)
	(note-caching! cenv name place)
	place)
       (else
	(warning 'get-location "invalid variable reference" name cenv)
	(note-caching! cenv name place)
	place))))
    (assigned?
     (get-location-for-undefined cenv name location-for-assignment))
    (else
     (get-location-for-undefined cenv name location-for-reference))))

; Packages have three tables used by env/pedit.scm:
;   undefineds
;     locations introduced for missing values
;   undefined-but-assigneds
;     locations introduced for missing cells
;   cached
;     locations used here that were supposed to have been provided by
;     someone else

; Get a location from PACKAGE's table at ACCESSOR.

(define (location-on-demand accessor)
  (lambda (package name)
    (let ((table (accessor package)))
      (or (table-ref table name)
	  (let ((new (make-new-location package name)))
	    (table-set! table name new)
	    new)))))

(define get-undefined
  (location-on-demand package-undefineds))

(define get-undefined-but-assigned
  (location-on-demand package-undefined-but-assigneds))

; If this package is mutable and it gets NAME from some other structure
; then add NAME and PLACE to its table of cached locations.

(define (package-note-caching! package name place)
  (if (package-unstable? package)
      (if (not (table-ref (package-definitions package) name))
	  (let loop ((opens (package-opens package)))
	    (if (not (null? opens))
		(call-with-values
		    (lambda ()
		      (interface-ref (structure-interface (car opens))
				     name))
		  (lambda (internal-name type)
		    (if internal-name
			(begin
			  (table-set! (package-cached package) name place)
			  (package-note-caching!
			   (structure-package (car opens))
			   internal-name place))
                        (loop (cdr opens))))))))))

; Find the actual package providing PLACE and remember that it is being used.

(define (note-caching! cenv name place)
  (if (generated? name)
      (note-caching! (generated-env name)
		     (generated-name name)
		     place)
      (let ((package (cenv->package cenv)))
        (if (package? package)
            (package-note-caching! package name place)))))

; Get a location for NAME which is SET! but isn't supposed to be.

; In this case, we're creating a new, local location for the
; assignment to happen - the original location isn't touched.

(define (get-location-for-unassignable cenv name)
  (if (generated? name)
      (get-location-for-unassignable (generated-env name)
				     (generated-name name))
      (let ((package (cenv->package cenv)))
	(warning 'get-location-for-unassignable "invalid assignment" name)
	(if (package? package)
	    (lambda () (get-undefined-but-assigned package name))
	    (lambda () (make-undefined-location name))))))

; Get a location for NAME, which is undefined.

(define (get-location-for-undefined cenv name get-location)
  (if (generated? name)
      (get-location-for-undefined (generated-env name)
				  (generated-name name)
				  get-location)
      (let ((package (cenv->package cenv)))
	((or (fluid $note-undefined)
	     (lambda (cenv name) (values)))
	   cenv
	   name)
        (if (package? package)
	    (let ((place (get-location package name)))
	      (package-note-caching! package name place)
	      place)
	    (make-undefined-location name)))))
	    
(define $note-undefined (make-fluid (lambda (cenv name) (values))))

; Because of generated names CENV may not be an actual compile-env

(define (cenv->package cenv)
  (cond ((compiler-env? cenv)
         ;; This returns #f if package is stable (static linking).
         (extract-package-from-comp-env cenv))
        ((package? cenv)
	 cenv)
        ((structure? cenv)
	 (structure-package cenv))))

; Exported for env/pedit.scm.

(define (location-for-reference package name)
  (get-undefined-location package name get-undefined #f))

(define (location-for-assignment package name)
  (get-undefined-location package name get-undefined-but-assigned #t))

(define (get-undefined-location package name get-undefined warn-assignment?)
  (let location ((package package)
		 (name name)
		 (warn? warn-assignment?))
    (let loop ((opens (package-opens package)))
      (if (null? opens)
	  (get-undefined package name)
	  (call-with-values
	      (lambda ()
		(interface-ref (structure-interface (car opens))
			       name))
	    (lambda (internal-name type)
	      (if internal-name
		  (begin
		    (if warn?
			(warning 'location-for-assignment "invalid assignment" name))
		    (location (structure-package (car opens))
			      internal-name
			      #f))
		  (loop (cdr opens)))))))))

;----------------
; Maintain and display a list of undefined names.
; We bind $NOTE-UNDEFINED to a procedure that adds names to LOSERS, an a-list of
; (<package> . <undefined-names>).  Once THUNK has finished we print out any
; names that are still undefined.  CURRENT-PACKAGE is used only to avoid printing
; out the name of the current package unnecessarily.  CURRENT-PACKAGE may be #f.

(define (noting-undefined-variables current-package thunk)
  (let* ((losers '())
	 (add-name (lambda (env name)
		     (let ((probe (assq env losers)))
		       (if probe
			   (if (not (member name (cdr probe)))
			       (set-cdr! probe (cons name (cdr probe))))
			   (set! losers (cons (list env name) losers)))))))
    (let-fluid $note-undefined
        (lambda (env name)
	  (if (generated? name)
	      (add-name (generated-env name)
			(generated-name name))
	      (add-name env name)))
      (lambda ()
	(dynamic-wind
	 (lambda () #f)
	 thunk
	 (lambda ()
	   (for-each (lambda (env+names)
		       (print-undefined-names (car env+names)
					      (cdr env+names)
					      current-package))
		     losers)))))))

(define (print-undefined-names env names current-package)
  (let ((names (filter (lambda (name)
			 ;; Keep the ones that are still unbound.
			 (not (generic-lookup env name)))
		       names)))
    (if (not (null? names))
	(let ((names (map (lambda (name)
			    (if (generated? name)
				(generated-name name)
				name))
			  (reverse names))))
	  (apply warning
		 'print-undefined-names
		 "undefined variables"
		 env
		 names)))))
	      
;        (let ((out (current-noise-port)))
;          (newline out)
;          (display "Undefined" out)
;          (if (and current-package
;                   (not (eq? env current-package)))
;              (begin (display " in " out)
;                     (write env out)))
;          (display ": " out)
;          (write (map (lambda (name)
;                        (if (generated? name)
;                            (generated-name name)
;                            name))
;                      (reverse names))
;                 out)
;          (newline out)))))
