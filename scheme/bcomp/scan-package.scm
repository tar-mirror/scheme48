; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber

; Scanning structures and processing package clauses.

; Utility for compile-structures (link/link.scm) and
; ensure-loaded (env/load-package.scm).
;
; Returns a list of all packages reachable from STRUCTS that answer true to
; INCLUDE-THIS-PACKAGE?.

(define (collect-packages structs include-this-package?)
  (let ((package-seen '())
	(structure-seen '())
	(packages '()))
    (letrec ((recur
	      (lambda (structure visited)
		(if (memq (structure-package structure) visited)
		    (warning 'collect-packages "cycle in structures dependencies"
			     structure visited))
		(if (not (memq structure structure-seen))
		    (begin
		      (set! structure-seen (cons structure structure-seen))
		      (let ((package (structure-package structure)))
			(if (not (memq package package-seen))
			    (begin
			      (set! package-seen (cons package package-seen))
			      (if (include-this-package? package)
				  (let ((visited (cons package visited)))
				    (for-each (lambda (struct)
						(recur struct visited))
					      (package-opens package))
				    (for-each (lambda (name+struct)
						(recur (cdr name+struct) visited))
					      (package-accesses package))
				    (set! packages (cons package packages))))))))))))
      (for-each (lambda (struct)
		  (recur struct '()))
		structs)
      (reverse packages))))

; Walk through PACKAGE's clauses to find the source code.  The relevant
; clauses are:
;   (files name ...)
;   (begin form ...)
;   (define-all-operators)
;   (usual-transforms name ...)
;
; Returns a list of pairs (file . (node1 node2 ...)), a list of names
; of standard transforms, and a boolean value which is true if the package
; is to include definitions of all primitives.

(define (package-source package)
  (let* ((config-file (package-file-name package))
	 (dir (if config-file
		  (file-name-directory config-file)
		  #f)))
    (call-with-values
	(lambda ()
	  (fold->3 (lambda (clause stuff transforms primitives?)
		     (case (car clause)
		       ((files)
			(values (read-files (cdr clause) stuff dir package)
				transforms
				primitives?))
		       ((begin)
			(values (cons (cons config-file (cdr clause))
				      stuff)
				transforms
				primitives?))
		       ((integrate)
			(set-package-integrate?! package
						 (or (null? (cdr clause))
						     (cadr clause)))
			(values stuff transforms primitives?))
		       ((optimize)
			(values stuff transforms primitives?))
		       ((define-all-operators)
			(values stuff transforms #t))
		       ((usual-transforms)
			(values stuff
				(append (reverse (cdr clause)) transforms)
				primitives?))
		       ((reader)
			(let ((r (force (comp-env-macro-eval (package->environment package)))))
			  (set-package-reader! package ((car r) (cadr clause) (cdr r))))
			(values stuff transforms primitives?))
		       (else
			(assertion-violation 'package-source
					     "unrecognized define-structure keyword"
					     clause))))
		   (package-clauses package)
		   '() '() #f))
      (lambda (stuff transforms primitives?)
	(values (reverse stuff)
		(reverse transforms)
		primitives?)))))

; Also prints out the filenames (courtesy of READ-FORMS).

(define (read-files all-files stuff dir package)
  (force-output (current-output-port))		; just to be nice
  (fold (lambda (filespec stuff)
	  (let ((file (namestring filespec
				  dir
				  *scheme-file-type*)))
	    (display #\space (current-noise-port))
	    (cons (cons file (read-forms file package #f))
		  stuff)))
	all-files
	stuff))

(define (package-optimizer-names package)
  (if (package-integrate? package)
      (let ((opts (apply append
			 (map cdr (filter (lambda (clause)
					    (eq? (car clause) 'optimize))
					  (package-clauses package))))))
	(reduce (lambda (name opts)
		  (if (memq name opts)
		      opts
		      (cons name opts)))
		opts
		'()))
      '()))

(define (check-structure structure)
  (let ((undefined '()))
    (for-each-export
         (lambda (name want-type binding)
	   (if (binding? binding)
	       (let ((have-type (binding-type binding)))
		 (if (not (compatible-types? have-type want-type))
		     (warning 'check-structure
			      "Type in interface doesn't match binding"
			      name
			      `(binding: ,(type->sexp have-type #t))
			      `(interface: ,(type->sexp want-type #t))
			      structure)))
	       (set! undefined (cons name undefined))))
	 structure)
    (if (not (null? undefined))
	(warning 'check-structure
		 "Structure has undefined exports"
		 structure
		 undefined))))

