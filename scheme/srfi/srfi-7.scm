; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Martin Gasbichler, Mike Sperber


; A command for loading SRFI-7 programs.  This is a user command because
; it gets loaded after the command processor is built.

(define-user-command-syntax 'load-srfi-7-program "<name> <filename>"
  "load an SRFI-7 program"
  '(name filename))

(define-user-command-syntax 'load-srfi-7-script "<name> <filename>"
  "load an SRFI-7 script"
  '(name filename))

; We create a structure, EVAL it in the configuration package, give
; it a name, and then load it.

(define (load-srfi-7-program name filename)
  (load-srfi-7 name filename read-srfi-7-program))

(define (load-srfi-7-script name filename)
  (load-srfi-7 name filename read-srfi-7-script))

(define (load-srfi-7 name filename read)
  (eval-from-file `((define ,name ,(read filename)))
		  (config-package)
		  filename)
  (let ((structure (eval name (config-package))))
    (note-structure-name! structure name)
    (ensure-loaded structure)))

; Add the LOAD-SRFI-7-PROGRAM to the user's command environment.

(environment-define! (user-command-environment)
		     'load-srfi-7-program
		     load-srfi-7-program)

(environment-define! (user-command-environment)
		     'load-srfi-7-script
		     load-srfi-7-script)

; Read a program from FILENAME and return the code for a structure that
; contains it.

(define (read-srfi-7 filename script?)
  (call-with-input-file filename
    (lambda (in)
      (if script?
	  (skip-line in))
      (let ((program (read in)))
	(if (and (pair? program)
		 (eq? (car program) 'program))
	    (receive (needed source)
		(parse-program program available-srfis)
	      (if needed
		  (program->structure-exp needed source)
		  (assertion-violation 'read-srfi-7
				       "cannot satisfy program's requirements")))
	    (assertion-violation 'read-srfi-7 "program not found in file" filename))))))

(define (skip-line port)
  (let loop ()
    (let ((char (read-char port)))
      (if (and (not (eof-object? char))
	       (not (char=? #\newline char)))
	  (loop)))))

(define (read-srfi-7-program filename)
  (read-srfi-7 filename #f))

(define (read-srfi-7-script filename)
  (read-srfi-7 filename #t))

; Returns a STRUCTURE expression for a program that uses the SRFIs listed
; in NEEDED and whose source is SOURCE.

(define (program->structure-exp needed source)
  (let ((shadowed (find-shadowed needed)))
    `(structure (export)
		(open ,(if (null? shadowed)
			   'scheme
			   `(modify scheme (hide . ,shadowed)))
		      . ,needed)
		. ,(transform-source source))))

(define (transform-source source)
  (map (lambda (source)
         (if (eq? (car source)
                  'code)
             (cons 'begin (cdr source))
             source))
       source))

; Returns a list of the names that SRFIS redefine from Scheme.

(define (find-shadowed srfis)
  (apply append (map (lambda (srfi)
		       (cond ((assq srfi shadowed)
			      => cdr)
			     (else
			      '())))
		     srfis)))

;----------------
; Parsing a program to find the source that we will use for it.
;
; The arguments are a PROGRAM form and a list of the names of available SRFIs.
; Two values are returned: a list of needed SRFIs and the program source as
; a list of (files ...) and (code ...) clauses.
;
; This searches through the possible sets of SRFIs to find one that works for
; the program.  The EITHER macro is used to try choices.  There are only two
; places where choices occur: FEATURE-COND clauses and OR predicates (which
; includes (NOT (AND ...)) predicates).

(define (parse-program program available)
  (receive (needed available source)
      (with-nondeterminism
       (lambda ()
	 (either (process-clauses (cdr program) '() available)
		 (values #f #f #f))))
    (if needed
	(values needed (reverse source))
	(values #f #f))))

(define-syntax program
  (syntax-rules ()
    ((program clauses ...)
     (receive (needed source)
         (parse-program '(program clauses ...) available-srfis)
       (if needed
           (let ((shadowed (find-shadowed needed)))
             (if (not (null? shadowed))
                 (open-scheme-shadowed! shadowed))
             (for-each open-structure (if (null? shadowed)
                                          (cons 'scheme needed)
                                          needed))
             (for-each
              (lambda (exp) (eval exp (interaction-environment)))
              (transform-source source)))
           (assertion-violation 'program "cannot satisfy program's requirements"))))))

(define (open-structure name)
  (let* ((c (config-package))
         (struct (environment-ref c name)))
    (ensure-loaded struct)
    (package-open! (interaction-environment) (lambda () struct))))

(define (open-scheme-shadowed! shadowed)
  (let* ((c (config-package)))
    (package-open! (interaction-environment)
                   (lambda ()
                     (make-modified-structure 
                      (environment-ref c 'scheme)
                      `((hide ,@shadowed)))))))

; NEEDED is a list of SRFIs that we already know we need.  AVAILABLE is a list
; of other SRFIs that can be used.  This returns new needed and available lists
; as well as a list of (files ...) and (code ...) clauses.
;
; This is a simple dispatch on the types of clauses.  For REQUIRES and
; FEATURE-COND we call another procedure to check the requirements list
; or cond clauses.

(define (process-clauses clauses needed available)
  (let loop ((clauses clauses)
	     (needed needed)
	     (available available)
	     (source '()))
    (if (null? clauses)
	(values needed available source)
	(let ((clause (car clauses)))
	  (case (car clause)
	    ((requires)
	     (receive (needed available)
		 (check-predicate `(and . ,(cdr clause)) #f needed available)
	       (loop (cdr clauses)
		     needed
		     available
		     source)))
	    ((feature-cond)
	     (receive (needed available more-source)
		 (process-cond-clauses (cdr clause) needed available)
	       (loop (cdr clauses)
		     needed
		     available
		     (append more-source source))))
	    ((code files)
	     (loop (cdr clauses)
		   needed
		   available
		   (cons clause source)))
	    (else
	     (assertion-violation 'process-clauses "bad program clause" clause)))))))
  

; Loop down CLAUSES looking for one whose predicate can be satisfied.
; If we find one we process its clauses.  EITHER is used to allow us
; to backtrack in case of failure.

(define (process-cond-clauses clauses needed available)
  (let loop ((clauses clauses))
    (if (null? clauses)
	(fail)
	(either (receive (needed available)
		    (check-predicate (caar clauses) #f needed available)
		  (process-clauses (cdar clauses) needed available))
		(loop (cdr clauses))))))

; REQUIREMENT is one of:
;   (and <requirement> ...)
;   (or <requirement> ...)
;   (not <requirement>)
;   <srfi-name>
; NEGATE? is true if we really want the negation of REQUIREMENT.
;
; AND and OR have their own procedures, which get flipped by negation.
; NOT just flips NEGATE?.  There are two separate procedures for positive
; and negative occurances of <srfi-name>.

(define (check-predicate requirement negate? needed available)
  (cond ((pair? requirement)
	 (case (car requirement)
	   ((and)
	    ((if negate? any-satisfied all-satisfied)
	       (cdr requirement) negate? needed available))
	   ((or)
	    ((if negate? all-satisfied any-satisfied)
	       (cdr requirement) negate? needed available))
	   ((not)
	    (check-predicate (cadr requirement)
			     (not negate?)
			     needed
			     available))))
	(negate?
	 (do-not-want requirement needed available))
	(else
	 (want requirement needed available))))
	 
; We want SRFI.  If it is NEEDED we are fine.  If it is in AVAILABLE we
; move it to needed.  Otherwise we lose.

(define (want srfi needed available)
  (cond ((memq srfi needed)
	 (values needed available))
	((memq srfi available)
	 (values (cons srfi needed)
		 (delq srfi available)))	; not really necessary
	(else
	 (fail))))

; We do not want SRFI.  If it is NEEDED we lose.  If it is in AVAILABLE we
; get rid of it.  Otherwise we win as-is.

(define (do-not-want srfi needed available)
  (cond ((memq srfi needed)
	 (fail))
	((memq srfi available)
	 (values needed
		 (delq srfi available)))
	(else
	 (values needed available))))

; Two loops for `and' and `or'.  The `and' loop needs to update NEEDED
; and AVAILABLE as it goes, the `or' keeps reusing the originals.

(define (all-satisfied list negate? needed available)
  (let loop ((list list) (needed needed) (available available))
    (if (null? list)
	(values needed available)
	(receive (needed available)
	    (check-predicate (car list) negate? needed available)
	  (if needed
	      (loop (cdr list) needed available)
	      (fail))))))

; Again, we use EITHER to allow for backtracking.

(define (any-satisfied list negate? needed available)
  (let loop ((list list))
    (if (null? list)
	(fail)
	(either (check-predicate (car list) negate? needed available)
		(loop (cdr list))))))

;----------------
; Our own copy to avoid having to load BIG-UTIL to get the original.

(define (delq thing list)
  (cond ((null? list)
	 '())
	((eq? thing (car list))
	 (cdr list))
	(else
	 (cons (car list)
	       (delq thing (cdr list))))))

