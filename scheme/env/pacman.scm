; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Robert Ransom,
; Marcus Crestani, Sebastian Rheinnecker

; PACkage-manipulation comMANds

(add-sentinel! package-system-sentinel)

(define (set-environment-for-commands! p)
  (set-interaction-environment! p)
  ;; (set-command-level-env! (command-level) p)
  )

(define user-environment
  (user-context-accessor 'user-environment interaction-environment))
(define set-user-environment!
  (user-context-modifier 'user-environment))

(define-command-syntax 'in "<struct> [<command>]"
  "go to package, or execute single command in package"
  '(name &opt command))

(define (in name . maybe-command)
  (if (and (not (null? maybe-command))
	   (command-just-evaluates-symbol? (car maybe-command)))
      (set-command-results! (list (environment-ref (really-get-package name)
						   (cadr (car maybe-command)))))
      (in-package (get-package name) maybe-command)))

(define (command-just-evaluates-symbol? command)
  (and (pair? command)
       (eq? (car command) 'run)
       (symbol? (cadr command))))

(define-command-syntax 'new-package "[<struct> ...]" "make and enter a new package"
  '(&rest expression))

(define (new-package . maybe-opens)
  (let* ((opens-thunk
          (if (pair? maybe-opens)
	      list
              (lambda ()
                (list (get-structure 'scheme)))))
         (p (make-package opens-thunk
                          (lambda () '())
                          #t    ;unstable?
                          (get-syntactic-tower (user-environment))
                          ""    ;file containing DEFINE-STRUCTURE form
                          '()   ;clauses
                          #f    ;uid
                          #f))) ;name
    (set-package-integrate?! p
			     (package-integrate? (environment-for-commands)))
    (set-environment-for-commands! p)
    (for-each open maybe-opens)))

(define (get-syntactic-tower env)    ;Returns promise of (eval . env)
  (comp-env-macro-eval (if (package? env)
			   (package->environment env)
			   env)))	;Mumble


; load-package

(define-command-syntax 'load-package "<struct>" "load package's source code"
  '(name))

(define (load-package name)
  (quietly-ensure-loaded (get-structure name)))

(define-command-syntax 'reload-package "<struct>" "load package's source code again"
  '(name))

(define (reload-package name)
  (let ((s (get-structure name)))
    (if (not (package-unstable? (structure-package s)))
	(assertion-violation 'reload-package "read-only structure" s))
    (set-package-loaded?! (structure-package s) #f)
    (quietly-ensure-loaded s)))

(define-command-syntax 'structure "<name> <interface>"
  "create new structure over the current package"
  '(name expression))

(define (structure name interface-expression)
  (let* ((c (config-package))
	 (p (environment-for-commands))
	 (s (make-structure p
			    (lambda ()
			      (eval interface-expression c))
			    name)))
    ;; (check-structure s)
    (environment-define! c name s)))

(define-command-syntax 'open "<struct> ..." "open a structure"
  '(&rest expression))

(define (open . specs)
  (for-each (lambda (spec)
              (let* ((c (config-package))
                     (thunk (lambda ()
                              (eval spec c)))
                     (probe (thunk)))
                (if (structure? probe)
                    (if (ensure-loaded-query probe)
                        (package-open! (environment-for-commands) thunk)
                        (assertion-violation 'open "structure not loaded" spec))
                    (assertion-violation 'open "not a structure" spec))))
            specs))

(define (ensure-loaded-query struct)
  (let ((p (structure-package struct)))
    (cond ((or (package-loaded? p)
	       (and (null? (package-clauses p))
		    (every (lambda (struct)
			     (package-loaded? (structure-package struct)))
			   (package-opens p))))
	   #t)
	  ((or (batch-mode?)
	       (not (ask-before-loading?))
	       (y-or-n? (if (structure-name struct)
			    (string-append "Load structure "
					   (symbol->string
					    (structure-name struct)))
			    "Load structure")
			#f))
	   (quietly-ensure-loaded struct)
	   #t)
	  (else #f))))

; Loading or asking is controlled by the user context.

(define ask-before-loading?
  (user-context-accessor 'ask-before-loading?
			 (lambda () #f)))

(define set-ask-before-loading?!
  (user-context-modifier 'ask-before-loading?))

(add-setting 'ask-before-loading #t
	     ask-before-loading?
	     set-ask-before-loading?!
	     "will ask before loading modules"
	     "will not ask before loading modules")

(define-command-syntax 'for-syntax "[<command>]"
  "go to current package's package for syntax"
  '(&opt command))

(define (for-syntax . maybe-command)
  (in-package (cdr (force (get-syntactic-tower (environment-for-commands))))
    maybe-command))


; ,user  goes to the user initial environment.

(define-command-syntax 'user "[<command>]" "go to user package"
  '(&opt command))

(define (user . maybe-command)
  (in-package (user-environment) maybe-command))

(define-command-syntax 'user-package-is "[<struct>]"
  "designate user package (for ,user command)"
  '(&opt name))

(define (user-package-is . name-option)
  (set-user-environment! (if (null? name-option)
			     (environment-for-commands)
			     (get-package (car name-option)))))

(define set-user-environment!
  (user-context-modifier 'user-environment))


; Configuration package  (should there be ,load-config as well?)

(define-command-syntax 'config "[<command>]" "go to configuration package"
  '(&opt command))

(define (config . maybe-command)
  (in-package (config-package) maybe-command))

(define-command-syntax 'config-package-is "<struct>"
  "designate configuration package"
  '(name))

(define (config-package-is name)
  (set-config-package! (get-package name)))


; ,exec  goes to the exec initial environment.

(define-command-syntax 'exec "[<command>]" "go to command execution package"
  '(&opt command))

(define (exec . maybe-command)
  (in-package (user-command-environment) maybe-command))


; ,undefine foo  removes definition of foo from current package.

(define-command-syntax 'undefine "<name>" "remove definition"
  '(name))

(define (undefine name)
  (package-undefine! (interaction-environment) name))

; ,set-reader changes the reader of the current package.

(define-command-syntax 'set-reader "<reader>" "sets the current reader"
  '(expression))

(define (set-reader reader-expression)
  (let* ((p (environment-for-commands))
	 (r (eval reader-expression p)))
    (set-reader! p r)
    (set-package-reader! p r)))

; --------------------
; Auxiliaries for package commands

(define (in-package p maybe-command)
  (if (null? maybe-command)
      (set-environment-for-commands! p)
      (with-interaction-environment p
	(lambda ()
	  (let ((command (car maybe-command)))
	    (if (procedure? command)
		(command)
		(execute-command (car maybe-command))))))))

(define (quietly-ensure-loaded s)
  (if (load-noisily?)
      (ensure-loaded s)
      (silently (lambda ()
		  (ensure-loaded s)))))

; The initial value used to be user-environment, but that required that the
; user context be created and initialized separately.

(define config-package
  (user-context-accessor 'config-package interaction-environment))

(define set-config-package!
  (user-context-modifier 'config-package))

(define (get-package name)
  (let ((p (really-get-package name)))
    (if (package-unstable? p)
	p
	(assertion-violation 'get-package "read-only structure" p))))

(define (really-get-package name)
  (let ((s (get-structure name)))
    (ensure-loaded-query s)
    (structure-package s)))

(define (get-structure name)
  (let ((thing (environment-ref (config-package) name)))
    (cond ((structure? thing) thing)
	  (else (assertion-violation 'get-structure "not a structure" name thing)))))

; Main entry point, with package setup.

(define (new-command-processor info commands built-in . meta-structs)
  ;; Argument to ,build command
  (lambda (arg)
    (call-with-values (lambda ()
			(make-user-envs commands built-in meta-structs))
      (lambda (env init-thunk)
	(with-interaction-environment
	 env
	 (lambda ()
	   (start-command-processor arg
				    (lambda ()
				      (greet-user info))
				    init-thunk)))))))

(define (make-user-envs commands built-in meta-structs)
  (let* ((tower (make-syntactic-tower
		      eval
		      (list (*structure-ref built-in 'scheme))
		      'user))
	 (user (make-user-package built-in tower))
	 (config-package (make-config-package 'config
					      tower
					      built-in
					      meta-structs))
	 (exec-package (make-exec-package commands tower built-in)))
    (values
     user
     (lambda ()
       (set-user-environment! user)
       (set-config-package! config-package)
       (set-user-command-environment! exec-package)))))

; User package

(define (make-user-package built-in tower)
  (let* ((scheme-structure (*structure-ref built-in 'scheme))
	 (user
	  (make-simple-package (list scheme-structure)
			       #t  ;unstable?
			       tower
			       'user)))
    (set-package-integrate?! user #f)
    user))

; Configuration package

(define (make-config-package name tower built-in meta-structs)
  (let* ((module-system (*structure-ref built-in 'module-system))
	 (config
	  (make-simple-package (cons module-system
				     (append meta-structs
					     (list built-in)))
			       #t  ;unstable?
			       tower
			       name)))
    (set-syntactic-tower-maker!
         config
	 (lambda (clauses id)
	   (if (null? clauses)
	       tower			;?
	       (delay (let ((p (eval `(a-package ((for-syntax ,id)) ,@clauses)
				     config)))
			(quietly-ensure-loaded (make-structure
						p
						(lambda ()
						  (make-simple-interface #f '()))
						'for-syntax))
			(cons eval p))))))
    (set-reader! config read)
    config))

; Exec package

(define (make-exec-package commands tower built-in)
  (make-simple-package (list commands
			     ;; we want the `load' from `usual-commands'
			     (make-modified-structure (*structure-ref built-in 'scheme)
						      '((hide load))))
		       #t		;unstable?
		       tower
		       'exec))

; for prompt string

(define-method &environment-id-string ((env :package))
  (if (eq? env (user-environment))
      ""
      (if (symbol? (package-name env))
	  (symbol->string (package-name env))
	  (number->string (package-uid env)))))

(define user-environment
  (user-context-accessor 'user-environment interaction-environment))

; This is only used by misc/remote.scm, which I don't know if anyone uses.
;
; Extract a package-specific evaluator from a package.  Eventually, it
; would be nice if load, eval-from-file, eval-scanned-forms, and
; perhaps other things were also generic over different kinds of
; environments.
;
;(define funny-name/evaluator (string->symbol ".evaluator."))
;
;(define (set-package-evaluator! p evaluator)
;  (package-define-funny! p funny-name/evaluator evaluator))
;
;(define (package-evaluator p)
;  (or (get-funny (package->environment p) funny-name/evaluator) eval))
;
;(define-method &evaluate (form (env :package))
;  ((package-evaluator env) form env))


; Show information about packages

;; prints known packages
(define-command-syntax 'show-known-packages "" 
  "shows all known packages" '())

(define (show-known-packages)
  (for-each (lambda (entry)
              (write entry)
              (newline)) 
            (filter symbol? (table->entry-list package-name-table))))

;; prints exported names
(define-command-syntax 'show-interface "<struct> ..." 
  "shows exported names of the given structures" '(&rest form))

(define (show-interface . structs)
  (if (null? structs)
      '?
      (let ((names '()))
        (for-each 
	 (lambda (struct)
	   (let ((probe (and (package-lookup (config-package) struct)
			     (eval struct (config-package)))))
	     (if (structure? probe)
		 (begin
		   (for-each-declaration
		     (lambda (name package-name type)
		       (if (not (assq name names))  ; compound signatures...
			   (set! names (cons (cons name type) 
                                             names))))
		     (structure-interface probe))
		   (for-each (lambda (pair)
                               (let ((name (car pair))
                                     (type (cdr pair)))
			         (write name)
			         (write-char #\space)
                                 (write (careful-type->sexp type))
                                 (newline)))
			     names))
		 (begin 
		   (display "[")
		   (write struct) 
		   (display " not found]")
		   (write-char #\space)
                   (newline)))))
	 structs))))

(define (careful-type->sexp thing)
  (cond ((not thing) 'undefined)
	((or (symbol? thing)
	     (null? thing)
	     (number? thing))
	 thing)     ;?
	((pair? thing)    ;e.g. (variable #{Type :value})
	 (cons (careful-type->sexp (car thing))
	       (careful-type->sexp (cdr thing))))
	(else
	 (type->sexp thing #t))))
