; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber


; Commands for writing images.

; A heap image written using ,dump or ,build can be invoked with
;    s48 -i <filename> [-h <heap size>] [-a <argument>]
; For images made with ,build <exp> <filename>, <argument> is passed as
; a string to the procedure that is the result of <exp>.


; dump <filename>

(define-command-syntax 'dump "<filename>"
  "write the current heap to an image file"
  '(filename &opt form))

(define (dump filename . maybe-info)
  (let ((info (if (null? maybe-info) "(suspended image)" (car maybe-info)))
	(context (user-context))
	(env (environment-for-commands)))
    (build-image #f
		 (lambda (arg)
		   (with-interaction-environment env
		     (lambda ()
		       (restart-command-processor arg
						  context
						  (lambda ()
						    (greet-user info))
						  values))))
		 filename)))

; build <exp> <filename>

(define-command-syntax 'build "<exp> <filename> <option> ..."
  "build a heap image file with <exp> as entry procedure, <option> can be no-warnings"
  '(expression filename &rest name))

(define (build exp filename . options)
  (build-image (not (memq 'no-warnings options))
	       (eval exp (environment-for-commands))
	       filename))

(define (build-image no-warnings? start filename)
  (let ((filename (translate filename)))
    (write-line (string-append "Writing " filename) (command-output))
    (write-image (x->os-byte-vector filename)
		 (stand-alone-resumer no-warnings? start)
		 (string->os-byte-vector ""))
    #t))

(define (stand-alone-resumer warnings? start)
  (make-usual-resumer  ;sets up exceptions, interrupts, and current input & output
   warnings?
   (lambda (arg)
     (call-with-current-continuation
       (lambda (halt)
	 (with-handler (simple-condition-handler halt (current-error-port))
	   (lambda ()
	     (start arg))))))))

; Simple condition handler for stand-alone programs.

(define (simple-condition-handler halt port)
  (lambda (c punt)
    (cond ((violation? c)
	   (display-condition c port)
	   (halt 3))
	  ((serious-condition? c)
	   (display-condition c port)
	   (halt 1))
	  ((warning? c)
	   (display-condition c port))	;Proceed
	  ((interrupt-condition? c)
	   ;; (and ... (= (cadr c) interrupt/keyboard)) ?
	   (halt 2))
	  (else
	   (punt)))))

;(define interrupt/keyboard (enum interrupt keyboard))
