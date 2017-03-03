; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Ivan Shmakov, Mike Sperber


; This file contains things that tie together the compiler and the
; run-time system.

; EVAL

(define (eval form package)
  (compile-and-run (list form) package #f #f))

; LOAD-INTO - load file into package.

(define (load-into filename package)
  (really-load-into filename package #f #f))

(define (load-script-into filename package)
  (really-load-into filename package #f #t))

; Evaluate forms as if they came from the given file.

(define (eval-from-file forms package filename)
  (if filename
      ((fluid-cell-ref $note-file-package)
        filename package))
  (compile-and-run forms package filename #t))

; LOAD

(define (load filename . package-option)
  (let ((package (if (null? package-option)
		     (interaction-environment)
		     (car package-option))))
    (really-load-into filename package #t #f)))

;----------------

(define (really-load-into filename package note-undefined? script?)
  (force-output (current-output-port))	; just to make the output nice
  (let ((forms (read-forms filename package script?)))
    (newline (current-noise-port))	; READ-FORMS prints the filename
    (compile-and-run forms
		     package
		     filename
		     note-undefined?)))

(define (compile-and-run forms package maybe-filename note-undefined?)
  (let* ((env (if maybe-filename
		  (bind-source-file-name maybe-filename
					 (package->environment package))
		  (package->environment package)))
	 (template (compile-forms (map (lambda (form)
					 (delay (expand-scanned-form form env)))
				       (scan-forms forms env))
				  maybe-filename
				  (package-uid package))))
    (link! template package note-undefined?)
    (with-load-filename maybe-filename
      (lambda ()
	(invoke-closure
	 (make-closure template
		       (package-uid package)))))))

(define $load-filename (make-fluid (make-cell #f)))

(define (with-load-filename filename thunk)
  (let-fluid $load-filename (make-cell filename)
	     thunk))

(define (current-load-filename)
  (fluid-cell-ref $load-filename))
