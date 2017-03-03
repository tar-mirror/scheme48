; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber

; flatloaded -> load

(define *noisy?* #f)

(define (flatload struct . env-option)
  (let ((env (if (null? env-option)
		 (interaction-environment)
		 (car env-option)))
	(l '())
	(set-package-loaded?! set-package-loaded?!))
    (walk-packages (list struct)
		   (lambda (p)
		     (not (package-loaded? p)))
		   (lambda (file p)
		     (let* ((fn (package-file-name p))
			    (file (namestring file
					      (if fn
						  (file-name-directory fn)
						  #f)
					     *load-file-type*)))
		       (if *noisy?*
			   (begin (display #\space) (display file)))
		       (set! l (cons (lambda () (apply fload file env-option))
				     l))))
		   (lambda (forms p)
		     (set! l (cons (lambda ()
				     (for-each (lambda (form)
						 (eval form env))
					       forms))
				   l)))
		   (lambda (p)
		     (set! l (cons (lambda ()
				     (set-package-loaded?! p #t))
				   l))))
    (for-each (lambda (thunk) (thunk)) (reverse l))
    (newline)))

(define (fload filename . rest)
  (let ((save filename))
    (dynamic-wind (lambda () (set! *source-file-name* filename))
		  (lambda ()
		    (apply load filename rest))
		  (lambda () (set! *source-file-name* save)))))

(define (walk-packages structs process? file-action forms-action after-action)
  (let ((seen '()))
    (letrec ((recur
	      (lambda (s)
		(let ((p (structure-package s)))
		  (if (not (memq p seen))
		      (begin 
			(set! seen (cons p seen))
			(if (process? p)
			    (begin
			      (if *noisy?*
				  (begin (newline)
					 (display "[")
					 (write (structure-name s))))
			      ;; (write (structure-name s)) (display " ")
			      (for-each recur (package-opens p))
			      (for-each (lambda (name+struct)
					  (recur (cdr name+struct)))
					(package-accesses p))
			      (for-each (lambda (clause)
					  (case (car clause)
					    ((files)
					     (for-each (lambda (f)
							 (file-action f p))
						       (cdr clause)))
					    ((begin)
					     (forms-action (cdr clause) p))))
					(package-clauses p))
			      (after-action p)
			      (if *noisy?* (display "]"))))))))))
      (for-each recur structs))
    (if *noisy?* (newline))
    seen))


; Return list of names of all files needed to build a particular structure.
; This is handy for creating dependency lists for "make".

(define (all-file-names struct . base-option)
  (let ((l '())
	(b '()))
    (walk-packages base-option
		   (lambda (p) #t)
		   (lambda (filename p) #f)
		   (lambda (forms p) #f)
		   (lambda (p)
		     (set! b (cons p b))))
    (walk-packages (list struct)
		   (lambda (p)
		     (not (memq p b)))
		   (lambda (filename p)
		     (let ((dir (file-name-directory (package-file-name p))))
		       (set! l (cons (namestring filename dir *load-file-type*)
				     l))))
		   (lambda (forms p)
		     (display "Package contains (begin ...) clause: ")
		     (write forms)
		     (newline))
		   (lambda (p) #f))
    (reverse l)))
