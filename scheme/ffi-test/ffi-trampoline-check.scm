; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Marcus Crestani

(define-test-suite ffi-trampoline-tests)

(import-lambda-definition-2 trampoline (proc nargs)
                         "s48_trampoline_2")

(define (foo . args)
 (for-each display (list "[foo " args "]"))
 (newline)
 (cons 'foo-return args))

(define (test0)
 (trampoline (lambda ()
               (call-with-current-continuation
                 (lambda (c)
                   (trampoline (lambda (x)
                                 (c (+ x 1000)))
                               1))))
             0))

(define (test1 error?)
 (let ((lock (make-lock))
       (repl-lock (make-lock)))
   (obtain-lock repl-lock)
   (spawn (lambda ()
            (obtain-lock lock)
            ;(debug-message "A returned "
                          (trampoline (lambda ()
                                        (obtain-lock lock) ; we block
                                        'a)
                                      0);)
            (release-lock repl-lock))
          'thread-a)
   (spawn (lambda ()
            ;(debug-message "B returned "
                          (trampoline (lambda ()
                                        (release-lock lock)    ; A can run
                                        (relinquish-timeslice) ; let A run
                                        (if error? #f 'b))
                                      0));)
          'thread-b)
   (obtain-lock repl-lock)))

(define (test2 error?)
 (let ((lock (make-lock))
       (repl-lock (make-lock)))
   (obtain-lock repl-lock)
   (spawn (lambda ()
            (obtain-lock lock)
	    (trampoline (lambda ()
			  (obtain-lock lock) ; we block
			  'a)
			0)
            (release-lock repl-lock))
          'thread-a)
   (spawn (lambda ()
	    (call-with-current-continuation
	     (lambda (esc)
	       (with-exception-handler
		(lambda (c)
		  (esc 
		   (call-with-values
		       (lambda () (decode-condition c))
		     (lambda (type who message more-stuff)
		       (check type => 'assertion-violation)))))
		(lambda ()
		  (trampoline (lambda ()
				(release-lock lock)    ; A can run
				(relinquish-timeslice) ; let A run
				(if error? 
				    #f
				    'b))
			      0))))))
	     'thread-b)
   (obtain-lock repl-lock)))

(define-test-case test0 ffi-trampoline-tests
  (check (test0) => 1100))

(define-test-case test1-no-error ffi-trampoline-tests
  (check (test1 #f) => #t))
  
(define-test-case test2-error ffi-trampoline-tests
  (check (test2 #t) => #t))

(define-test-case trampoline-error ffi-trampoline-tests
  (call-with-current-continuation
   (lambda (esc)
     (with-exception-handler
      (lambda (c)
	(esc 
	 (call-with-values
	     (lambda () (decode-condition c))
	   (lambda (type who message more-stuff)
	     (check type => 'assertion-violation)
	     (check who => "s48_trampoline_2")))))
      (lambda ()
	(trampoline (lambda ()
		      #f)
		    0))))))
