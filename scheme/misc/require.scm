; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber



(define-syntax require
  (syntax-rules (quote)
    ((require '(name1 name2 ...))
     (*require '(name1 name2 ...)))))

(define (*require interface-id)
  (let ((start-thunk
	 (case (car interface-id)
	   ((scheme-48)
	    (let ((p (config-package)))
	      (lambda () p)))
	   ((scheme-library-1)
	    (let* ((p (config-package))
		   (thunk
		    (lambda ()
		      (environment-ref p 'scheme-library-1))))
	      (ensure-loaded (thunk))
	      (thunk)))
	   (else
	    (assertion-violation
	     'require "unrecognized interface identifier" interface-id)))))
    (package-open! (interaction-environment)
		   (let loop ((names (cdr interface-id))
			      (thunk start-thunk))
		     (if (null? names)
			 thunk
			 (let ((new-thunk 
				(lambda ()
				  (let ((source (thunk)))
				    (if (package? source)
					(environment-ref source
							 (car names))
					(*structure-ref source
							(car names)))))))
			   (ensure-loaded (new-thunk))
			   (loop (cdr names)
				 new-thunk)))))))
