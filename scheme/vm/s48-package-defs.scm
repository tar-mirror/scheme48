; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Marcus Crestani, Mike Sperber,
; Martin Gasbichler


(define-structure vm-utilities vm-utilities-interface
  (open scheme
	(subset prescheme
		(char->ascii
		 error
		 enum errors
		 shift-left bitwise-and arithmetic-shift-right logical-shift-right 
		 write-string write-integer current-error-port)))
  (files (util vm-utilities))
  (begin
;    (define-syntax assert
;      (lambda (exp rename compare)
;    	0))
    (define (assert x)
      (if (not x)
    	  (error "assertion failed")))
    ))

(define-structure external external-interface
  (open scheme bitwise ps-memory)
  (for-syntax (open scheme)) ; for error
  (files (util external)))

(define-structures ((channel-io channel-interface)
		    (events event-interface))
  (open scheme big-scheme ps-memory ports
	(subset i/o		(current-error-port))
	(modify prescheme	(prefix prescheme:)
		                (expose open-input-file open-output-file
					close-input-port close-output-port
					errors)))
  (files (util s48-channel)))
