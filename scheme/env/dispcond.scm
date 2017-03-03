; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber



; Displaying conditions

(define display-condition
  (let ((display display) (newline newline))
    (lambda (c port . rest)
      (let ((depth (if (pair? rest)
		       (car rest)
		       5))
	    (length (if (and (pair? rest) (pair? (cdr rest)))
			(cadr rest)
			6)))
	(if (ignore-errors (lambda ()
			     (newline port)
			     (really-display-condition c port depth length)
			     #f))
	    (begin (display "<Error while displaying condition.>" port)
		   (newline port)))))))

(define (really-display-condition c port depth length)
  (call-with-values
      (lambda () (decode-condition c))
    (lambda (type who message stuff)
      (display type port)
      (display ": " port)
      (if (string? message)
	  (display message port)
	  (limited-write message port depth length))
      (let ((spaces
	     (make-string (+ (string-length (symbol->string type)) 2)
			  #\space)))
	(if who
	    (begin
	      (display " [" port)
	      (display who port)
	      (display "]" port)))
	(for-each (lambda (irritant)
		    (newline port)
		    (display spaces port)
		    (limited-write irritant port depth length))
		  stuff))))
  (newline port))

(define (limited-write obj port max-depth max-length)
  (let recur ((obj obj) (depth 0))
    (if (and (= depth max-depth)
	     (not (or (boolean? obj)
		      (null? obj)
		      (number? obj)
		      (symbol? obj)
		      (char? obj)
		      (string? obj))))
	(display "#" port)
	(call-with-current-continuation
	  (lambda (escape)
	    (recurring-write obj port
	      (let ((count 0))
		(lambda (sub)
		  (if (= count max-length)
		      (begin (display "---" port)
			     (write-char
			      (if (or (pair? obj) (vector? obj))
				  #\)
				  #\})
			      port)
			     (escape #t))
		      (begin (set! count (+ count 1))
			     (recur sub (+ depth 1))))))))))))

