; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber, Robert Ransom

; Tricky business, as we want to typecheck all arguments, and avoid
; redundant normalizations.

; x is already wrapped
(define (compare-n-ary name =? wrap pred x . rest)
  (let loop ((x x)
	     (rest rest))
    (or (null? rest)
	(let ((next (wrap (car rest))))
	  (if (=? x next)
	      (loop next (cdr rest))
	      (check-pred name pred (cdr rest)))))))

(define (check-pred name pred lis)
  (cond
   ((memp (lambda (x)
	    (not (pred x)))
	  lis)
    => (lambda (wrong)
	 (assertion-violation name
			      "invalid argument"
			      (car wrong))))
   (else #f)))

(define-syntax define-n-ary-comparison
  (syntax-rules ()
    ((define-n-ary-comparison ?name ?pred ?wrap ?binary-name)
     (define (?name a b . rest)
       (let ((bw (?wrap b)))
	 (cond
	  ((?binary-name (?wrap a) bw)
	   (or (null? rest)
	       (apply compare-n-ary '?name ?binary-name ?wrap ?pred bw rest)))
	  ((null? rest) #f)
	  (else (check-pred '?name ?pred rest))))))))

