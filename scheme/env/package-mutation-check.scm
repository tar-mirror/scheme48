; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber

; Package mutation tests

(define-test-suite package-mutation-tests)

(define-test-case package-mutation package-mutation-tests
  
  (let* ((meta
	  (make-simple-package
	   (list scheme interfaces packages defpackage built-in-structures)
	   eval #f 'meta))
	 (p1
	  (eval '(begin
		   (define p1 (make-simple-package (list scheme) eval #f 'p1))
		   p1)
		meta)))

    (check-exception (eval 'a p1))

    (eval '(define a 'aa) p1)
    (check (eval 'a p1) => 'aa)
    
    (eval '(define (foo) b) p1)
    (check-exception (eval '(foo) p1))
    
    (eval '(define b 'bb) p1)
    (check (eval 'b p1) => 'bb)
    (check (eval '(foo) p1) => 'bb)

    (eval '(define s1-sig (make-simple-interface 's1-sig `(a b c d e f)))
	  meta)
    (eval '(define s1 (make-structure p1 (lambda () s1-sig) 's1))
	  meta)
    
    (let ((p2
	   (eval '(begin
		    (define p2 (make-simple-package (list s1 scheme) eval #f 'p2))
		    p2)
		 meta)))

      (check (eval 'b p2) => 'bb)
      (check-exception (eval 'c p2))
      (check-exception (eval 'z p2))
      
      (eval '(define (bar) c) p2)
      (check-exception (eval '(bar) p2))
      (eval '(define c 'cc) p1)
      (check (eval 'c p2) => 'cc)
      (check (eval '(bar) p2) => 'cc)
      
      (eval '(define (baz1) d) p1)
      (eval '(define (baz2) d) p2)
      (check-exception (eval '(baz1) p1))
      (check-exception (eval '(baz2) p2))
      (eval '(define d 'dd) p1)
      (check (eval '(baz1) p1) => 'dd)
      (check (eval '(baz2) p2) => 'dd)
      
      ;; Shadow
      (eval '(define d 'shadowed) p2)
      (check (eval '(baz1) p1) => 'dd)
      (check (eval '(baz2) p2) => 'shadowed)
      
      ;; Shadow undefined
      (eval '(define (moo1) f) p1)
      (eval '(define (moo2) f) p2)
      (eval '(define f 'ff) p2)
      (check-exception (eval '(moo1) p1))
      (check (eval '(moo2) p2) => 'ff)
      
      (eval '(define (quux1) e) p1)
      (eval '(define (quux2) e) p2)
      (eval '(define (quux3 x) (set! e x)) p1)
      (eval '(define (quux4 x) (set! e x)) p2)

      (check-exception (eval '(quux1) p1))
      (check-exception (eval '(quux2) p2))
      (check-exception (eval '(quux3 'q3) p1))
      (check-exception (eval '(quux4 'q4) p2))

      (eval '(define e 'ee) p1)
      (check (eval '(quux1) p1) => 'ee)
      (check (eval '(quux2) p2) => 'ee)
      (eval '(quux3 'q3) p1)
      (check (eval '(quux1) p1) => 'q3)
      (check (eval '(quux2) p2) => 'q3)
      (eval '(quux4 'q4) p2) ; should eventually be violation

      (eval '(define e 'ee2) p2)
      (check (eval '(quux1) p1) => 'q4) ; should eventually be q3
      (check (eval '(quux2) p2) => 'ee2)
      (eval '(quux3 'qq3) p1)
      (eval '(quux4 'qq4) p2)
      (check (eval '(quux1) p1) => 'qq3)
      (check (eval '(quux2) p2) => 'qq4)

      ;; (set-verify-later! really-verify-later!)

      (eval '(define-interface s3-sig (export a b x y z))
	    meta)
      
      (eval '(define s3
	       (make-structure p1 (lambda () s3-sig) 's3))
	    meta)
      
      (let ((p4
	     (eval '(begin
		      (define p4 (make-simple-package (list s3 scheme) eval #f 'p4))
		      p4)
		   meta)))
	    
	    (eval '(define (fuu1) a) p4)
	    (eval '(define (fuu2) d) p4)
	    (check (eval '(fuu1) p4) => 'aa)
	    (check-exception (eval '(fuu2) p4))
	    
	    ;; Remove a, add d
	    (eval '(define-interface s3-sig (export b d x y z))
		  meta)
	    (package-system-sentinel)
	    
	    (check-exception (eval 'a p4))
	    (check (eval 'd p4) => 'dd)
	    (check (eval '(fuu2) p4) => 'dd)
	    (check-exception (eval '(fuu1) p4))    ; Foo.
	    ))))


