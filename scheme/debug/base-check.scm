; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber

; Tests for stuff in the base language.

(define-test-suite base-tests)

; adapted from the R6RS document
(define-test-case quasiquote base-tests
  (check `(list ,(+ 1 2) 4) => '(list 3 4))
  (check (let ((name 'a)) `(list ,name ',name)) 
	 => '(list a (quote a)))
  (check `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)
	 =>  '(a 3 4 5 6 b))
  (check `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))
	 => '((foo 7) . cons))
  (check `#(10 5 ,(even? 4) ,@(map even? '(2 3 5 7)) 8)
	 => '#(10 5 #t #t #f #f #f 8))
  (check (let ((name 'foo))
	   `((unquote name name name)))
	 => '(foo foo foo))
  (check (let ((name '(foo)))
	   `((unquote-splicing name name name)))
	 => '(foo foo foo))
  (check (let ((q '((append x y) (even? 9))))
	   ``(foo ,,@q)) 
	 => '`(foo (unquote (append x y) (even? 9))))
  (check (let ((x '(2 3))
	       (y '(4 5)))
	   `(foo (unquote (append x y) (even? 9))))
	 => '(foo (2 3 4 5) #f))

  (check `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)
	 => '(a `(b ,(+ 1 2) ,(foo 4 d) e) f))
  (check (let ((name1 'x)
	       (name2 'y))
	   `(a `(b ,,name1 ,',name2 d) e))
	 => '(a `(b ,x ,'y d) e)))


(define-test-case dynamic-wind base-tests
  (let* ((f (make-fluid 'top))
	 (log '())
	 (report (lambda (foo)
		   (set! log (cons (cons foo (fluid f)) log)))))
    ((call-with-current-continuation
       (lambda (k1)
	 (let-fluid f 1
	   (lambda ()
	     (dynamic-wind
	      (lambda () (report 'wind-1))
	      (lambda ()
		(let-fluid f 2
		  (lambda ()
		    (dynamic-wind
		     (lambda () (report 'wind-2))
		     (lambda ()
		       (let-fluid f 3
			 (lambda ()
			   (report 'before-throw-out)
			   (call-with-current-continuation
			     (lambda (k2)
			       (k1 (lambda ()
				     (report 'after-throw-out)
				     (k2 #f)))))
			   (report 'after-throw-in)
			   (lambda () (report 'done)))))
		     (lambda () (report 'unwind-2))))))
	      (lambda () (report 'unwind-1))))))))
    (check log
	   => '((done . top)
		(unwind-1 . 1)
		(unwind-2 . 2)
		(after-throw-in . 3)
		(wind-2 . 1)
		(wind-1 . top)
		(after-throw-out . top)
		(unwind-1 . 1)
		(unwind-2 . 2)
		(before-throw-out . 3)
		(wind-2 . 2)
		(wind-1 . 1)))))
