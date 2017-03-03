; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber

(define-test-suite r6rs-lists-tests)

;; These are all from the R6RS document

(define-test-case find r6rs-lists-tests
  (check (find even? '(3 1 4 1 5 9)) => 4)
  (check (find even? '(3 1 5 1 5 9)) => #f))

(define-test-case for-all r6rs-lists-tests
  (check (for-all even? '(3 1 4 1 5 9)) => #f)
  (check (for-all even? '(3 1 4 1 5 9 . 2)) => #f)
  (check (for-all even? '(2 4 14)) => #t)
  (check-exception (for-all even? '(2 4 14 . 9)))
  (check (for-all (lambda (n) (and (even? n) n))
		  '(2 4 14)) => 14)
  (check (for-all < '(1 2 3) '(2 3 4)))
  (check (for-all < '(1 2 4) '(2 3 4)) => #f))

(define-test-case exists r6rs-lists-tests
  (check (exists even? '(3 1 4 1 5 9)))
  (check (exists even? '(3 1 1 5 9)) => #f)
  (check-exception (exists even? '(3 1 1 5 9 . 2)))
  (check (exists (lambda (n) (and (even? n) n)) '(2 1 4 14)) => 2)
  (check (exists < '(1 2 4) '(2 3 4)))
  (check (exists > '(1 2 3) '(2 3 4)) => #f)
  (check (not (exists even? '()))))

(define-test-case filter r6rs-lists-tests
  (check (filter even? '(3 1 4 1 5 9 2 6)) => '(4 2 6)))

(define-test-case partition r6rs-lists-tests
  (check
   (call-with-values
       (lambda () 
	 (partition even? '(3 1 4 1 5 9 2 6)))
     cons)
   => '((4 2 6) . (3 1 1 5 9))))

(define-test-case fold-left r6rs-lists-tests
  (check (fold-left + 0 '(1 2 3 4 5)) => 15)

  (check (fold-left (lambda (a e) (cons e a)) '()
		    '(1 2 3 4 5)) => '(5 4 3 2 1))

  (check (fold-left (lambda (count x)
		      (if (odd? x) (+ count 1) count))
		    0
		    '(3 1 4 1 5 9 2 6 5 3)) => 7)

  (check (fold-left (lambda (max-len s)
		      (max max-len (string-length s)))
		    0
		    '("longest" "long" "longer")) => 7)

  (check (fold-left cons '(q) '(a b c)) => '((((q) . a) . b) . c))

  (check (fold-left + 0 '(1 2 3) '(4 5 6)) => 21))

(define-test-case fold-right r6rs-lists-tests
  (check (fold-right + 0 '(1 2 3 4 5)) => 15)

  (check (fold-right cons '() '(1 2 3 4 5)) => '(1 2 3 4 5))

  (check (fold-right (lambda (x l)
		       (if (odd? x) (cons x l) l))
		     '()
		     '(3 1 4 1 5 9 2 6 5))
	 => '(3 1 1 5 9 5))

  (check (fold-right cons '(q) '(a b c)) => '(a b c q))

  (check (fold-right + 0 '(1 2 3) '(4 5 6)) => 21))

(define-test-case remp r6rs-lists-tests
  (check (remp even? '(3 1 4 1 5 9 2 6 5)) => '(3 1 1 5 9 5)))

(define-test-case remove r6rs-lists-tests
  (check (remove 1 '(3 1 4 1 5 9 2 6 5)) => '(3 4 5 9 2 6 5))
  (check (remv 1 '(3 1 4 1 5 9 2 6 5)) => '(3 4 5 9 2 6 5))
  (check (remq 'foo '(bar foo baz)) => '(bar baz)))

(define-test-case memp r6rs-lists-tests
  (check (memp even? '(3 1 4 1 5 9 2 6 5)) => '(4 1 5 9 2 6 5)))

(define-test-case member r6rs-lists-tests
  (check (memq 'a '(a b c)) => '(a b c))
  (check (memq 'b '(a b c)) => '(b c))
  (check (memq 'a '(b c d)) => #f)
  (check (memq (list 'a) '(b (a) c)) =>  #f)
  (check (member (list 'a) '(b (a) c)) => '((a) c))
  (check (memv 101 '(100 101 102)) => '(101 102)))

(define-test-case assp r6rs-lists-tests
  (define d '((3 a) (1 b) (4 c)))
  
  (check (assp even? d) => '(4 c))
  (check (assp odd? d) => '(3 a)))

(define-test-case assoc r6rs-lists-tests
  (define e '((a 1) (b 2) (c 3)))
  (check (assq 'a e) => '(a 1))
  (check (assq 'b e) => '(b 2))
  (check (assq 'd e) => #f)
  (check (assq (list 'a) '(((a)) ((b)) ((c))))
	 => #f)
  (check (assoc (list 'a) '(((a)) ((b)) ((c))))   
	 => '((a)))
  (check (assv 5 '((2 3) (5 7) (11 13)))    
	 => '(5 7)))

(define-test-case cons* r6rs-lists-tests
  (check (cons* 1 2 '(3 4 5)) => '(1 2 3 4 5))
  (check (cons* 1 2 3) => '(1 2 . 3))
  (check (cons* 1) => 1))
