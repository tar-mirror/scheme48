; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Harald Glab-Plhak, Marcus Crestani

;tests for r6rs hashtables

(define-test-suite r6rs-hashfun-tests)
(define-test-suite r6rs-nexec-tests)
(define-test-suite r6rs-hashtables-simple-tests)
(define-test-suite r6rs-hashtables-extended-tests)
(define-test-suite r6rs-hashtables-tests
  (r6rs-hashtables-simple-tests
   r6rs-hashfun-tests r6rs-hashtables-extended-tests r6rs-nexec-tests ))

(define-test-case test-set!/ref/keys/entries/delete r6rs-hashtables-simple-tests
  (let ((hash-tab (make-eq-hashtable)))
    (hashtable-set! hash-tab 8 'lolo)
    (hashtable-set! hash-tab 88 'lala)
    (hashtable-set! hash-tab 888 'smily)
    (hashtable-set! hash-tab 8888 'rofl)
    (hashtable-set! hash-tab 88888 'blorf) 
    (check (hashtable-keys hash-tab) => '#(88888 8888 888 88 8))
    (check (call-with-values 
	       (lambda () (hashtable-entries hash-tab))
	     (lambda (a b) (list a b)))
	   => 
	     (list '#(88888 8888 888 88 8) 
		   '#(blorf rofl smily lala lolo)))
    (check (hashtable-ref hash-tab 8888 'noval) => 'rofl)
    (hashtable-delete! hash-tab 8888)
    (check (hashtable-ref hash-tab 8888 'noval) => 'noval)))

(define-test-case create/copy/inspect/contains r6rs-hashtables-simple-tests
  (let ((hash-tab (make-hashtable string-hash string=?)))
    (hashtable-set! hash-tab "a" 'lolo)
    (hashtable-set! hash-tab "b" 'lala)
    (hashtable-set! hash-tab "c" 'smily)
    (hashtable-set! hash-tab "d" 'rofl)
    (hashtable-set! hash-tab "e" 'blorf) 
    (check (hashtable-contains? hash-tab  "a") => #t)
    (check (hashtable-contains? hash-tab  "g") => #f)
    (check (hashtable-size hash-tab) => 5)
    (check (hashtable-mutable? hash-tab) => #t)
    (check (hashtable-equivalence-function hash-tab) => string=?)
    (check (hashtable-hash-function hash-tab) => string-hash)
    (let ((mutable-tab (hashtable-copy hash-tab #t)))
      (check (hashtable-size mutable-tab) => 5)
      (check (hashtable-mutable? mutable-tab) => #t)
      (check (hashtable-equivalence-function mutable-tab) => string=?)
      (check (hashtable-hash-function mutable-tab) => string-hash))
    (let ((immutable-tab (hashtable-copy hash-tab)))
      (check (hashtable-size immutable-tab) => 5)
      (check (hashtable-mutable? immutable-tab) => #f)
      (check (hashtable-equivalence-function immutable-tab) => string=?)
      (check (hashtable-hash-function immutable-tab) => string-hash))))

(define-test-case test-set!/clear/update r6rs-hashtables-simple-tests
   (let ((hash-tab (make-eq-hashtable 100)))
      (hashtable-set! hash-tab 8 'lolo)
      (hashtable-set! hash-tab 88 'lala)
      (hashtable-set! hash-tab 888 'smily)
      (hashtable-set! hash-tab 8888 'rofl)
      (check (vector-length (hashtable-keys hash-tab)) => 4)
      (hashtable-clear! hash-tab)
      (check (vector-length (hashtable-keys hash-tab)) => 0)
      (hashtable-set! hash-tab 8 'lolo)
      (hashtable-set! hash-tab 88 'lala)
      (hashtable-set! hash-tab 888 'smily)
      (check (vector-length (hashtable-keys hash-tab)) => 3)
      (hashtable-clear! hash-tab 25)
      (check (vector-length (hashtable-keys hash-tab)) => 0)
      (hashtable-set! hash-tab 8 'lolo)
      (hashtable-set! hash-tab 880 'lala) 
      (check (vector-length (hashtable-keys hash-tab)) => 2)
      (check (hashtable-size hash-tab) => 2)
      (hashtable-clear! hash-tab 10)
      (hashtable-set! hash-tab 8 'lolo)
      (hashtable-set! hash-tab 88 'lala)
      (hashtable-set! hash-tab 888 'smily)
      (check (hashtable-contains? hash-tab 888) => #t)
      (hashtable-update! hash-tab 8 (lambda (v) (list v (hashtable-keys hash-tab))) #f)
      (check (hashtable-ref hash-tab 8 'noval) => (list 'lolo '#(888 88 8)))))

(define-test-case exception-test r6rs-hashtables-simple-tests
  (let ((hash-tab (make-hashtable string-hash string=?)))
    (hashtable-set! hash-tab "a" 'lolo)
    (hashtable-set! hash-tab "b" 'lala)
    (hashtable-set! hash-tab "c" 'smily)
    (let ((immutable-tab (hashtable-copy hash-tab)))
      (check-exception (hashtable-set! immutable-tab "d" 'rofl))
      (check-exception (hashtable-delete! immutable-tab "a"))
      (check-exception (hashtable-update! immutable-tab "a" (lambda (v) (list v)) 'no-default)))))


;; copied from tlc-table-tests
;; fill a table with objects, delete some, and retrieve them after one
;; collection
(define max-table-size 1023)
(define table-step 23)
(define min-collect-times 2)
(define max-collect-times 5)
(define-test-case set-n/collect/delete-n/ref-n  r6rs-nexec-tests
  (do-ec
   (:range size 1 max-table-size table-step)
   (let* ((table (make-eq-hashtable size))
	  (n (* 3 size))
	  (objs (list-ec (: i n) (cons i n)))
	  (delobjs (list-ec (: i n) (cons (+ i max-table-size) n))))
     (do-ec
      (:list o delobjs)
      (hashtable-set! table o o))
     (collect)
     (do-ec
      (:list o objs)
      (hashtable-set! table o o))
     (collect)
     (do-ec
      (:list o delobjs)
      (check-that
       (hashtable-delete! table o) (opposite (is-false))))
     (collect)
     (do-ec
      (:list o delobjs)
      (check
       (hashtable-ref table o #f) => #f))
     (do-ec
      (:list o objs)
      (check (hashtable-ref table o #f) => o)))))
    
(define-test-case immutable-extended r6rs-hashtables-extended-tests
   (let ((hash-tab (make-hashtable string-hash string=?)))
     	 (hashtable-set! hash-tab "a" 'lolo)
	 (hashtable-set! hash-tab "b" 'lala)
	 (hashtable-set! hash-tab "c" 'smily)
       (let ((hash-tab2 (make-hashtable string-hash string=?))
	     (hash-tab3 (hashtable-copy hash-tab)))
	 (hashtable-set! hash-tab2 "a2" 'lolo)
	 (hashtable-set! hash-tab2 "b2" 'lala)
	 (hashtable-set! hash-tab2 "c2" 'smily)
	 (hashtable-set! hash-tab "a" hash-tab2)
	 (hashtable-set! hash-tab "b" hash-tab3)
	 (let ((hash-tab4  (hashtable-copy hash-tab)))
	   (check-exception (hashtable-set! (hashtable-ref hash-tab "b" 'blah) "c" 'test))
	   (check (begin 
		    (hashtable-set! 
		     (hashtable-ref hash-tab4 "a" 'blah) 
		     "a2" 'setit)
		    (hashtable-ref hash-tab2 "a2" 'noval )) => 'setit)))))
      
(define-test-case constructor-predicate r6rs-hashtables-simple-tests
  (check
   (hashtable? (make-hashtable symbol-hash eq? 11)) => #t))

(define-test-case test-hashing r6rs-hashfun-tests
    (check (equal-hash "a") => (equal-hash (make-string 1 #\a)))
    (check (equal-hash 1024) => (equal-hash (expt 2 10)))
    (check (equal-hash '(1 2 3)) => (equal-hash (list 1 2 3)))

    (check (string-hash "a") => (string-hash (make-string 1 #\a)))
    (check (string-hash "aaaaa") => (string-hash (make-string 5 #\a)))
    (check (string-ci-hash "aAaAA") => (string-ci-hash (make-string 5 #\a)))
    (check (string-ci-hash "aAaAA") => (string-ci-hash (make-string 5 #\A)))

    (check (symbol-hash 'a) => (symbol-hash 'a)))
