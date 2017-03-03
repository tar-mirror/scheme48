; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Marcus Crestani

(define-test-suite tconc-queue-tests)

(define max-queue-size 999)

(define (check-raises-assertion-violation thunk source)
  (call-with-current-continuation
   (lambda (esc)
     (with-exception-handler
      (lambda (c)
        (esc
	 (call-with-values
	   (lambda () (decode-condition c))
	   (lambda (type who message more-stuff)
	     (check type => 'assertion-violation)
	     (check who => source)))))
      (lambda ()
        (thunk)
        (check #f => 'should-never-reach-this-point))))))

(define-test-case constructor-predicate tconc-queue-tests
  (check-that 
   (tconc-queue? (make-tconc-queue))
   (is-true)))

(define-test-case empty tconc-queue-tests
  (check-that
   (tconc-queue-empty? (make-tconc-queue))
   (is-true)))

(define-test-case non-empty tconc-queue-tests
  (let ((q (make-tconc-queue)))
    (tconc-queue-enqueue! q 23)
    (check-that
     (tconc-queue-empty? q)
     (is-false))))

(define-test-case dequeue-empty tconc-queue-tests
  (let ((q (make-tconc-queue)))
    (check-raises-assertion-violation
     (lambda () (tconc-queue-dequeue! q))
     'tconc-queue-dequeue)))

(define-test-case peek-empty tconc-queue-tests
  (let ((q (make-tconc-queue)))
    (check-raises-assertion-violation
     (lambda () (tconc-queue-peek q))
     'tconc-queue-peek)))

(define-test-case size tconc-queue-tests
  (do-ec
   (:range n 0 max-queue-size)
   (let ((q (make-tconc-queue)))
     (do-ec
      (:range m 0 n)
      (tconc-queue-enqueue! q m))
     (check (tconc-queue-size q) => n)
     (check-that (tconc-queue? q) (is-true)))))

(define-test-case dequeue-1 tconc-queue-tests
  (let ((q (make-tconc-queue)))
    (tconc-queue-enqueue! q 23)
    (check-that (tconc-queue? q) (is-true))
    (check (tconc-queue-peek q) => 23)
    (check (tconc-queue-dequeue! q) => 23)
    (check-that (tconc-queue? q) (is-true))))

(define-test-case dequeue-2 tconc-queue-tests
  (let ((q (make-tconc-queue)))
    (tconc-queue-enqueue! q 23)
    (check-that (tconc-queue? q) (is-true))
    (tconc-queue-enqueue! q 42)
    (check-that (tconc-queue? q) (is-true))
    (check (tconc-queue-peek q) => 23)
    (check (tconc-queue-dequeue! q) => 23)
    (check-that (tconc-queue? q) (is-true))
    (check (tconc-queue-peek q) => 42)
    (check (tconc-queue-dequeue! q) => 42)
    (check-that (tconc-queue? q) (is-true))))

(define-test-case en/dequeue-2 tconc-queue-tests
  (let ((q (make-tconc-queue)))
    (tconc-queue-enqueue! q 23)
    (check (tconc-queue-peek q) => 23)
    (check (tconc-queue-dequeue! q) => 23)
    (tconc-queue-enqueue! q 42)
    (check (tconc-queue-peek q) => 42)
    (check (tconc-queue-dequeue! q) => 42)))

(define-test-case en/dequeue-3 tconc-queue-tests
  (let ((q (make-tconc-queue)))
    (tconc-queue-enqueue! q 23)
    (check-that (tconc-queue? q) (is-true))
    (tconc-queue-enqueue! q 65)
    (check-that (tconc-queue? q) (is-true))
    (check (tconc-queue-peek q) => 23)
    (check (tconc-queue-dequeue! q) => 23)
    (check (tconc-queue-peek q) => 65)
    (check (tconc-queue-dequeue! q) => 65)
    (tconc-queue-enqueue! q 42)
    (check-that (tconc-queue? q) (is-true))
    (check (tconc-queue-peek q) => 42)
    (check (tconc-queue-dequeue! q) => 42)))

(define-test-case enqueue-n/dequeue-n tconc-queue-tests
  (do-ec
   (:range n 0 max-queue-size)
   (let ((q (make-tconc-queue)))
     (do-ec
      (:range m 0 n)
      (begin
	(tconc-queue-enqueue! q m)
	(check (tconc-queue-peek q) => 0)))
     (check-that (tconc-queue? q) (is-true))
     (check (tconc-queue-size q) => n)
     (do-ec
      (:range m 0 n)
      (begin
	(check (tconc-queue-peek q) => m)
	(check (tconc-queue-dequeue! q) => m)))
     (check-that (tconc-queue? q) (is-true))
     (check (tconc-queue-size q) => 0)
     (check-raises-assertion-violation
      (lambda () (tconc-queue-peek q))
      'tconc-queue-peek)
     (check-raises-assertion-violation
      (lambda () (tconc-queue-dequeue! q))
      'tconc-queue-dequeue))))

(define-test-case en/dequeue-n tconc-queue-tests
  (do-ec
   (:range n 0 max-queue-size)
   (let ((q (make-tconc-queue)))
     (do-ec
      (:range m 0 n)
      (begin
	(tconc-queue-enqueue! q m)
	(check (tconc-queue-size q) => 1)
	(check-that (tconc-queue? q) (is-true))
	(check (tconc-queue-peek q) => m)
	(check (tconc-queue-dequeue! q) => m)
	(check-that (tconc-queue? q) (is-true))
	(check (tconc-queue-size q) => 0)))
     (check-raises-assertion-violation
      (lambda () (tconc-queue-peek q))
      'tconc-queue-peek)
     (check-raises-assertion-violation
      (lambda () (tconc-queue-dequeue! q))
      'tconc-queue-dequeue))))

(define-test-case en/clear/dequeue-n tconc-queue-tests
  (let ((q (make-tconc-queue)))
    (do-ec
     (:range n 0 max-queue-size)
     (begin
       (tconc-queue-clear! q)
       (check-that (tconc-queue-empty? q) (is-true))
       (do-ec
	(:range m 0 n)
	(begin
	  (tconc-queue-enqueue! q m)
	  (check (tconc-queue-size q) => 1)
	  (check-that (tconc-queue? q) (is-true))
	  (check (tconc-queue-peek q) => m)
	  (check (tconc-queue-dequeue! q) => m)
	  (check-that (tconc-queue? q) (is-true))
	  (check (tconc-queue-size q) => 0)))
       (check-raises-assertion-violation
	(lambda () (tconc-queue-peek q))
	'tconc-queue-peek)
       (check-raises-assertion-violation
	(lambda () (tconc-queue-dequeue! q))
	'tconc-queue-dequeue)))))
