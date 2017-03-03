; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Marcus Crestani

(define-test-suite transport-link-cell-tests)

(define max-number-of-tlcs 999)

(define-test-case constructor-predicate transport-link-cell-tests
  (check-that 
   (transport-link-cell? (make-transport-link-cell 'key 'value 'tconc 'next))
   (is-true)))

(define-test-case accessors transport-link-cell-tests
  (let* ((key (cons 23 42))
	 (value (cons 65 99))
	 (tconc 'tconc)
	 (next 'next)
	 (tlc (make-transport-link-cell key value tconc next)))
    (check (transport-link-cell-key tlc) => key)
    (check (transport-link-cell-value tlc) => value)
    (check (transport-link-cell-tconc tlc) => tconc)
    (check (transport-link-cell-next tlc) => next)))

(define-test-case setters transport-link-cell-tests
  (let* ((key (cons 23 42))
	 (value (cons 65 99))
	 (tconc 'tconc)
	 (next 'next)
	 (tlc (make-transport-link-cell key value tconc next)))
    (check (transport-link-cell-key tlc) => key)
    (check (transport-link-cell-value tlc) => value)
    (check (transport-link-cell-tconc tlc) => tconc)
    (check (transport-link-cell-next tlc) => next)
    (let ((new-value 'value)
	  (new-tconc (cons #f #f))
	  (new-next "I'm next!"))
      (set-transport-link-cell-value! tlc new-value)
      (set-transport-link-cell-tconc! tlc new-tconc)
      (set-transport-link-cell-next! tlc new-next)
      (check (transport-link-cell-key tlc) => key)
      (check (transport-link-cell-value tlc) => new-value)
      (check (transport-link-cell-tconc tlc) => new-tconc)
      (check (transport-link-cell-next tlc) => new-next))))

(define-test-case collection transport-link-cell-tests
  (do-ec
   (:range n 1 max-number-of-tlcs)
   (let* ((key (cons 23 42))
	  (value (cons 65 99))
	  (tconc (make-tconc-queue))
	  (next #f)
	  (tlc (make-transport-link-cell key value tconc next)))
    (collect)
    (let ((tlc-tconc (transport-link-cell-tconc tlc)))
      (if tlc-tconc
	  (check-that (tconc-queue-empty? tlc-tconc) (is-true))
	  (begin
	    (check-that (eq? (tconc-queue-dequeue! tconc) tlc) (is-true))
	    (check-that (tconc-queue-empty? tconc) (is-true))))))))

(define-test-case collection-one-tconc transport-link-cell-tests
  (let ((tconc (make-tconc-queue)))
    (do-ec
     (:range n 1 max-number-of-tlcs)
     (let* ((key (cons 23 42))
	    (value (cons 65 99))
	    (next #f)
	    (tlc (make-transport-link-cell key value tconc next)))
       (collect)
       (let ((tlc-tconc (transport-link-cell-tconc tlc)))
	 (if tlc-tconc
	     (check-that (tconc-queue-empty? tlc-tconc) (is-true))
	     (begin
	       (check-that (eq? (tconc-queue-dequeue! tconc) tlc) (is-true))
	       (check-that (tconc-queue-empty? tconc) (is-true)))))))))

(define-test-case collection-no-tconc transport-link-cell-tests
  (let ((key (cons 23 42))
	(value (cons 65 99))
	(next #f))
    (let* ((tconc 23)
	   (tlc (make-transport-link-cell key value tconc next)))
      (collect)
      (check (transport-link-cell-tconc tlc) => tconc))
    (let* ((tconc (cons 23 42))
	   (tlc (make-transport-link-cell key value tconc next)))
      (collect)
      (check (transport-link-cell-tconc tlc) => tconc)
      (check (car (transport-link-cell-tconc tlc)) => (car tconc))
      (check (cdr (transport-link-cell-tconc tlc)) => (cdr tconc)))
    (let* ((tconc (cons (cons 23 42) 65))
	   (tlc (make-transport-link-cell key value tconc next)))
      (collect)
      (check (transport-link-cell-tconc tlc) => tconc)
      (check (car (transport-link-cell-tconc tlc)) => (car tconc))
      (check (car (car (transport-link-cell-tconc tlc))) => (car (car tconc)))
      (check (cdr (car (transport-link-cell-tconc tlc))) => (cdr (car tconc)))
      (check (cdr (transport-link-cell-tconc tlc)) => (cdr tconc)))
    (let* ((tconc (cons 23 (cons 42 65)))
	   (tlc (make-transport-link-cell key value tconc next)))
      (collect)
      (check (transport-link-cell-tconc tlc) => tconc)
      (check (car (transport-link-cell-tconc tlc)) => (car tconc))
      (check (car (cdr (transport-link-cell-tconc tlc))) => (car (cdr tconc)))
      (check (cdr (cdr (transport-link-cell-tconc tlc))) => (cdr (cdr tconc)))
      (check (car (transport-link-cell-tconc tlc)) => (car tconc)))))

(define-test-case collect-n transport-link-cell-tests
  (let* ((tconc (make-tconc-queue))
	 (tlcs (list-ec 
		(: n 1 max-number-of-tlcs)
		(let* ((key (cons n n))
		       (value (cons (+ 1000 n) (+ 1000 n)))
		       (next #f)
		       (tlc (make-transport-link-cell key value tconc next)))
		  tlc))))
    (collect)
    (for-each 
     (lambda (tlc)
       (let ((tlc-tconc (transport-link-cell-tconc tlc)))
	 (if tlc-tconc
	     (check-that (tconc-queue? tlc-tconc) (is-true))
	     (tconc-queue-dequeue! tconc))))
     tlcs)
    (check-that (tconc-queue-empty? tconc) (is-true))))

(define-test-case collect-n-one-key transport-link-cell-tests
  (let* ((tconc (make-tconc-queue))
	 (key (cons 23 42))
	 (tlcs (list-ec
		(: n 1 max-number-of-tlcs)
		(let* ((value (cons (+ 1000 n) (+ 1000 n)))
		       (next #f)
		       (tlc (make-transport-link-cell key value tconc next)))
		  tlc))))
    (collect)
    (for-each
     (lambda (tlc)
       (let ((tlc-tconc (transport-link-cell-tconc tlc)))
	 (if tlc-tconc
	     (check-that (tconc-queue? tlc-tconc) (is-true))
	     (tconc-queue-dequeue! tconc))))
     tlcs)
    (check-that (tconc-queue-empty? tconc) (is-true))))

(define-test-case collect-n-one-unmovable-key transport-link-cell-tests
  (let* ((tconc (make-tconc-queue))
	 (key 23)
	 (tlcs (list-ec
		(: n 1 max-number-of-tlcs)
		(let* ((value (cons (+ 1000 n) (+ 1000 n)))
		       (next #f)
		       (tlc (make-transport-link-cell key value tconc next)))
		  tlc))))
    (collect)
    (for-each
     (lambda (tlc)
       (let ((tlc-tconc (transport-link-cell-tconc tlc)))
	 (check-that (tconc-queue-empty? tlc-tconc) (is-true))))
     tlcs)
    (check-that (tconc-queue-empty? tconc) (is-true))))
