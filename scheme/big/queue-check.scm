; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Robert Ransom

(define-test-suite queues-tests)

(define-syntax with-queue
  (syntax-rules ()
    ((with-queue (?var) . ?body)
     (let ((?var (make-queue))) . ?body))))

;;; TODO? - move to utility package?
(define-syntax list*
  (syntax-rules (skip)
    ((list* (skip ?expr) . ?rest)
     (begin ?expr
            (list* . ?rest)))
    ((list* ?expr . ?rest)
     (let ((x ?expr))
       (cons x (list* . ?rest))))
    ((list*)
     '())))

;;; TODO? - rename to PROVISIONAL-CELL-PUSH! and move to utility package?
(define (prov-cell-push! c x)
  (ensure-atomicity!
   (provisional-cell-set! c (cons x (provisional-cell-ref c)))))

;; Test wrapper for ENQUEUE!.  Real applications should use
;; ENQUEUE-MANY!, which is faster.
(define (stuff-queue! q xs)
  (ensure-atomicity!
   (for-each (lambda (x) (enqueue! q x)) xs)))

(define (devour-queue! q)
  (ensure-atomicity
   (let loop ((acc '()))
     (if (queue-empty? q)
	 (reverse acc)
	 (loop (cons (dequeue! q) acc))))))

(define (suck-queue! q n)
  (ensure-atomicity
   (let loop ((acc '())
	      (n n))
     (if (or (queue-empty? q)
	     (<= n 0))
	 (reverse acc)
	 (loop (cons (dequeue! q) acc)
	       (- n 1))))))

;;; Tests for utility functions used by the queues package.

(define-test-case list->queue-list queues-tests
  (for-each
   (lambda (xs)
     (check (receive (head tail) (list->queue-list xs)
		     (list xs (cdr head)))
	    => (list xs xs))
     (if (not (null? xs))
	 (check (receive (head tail) (list->queue-list xs)
			 (list xs tail))
		=> (list xs (srfi-1:last-pair xs))))
     (check (receive (head tail) (list->queue-list xs)
		     (list xs (eq? (srfi-1:last-pair head) tail)))
	    => (list xs #t)))
   '(() (foo) (foo bar) (foo bar baz) (foo bar baz quux))))

;;; Tests for the queue operations we plan to keep.

(define (do-basic-tests!)
  (check (with-queue (q)
                     (enqueue! q 'a)
                     (dequeue! q))
         => 'a)
  (check (with-queue (q)
                     (stuff-queue! q '(a b c a b c))
                     (devour-queue! q))
         => '(a b c a b c))
  (check (with-queue (q)
                     (stuff-queue! q '(a b c a b c))
                     (list* (suck-queue! q 3)
                            (skip (stuff-queue! q '(d e f)))
                            (devour-queue! q)))
         => '((a b c) (a b c d e f)))
  (check (with-queue (q)
                     (stuff-queue! q '(a b c a b c))
                     (list* (suck-queue! q 3)
                            (skip (enqueue-many! q '(d e f)))
                            (devour-queue! q)))
         => '((a b c) (a b c d e f)))
  (check (with-queue (q)
                     (stuff-queue! q '(a b c a b c))
                     (list* (devour-queue! q)
                            (maybe-dequeue! q)
                            (skip (stuff-queue! q '(d e f)))
                            (maybe-dequeue! q)
                            (devour-queue! q)))
         => '((a b c a b c)
              #f
              d
              (e f))))

(define-test-case basics queues-tests
  (do-basic-tests!))
(define-test-case basics-in-big-transaction queues-tests
  (ensure-atomicity!
   ;; Calling CHECK inside a transaction is normally a *bad* idea, but
   ;; this transaction should not need to be restarted.
   (do-basic-tests!)))

(define-test-case basics-comment-tests queues-tests
  (check (let ((q (make-queue))
	       (c (make-cell '())))
	   (enqueue! q 'a)
	   (ensure-atomicity!
	    (enqueue! q 'b)
	    (prov-cell-push! c (maybe-dequeue! q)))
	   (prov-cell-push! c (maybe-dequeue! q))
	   (cell-ref c))
	 => '(b a)))

(define-test-case queue-head queues-tests
  (check-exception (with-queue (q) (queue-head q)))
  (check (with-queue (q)
                     (stuff-queue! q '(a b c))
                     (queue-head q))
         => 'a))

(define-test-case list->queue queues-tests
  (check (let ((q (list->queue '(a b c d))))
           (list* (suck-queue! q 2)
                  (skip (stuff-queue! q '(e f g)))
                  (devour-queue! q)))
         => '((a b)
              (c d e f g))))

;;; Delenda.

(define-test-case delenda-comment-tests queues-tests
  (check (let ((q (make-queue))
	       (c (make-cell 'OOPS)))
	   (enqueue! q 'a)
	   (ensure-atomicity!
	    (enqueue! q 'b)
	    (provisional-cell-set! c (queue->list q)))
	   (cell-ref c))
	 => '(a b))
  (check (let ((q (make-queue))
	       (c (make-cell 'OOPS)))
	   (enqueue! q 'a)
	   (ensure-atomicity!
	    (enqueue! q 'b)
	    (provisional-cell-set! c (queue-length q)))
	   (cell-ref c))
	 => 2)
  (check (let ((q (make-queue))
	       (c (make-cell 'OOPS)))
	   (enqueue! q 'a)
	   (ensure-atomicity!
	    (enqueue! q 'b)
	    (provisional-cell-set! c (on-queue? q 'b)))
	   (cell-ref c))
	 => #t)
  ;; The following test is no longer in a comment, but might as well
  ;; stay here.
  (check (let ((q (make-queue))
	       (c (make-cell '())))
	   (enqueue! q 'a)
	   (ensure-atomicity!
	    (enqueue! q 'b)
	    (prov-cell-push! c (delete-from-queue! q 'b))
	    (prov-cell-push! c (maybe-dequeue! q))
	    (prov-cell-push! c (maybe-dequeue! q)))
	   (cell-ref c))
	 => '(#f a #t)))

(define-test-case queue-length queues-tests
  (for-each
   (lambda (n)
     (check (with-queue (q)
                        (stuff-queue! q (srfi-1:iota n))
                        (queue-length q))
            => n))
   '(0 1 2 3 4 5 6 7 8 9 10)))

(define-test-case delete-from-queue! queues-tests
  (for-each
   (lambda (x)
     (check (with-queue (q)
                        (stuff-queue! q '(a b c a b c))
                        (list* x
                               (delete-from-queue! q x)
                               (devour-queue! q)))
            => (list x
                     (x->boolean (memq x '(a b c)))
                     (append (delq x '(a b c))
                             '(a b c)))))
   '(a b c d))
  (for-each
   (lambda (x)
     (check (with-queue (q)
                        (stuff-queue! q '(a b c a b c))
                        (list* x
                               (delete-from-queue! q x)
                               (skip (stuff-queue! q '(d e f)))
                               (devour-queue! q)))
            => (list x
                     (x->boolean (memq x '(a b c)))
                     (append (delq x '(a b c))
                             '(a b c d e f)))))
   '(a b c d)))

(define-test-case on-queue? queues-tests
  (check
   (map (lambda (x)
          (with-queue (q)
                      (stuff-queue! q '(a b c))
                      (on-queue? q x)))
        '(a b c d))
   => '(#t #t #t #f))
  (check
   (map (lambda (x)
          (with-queue (q)
                      (on-queue? q x)))
        '(a b c d))
   => '(#f #f #f #f)))

(define-test-case queue->list queues-tests
  (check (with-queue (q)
                     (stuff-queue! q '(a b c d e f))
                     (list* (queue->list q)
                            (suck-queue! q 2)
                            (skip (stuff-queue! q '(g h)))
                            (queue->list q)
                            (devour-queue! q)))
         => '((a b c d e f)
              (a b)
              (c d e f g h)
              (c d e f g h))))
