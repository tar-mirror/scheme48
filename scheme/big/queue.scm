; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, David Frese, Mike Sperber,
; Robert Ransom, Taylor Campbell

;;;; Queues

;;; Following Taylor Campbell's suggestion, the elements in a queue
;;; are stored in an ordinary list, with a dummy pair at the beginning
;;; of the list.  The queue record maintains pointers to both the
;;; dummy pair (in the HEAD field) and the last pair in the list (in
;;; the TAIL field).
;;;
;;; With this representation, the only fields that queue operations
;;; need to mutate are cdrs of pairs and the TAIL field of the queue
;;; record, and the TAIL field needs to be set if and only if a pair's
;;; cdr is set to '().  This allows all queue mutations to be
;;; performed by two procedures, SPLICE-IN-QUEUE-LIST! and
;;; SPLICE-OUT-OF-QUEUE!, which are simple, easy to use correctly,
;;; and, when used correctly, maintain the queue's invariant (that the
;;; TAIL field points to the last pair in the queue's list).
;;;
;;; The procedures exported from this module never give away pointers
;;; to the pairs in a queue's list, and never attach pairs provided by
;;; other code to a queue's list.  Once pairs are in a queue's list,
;;; their CARs are never modified, so there is no need to use
;;; PROVISIONAL-CAR.  However, pairs' CDRs are (necessarily) modified
;;; by ENQUEUE! and other queue operations, so all accesses to and
;;; modifications of CDRs must be provisional.

(define-synchronized-record-type queue :queue
  (really-make-queue head tail)
  (tail)          	;synchronize on these
  queue?
  ;; Despite their names, the tail's accessor and modifier are both
  ;; provisional.
  (head real-queue-head)
  (tail queue-tail set-queue-tail!))

;; A few of the comments below use the following utility functions:
;;
;; (define (func-exp/1 f n)
;;   (lambda (x)
;;     (do ((x x (f x))
;;          (n n (- n 1)))
;;         ((= n 0) x))))
;;
;; (define (nth-cdr n x)
;;   ((func-exp/1 cdr n) x))
;;
;; (define (nth-prov-cdr n x)
;;   ((func-exp/1 provisional-cdr n) x))
;;
;; (define (prov-cell-push! c x)
;;   (ensure-atomicity!
;;    (provisional-cell-set! c (cons x (provisional-cell-ref c)))))

;;; Unique IDs and discloser for debugging.

(define *next-queue-list-uid* (make-cell 1))

(define (next-queue-list-uid)
  (atomically
    (let ((uid (provisional-cell-ref *next-queue-list-uid*)))
      (provisional-cell-set! *next-queue-list-uid* (+ uid 1))
      uid)))

(define (queue-uid q)
  (car (real-queue-head q)))

(define-record-discloser :queue
  (lambda (q)
    (list 'queue (queue-uid q))))

;;; Constructors.

;; MAKE-QUEUE - Create a new, empty queue.
(define (make-queue)
  (let ((head (cons (next-queue-list-uid) '())))
    (really-make-queue head head)))

;; LIST->QUEUE - Create a new queue containing a list of elements.
;;
;; This does not use the other queue operations because they would add
;; unnecessary synchronization overhead.  Even if this procedure
;; temporarily set the current proposal to #F, each call to ENQUEUE!
;; would create and commit a proposal unnecessarily.
(define (list->queue xs)
  (call-with-values
      (lambda ()
	(list->queue-list xs))
    really-make-queue))

;;; Internal utilities.

;; LIST->QUEUE-LIST - Copies a list, prepending a head pair, and
;; returns the head pair and the last pair in the copy (or null, if
;; the original list is empty).
;;
;; Throws an exception if XS is an improper list.
(define list->queue-list                      ;cons-only version
  (let ()
    (define (loop xs)
      (if (null? (cdr xs))
	  (let ((tail (cons (car xs) '())))
	    (values tail tail))
	  (receive (head tail) (loop (cdr xs))
		   (values (cons (car xs) head) tail))))
    (lambda (xs)
      (loop (cons (next-queue-list-uid) xs)))))

;; (define (list->queue-list xs)                 ;side-effecting version
;;   (let ((result-head (cons (next-queue-list-uid) '())))
;;     (let loop ((xs xs)
;; 	       (prev-result-pair result-head))
;;       (if (null? xs)
;; 	  (values result-head prev-result-pair)
;; 	  (let ((cur-result-pair (cons (car xs) '())))
;; 	    (set-cdr! prev-result-pair cur-result-pair)
;; 	    (loop (cdr xs) cur-result-pair))))))

;; (define (list->queue-list xs)                 ;alternate cons-only version
;;   (if (null? xs)
;;       (let ((result-head (cons (next-queue-list-uid) '())))
;; 	(values result-head result-head))
;;       (receive (head tail)
;; 	       (let loop ((xs xs))
;; 		 (if (null? (cdr xs))
;; 		     (let ((result-tail (cons (car xs) '())))
;; 		       (values result-tail result-tail))
;; 		     (receive (head tail) (loop (cdr xs))
;; 			      (values (cons (car xs) head) tail))))
;; 	       (values (cons (next-queue-list-uid) head) tail))))

;; SPLICE-IN-QUEUE-LIST! - Inserts a list into a queue.
;;
;; This function must be called with a proposal active.  No argument
;; checking is performed.
;;
;; Preconditions:
;;
;; - Q must be a queue.
;;
;; - (QUEUE-TAIL Q) must be (NTH-PROV-CDR k (REAL-QUEUE-HEAD Q)) for
;;   some exact non-negative integer k.
;;
;; - (PROVISIONAL-CDR (QUEUE-TAIL Q)) must be the empty list.
;;
;; - PAIR-BEFORE-INSERTION must be a pair.
;;
;; - PAIR-BEFORE-INSERTION must be (NTH-PROV-CDR m (REAL-QUEUE-HEAD
;;   Q)) for some exact non-negative integer m.
;;
;; - SPLICE-HEAD-PAIR must be a pair.
;;
;; - SPLICE-TAIL-PAIR must be a pair.
;;
;; - SPLICE-TAIL-PAIR must be (NTH-CDR n SPLICE-HEAD-PAIR) for some
;;   exact non-negative integer n.
;;
;; - Each pair reachable as (NTH-CDR i SPLICE-HEAD-PAIR) for some
;;   exact non-negative integer i such that (<= i n) must not have
;;   been accessed or modified provisionally within any active
;;   proposal.
;;
;; Postconditions:
;;
;; - (QUEUE-TAIL Q) is (NTH-PROV-CDR k2 (REAL-QUEUE-HEAD Q)) for some
;;   exact non-negative integer k2.
;;
;; - (PROVISIONAL-CDR (QUEUE-TAIL Q)) is the empty list.
;;
;; - (PROVISIONAL-CDR PAIR-BEFORE-INSERTION) is EQ? to (CDR
;;   SPLICE-HEAD-PAIR).
;;
;; - (PROVISIONAL-CDR SPLICE-TAIL-PAIR) is EQ? to the value of
;;   (PROVISIONAL-CDR PAIR-BEFORE-INSERTION) when this function was
;;   called.
(define (splice-in-queue-list! q
			       pair-before-insertion
			       splice-head-pair
			       splice-tail-pair)
  (if (not (eq? splice-head-pair splice-tail-pair))
      (begin
	(let ((new-splice-tail-cdr (provisional-cdr pair-before-insertion)))
	  (set-cdr! splice-tail-pair new-splice-tail-cdr)
	  (if (null? new-splice-tail-cdr)
	      (set-queue-tail! q splice-tail-pair)))
	(provisional-set-cdr! pair-before-insertion
			      (cdr splice-head-pair)))))

;; SPLICE-OUT-OF-QUEUE! - Removes a piece of a queue's list.
;;
;; This function must be called with a proposal active.  No argument
;; checking is performed.
;;
;; Preconditions:
;;
;; - Q must be a queue.
;;
;; - (QUEUE-TAIL Q) must be (NTH-PROV-CDR k (REAL-QUEUE-HEAD Q)) for
;;   some exact non-negative integer k.
;;
;; - (PROVISIONAL-CDR (QUEUE-TAIL Q)) must be the empty list.
;;
;; - SPLICE-HEAD-PAIR must be a pair.
;;
;; - SPLICE-HEAD-PAIR must be (NTH-PROV-CDR m (REAL-QUEUE-HEAD Q)) for
;;   some exact non-negative integer m.
;;
;; - SPLICE-TAIL-PAIR must be a pair.
;;
;; - SPLICE-TAIL-PAIR must be (NTH-PROV-CDR n SPLICE-HEAD-PAIR) for
;;   some exact non-negative integer n.
;;
;; Postconditions:
;;
;; - (QUEUE-TAIL Q) is (NTH-PROV-CDR k2 (REAL-QUEUE-HEAD Q)) for some
;;   exact non-negative integer k2.
;;
;; - (PROVISIONAL-CDR (QUEUE-TAIL Q)) is the empty list.
;;
;; - (PROVISIONAL-CDR SPLICE-HEAD-PAIR) is EQ? to the value of
;;   (PROVISIONAL-CDR SPLICE-TAIL-PAIR) when this function was called.
(define (splice-out-of-queue! q
			      splice-head-pair
			      splice-tail-pair)
  (if (not (eq? splice-head-pair splice-tail-pair))
      (let ((splice-tail-cdr (provisional-cdr splice-tail-pair)))
	(provisional-set-cdr! splice-head-pair splice-tail-cdr)
	(if (null? splice-tail-cdr)
	    (set-queue-tail! q splice-head-pair)))))

;; ENQUEUE-MANY-NO-COPY! - Attach a list (provided as (CDR HEAD)) to
;; the tail of the queue.  TAIL must be the last pair in HEAD (since
;; (CDR HEAD) is a list, HEAD is a non-empty list).
;;
;; No argument checking is performed.
(define (enqueue-many-no-copy! q head tail)
  (ensure-atomicity!
   (splice-in-queue-list! q (queue-tail q) head tail)))

;; QUEUE-PROC-CALLER-*REALLY*-MESSED-UP! - Removes the current
;; proposal and raises an error with a rather more useful message than
;; the fool^H^H^H^Hprogrammer who provoked this error deserves.
(define (queue-proc-caller-*really*-messed-up! who q)
  (define the-nasty-message
    " called on empty or inconsistent queue with a proposal active")
  (remove-current-proposal!)
  (assertion-violation who
                       (string-append (symbol->string who)
                                      the-nasty-message)
                       q))

;; MAKE-EMPTY-QUEUE-DIE-THUNK - Adequately described by its name.
(define (make-empty-queue-die-thunk who q)
  (lambda ()
    (if (proposal-active?)
	(queue-proc-caller-*really*-messed-up! who q)
	(assertion-violation who "empty queue" q))))

;; FOO-OR-VALUE->FOO-OR-THUNK/1/0 - Converts a procedure which takes a
;; default value and returns it on failure to a procedure which takes
;; a thunk and tail-calls it on failure.
;;
;; This procedure should be moved to a utility package and generated
;; by a macro.
(define foo-or-value->foo-or-thunk/1/0        ;1 arg before VALUE, 0 after
  (lambda (foo-or-value)
    (let ((unreleased (make-cell 'unreleased)))
      (lambda (b/0 thunk)
	(let ((result (foo-or-value b/0 unreleased)))
	  (if (eq? result unreleased)
	      (thunk)
	      result))))))

;;; The exported queue operations.

;; QUEUE-EMPTY? - Returns #F if the queue is not empty, or #T if the
;; queue is empty.
(define (queue-empty? q)
  ;; ENSURE-ATOMICITY is not necessary here, as this function makes
  ;; only one call to a provisional function (PROVISIONAL-CDR).
  (null? (provisional-cdr (real-queue-head q))))

;; ENQUEUE! - Enqueue one element.
(define (enqueue! q v)
  ;; ENSURE-ATOMICITY! is not necessary here, as ENQUEUE-MANY-NO-COPY!
  ;; uses it for us.
  (let ((p (cons v '())))
    (enqueue-many-no-copy! q (cons 'dummy p) p)))

;; ENQUEUE-MANY! - Enqueue a list of elements.
(define (enqueue-many! q xs)
  ;; ENSURE-ATOMICITY! is not necessary here, and not using it reduces
  ;; the risk of raising an exception (while traversing a
  ;; caller-provided value as a list) with a proposal active.
  (call-with-values
      (lambda ()
	(list->queue-list xs))
    (lambda (head tail)
      (enqueue-many-no-copy! q head tail))))

;; QUEUE-HEAD-OR-VALUE - Return the first element in the queue, or
;; return VALUE if the queue is empty.
(define (queue-head-or-value q value)
  ;; ENSURE-ATOMICITY is not necessary here.
  (let ((first-pair (provisional-cdr (real-queue-head q))))
    (if (null? first-pair)
	value
	(car first-pair))))

;; QUEUE-HEAD-OR-THUNK - Return the first element in the queue, or
;; tail-call THUNK if the queue is empty.
;;
;; THUNK is tail-called so that, if this function is called without a
;; proposal active, THUNK will not use the proposal created by this
;; function.  This is especially important if THUNK raises an
;; exception.
(define queue-head-or-thunk
  (foo-or-value->foo-or-thunk/1/0 queue-head-or-value))

;; QUEUE-HEAD - Return the first element in the queue, or raise an
;; error if the queue is empty.
;;
;; DO NOT CALL THIS FUNCTION WITH A PROPOSAL ACTIVE UNLESS
;; QUEUE-EMPTY? HAS RETURNED #F!
(define (queue-head q)
  (let ((die-thunk (make-empty-queue-die-thunk 'queue-head q)))
    (queue-head-or-thunk q die-thunk)))

;; MAYBE-QUEUE-HEAD - Return the first element in the queue, or return
;; #F if the queue is empty.
(define (maybe-queue-head q)
  (queue-head-or-value q #f))

;; DEQUEUE-OR-VALUE! - Remove and return the first element in the
;; queue, or return VALUE if the queue is empty.
(define (dequeue-or-value! q value)
  (ensure-atomicity
   (let* ((head (real-queue-head q))
	  (first-pair (provisional-cdr head)))
     (if (null? first-pair)
	 value                                ;empty queue
	 (begin
	   (splice-out-of-queue! q head first-pair)
	   (car first-pair))))))

;; DEQUEUE-OR-THUNK! - Remove and return the first element in the
;; queue, or tail-call THUNK if the queue is empty.
;;
;; THUNK is tail-called here for the same reason as it is in
;; QUEUE-HEAD-OR-THUNK.
(define dequeue-or-thunk!
  (foo-or-value->foo-or-thunk/1/0 dequeue-or-value!))

;; DEQUEUE! - Remove and return the first element in the queue, or
;; raise an error if the queue is empty.
;;
;; DO NOT CALL THIS FUNCTION WITH A PROPOSAL ACTIVE UNLESS
;; QUEUE-EMPTY? HAS RETURNED #F!
(define (dequeue! q)
  (let ((die-thunk (make-empty-queue-die-thunk 'dequeue! q)))
    (dequeue-or-thunk! q die-thunk)))

;; MAYBE-DEQUEUE! - Remove and return the first element in the queue,
;; or return #F if the queue is empty.
(define (maybe-dequeue! q)
  (dequeue-or-value! q #f))

;; EMPTY-QUEUE! - Make the queue empty.
(define (empty-queue! q)
  (ensure-atomicity!
   (splice-out-of-queue! q (real-queue-head q) (queue-tail q))))

;;; Queue operations not used in the Scheme 48 system, and known to be
;;; *very* slow.  These operations may be removed from this package in
;;; a future revision.

;; These operations could be made to run faster when called without an
;; active proposal by locking out all other threads from accessing the
;; queue and then using non-provisional operations on the queue's
;; list.  This would require another field in the queue record type
;; and one additional provisional read in each of the queue operations
;; above.  The operations below would still run slowly when called
;; with a proposal active.

;; QUEUE->LIST - Return a list of the elements in the queue.
(define (queue->list q)
  (ensure-atomicity
   (let loop ((qp (provisional-cdr (real-queue-head q))))
     (if (null? qp)
	 '()
	 ;; The next line must use PROVISIONAL-CDR; see below.
	 (cons (car qp) (loop (provisional-cdr qp)))))))
;; If LOOP were applied to (CDR QP) above, the following code would
;; return a value EQUAL? to '(a):
;;
;; (let ((q (make-queue))
;;       (c (make-cell 'OOPS)))
;;   (enqueue! q 'a)
;;   (ensure-atomicity!
;;    (enqueue! q 'b)
;;    (provisional-cell-set! c (queue->list q)))
;;   (cell-ref c))
;;
;; The result should be EQUAL? to '(a b).

;; QUEUE-LENGTH - Return the number of elements in the queue.
;;
;; QUEUE-LENGTH could be sped up by having all queue-modifying
;; operations maintain a count of the number of elements in the queue.
;; This would make the queue operations which *are* currently used in
;; the system much slower (e.g. ENQUEUE! currently performs 4 or 5
;; provisional operations on 2 or 3 locations; maintaining a queue
;; length counter would require it to perform another provisional read
;; and write on another location).
(define (queue-length q)
  (ensure-atomicity
   (let loop ((acc 0)
	      (qp (provisional-cdr (real-queue-head q))))
     (if (null? qp)
	 acc
	 ;; The next line must use PROVISIONAL-CDR; see below.
	 (loop (+ acc 1) (provisional-cdr qp))))))
;; If LOOP were applied to (CDR QP) above, the following code would
;; return a value EQUAL? to 1:
;;
;; (let ((q (make-queue))
;;       (c (make-cell 'OOPS)))
;;   (enqueue! q 'a)
;;   (ensure-atomicity!
;;    (enqueue! q 'b)
;;    (provisional-cell-set! c (queue-length q)))
;;   (cell-ref c))
;;
;; The result should be EQUAL? to 2.

;; ON-QUEUE? - Returns #T if VALUE is currently in the queue (as
;; determined by EQV?), and returns #F if VALUE is not in the queue.
(define (on-queue? q value)
  (ensure-atomicity
   (let loop ((qp (provisional-cdr (real-queue-head q))))
     (cond
      ((null? qp)
       #f)
      ((eqv? value (car qp))
       #t)
      (else
       ;; The next line must use PROVISIONAL-CDR; see below.
       (loop (provisional-cdr qp)))))))
;; If LOOP were applied to (CDR QP) above, the following code would
;; return a value EQUAL? to #f:
;;
;; (let ((q (make-queue))
;;       (c (make-cell 'OOPS)))
;;   (enqueue! q 'a)
;;   (ensure-atomicity!
;;    (enqueue! q 'b)
;;    (provisional-cell-set! c (on-queue? q 'b)))
;;   (cell-ref c))
;;
;; The result should be EQUAL? to #t.

;; DELETE-FROM-QUEUE-IF! - INTERNAL - Removes the first element in the
;; queue satisfying PRED; returns #T if an element is removed, #F
;; otherwise.
;;
;; PRED is called with a proposal active.  PRED must not raise an
;; exception, and should not have side effects.
;;
;; Because of these restrictions on PRED and the fact that this
;; procedure may be removed due to its sloth, DELETE-FROM-QUEUE-IF! is
;; not exported.
(define (delete-from-queue-if! q pred)
  (ensure-atomicity
   (let ((head (real-queue-head q)))
     (let loop ((prev-pair head)
		(cur-pair (provisional-cdr head)))
       (cond
	((null? cur-pair)
	 #f)
	((pred (car cur-pair))
	 (splice-out-of-queue! q prev-pair cur-pair)
	 #t)
	(else
	 (loop cur-pair (provisional-cdr cur-pair))))))))

;; DELETE-FROM-QUEUE! - Removes the first element in the queue EQV? to
;; VALUE; returns #T if an element is removed, #F otherwise.
(define (delete-from-queue! q value)
  (delete-from-queue-if! q (lambda (x) (eqv? value x))))
