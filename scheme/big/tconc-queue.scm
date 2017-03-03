; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Marcus Crestani

;; tconc queue for transport link cells
;; Teitelman 1974

(define (make-tconc-queue)
  (let ((q (cons #f #f)))
    (cons q q)))

(define (tconc-queue? thing)
  (and (pair? thing)
       (pair? (car thing))
       (pair? (cdr thing))))

(define (tconc-queue-empty? tconc)
  (and (tconc-queue? tconc) 
       (eq? (car tconc) (cdr tconc))))

(define (tconc-queue-enqueue! tconc value)
  (let ((newpair (cons #f #f)))
    (set-car! (cdr tconc) value)
    (set-cdr! (cdr tconc) newpair)
    (set-cdr! tconc newpair)))

(define (tconc-queue-dequeue! tconc)
  (if (tconc-queue-empty? tconc)
      (assertion-violation 'tconc-queue-dequeue "empty tconc queue" tconc)
      (let ((element (car (car tconc))))
	(set-car! tconc (cdr (car tconc)))
	element)))

(define (tconc-queue-peek tconc)
  (if (tconc-queue-empty? tconc)
      (assertion-violation 'tconc-queue-peek "empty tconc queue" tconc)
      (car (car tconc))))

(define (tconc-queue-clear! tconc)
  (let ((q (cons #f #f)))
    (set-car! tconc q)
    (set-cdr! tconc q)))

(define (tconc-queue-size tconc)
  (if (and tconc (pair? tconc))
      (let loop-tconc ((x (car tconc))
		       (count 0))
	(if (or (eq? x (cdr tconc))
		(not (pair? x)))
	    count
	    (loop-tconc (cdr x) (+ count 1))))
      (assertion-violation 'tconc-queue-size "not a valid tconc" tconc)))
