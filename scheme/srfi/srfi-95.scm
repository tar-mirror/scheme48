; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: David van Horn, Mike Sperber, Marcus Crestani

;;; "sort.scm" defines: sorted?, merge, merge!, sort, sort!

(define (sorted? seq less? . opt-key)
  (define key (if (null? opt-key) (lambda (x) x) (car opt-key)))
  (define less-key? (lambda (a b) (less? (key a) (key b))))
  (cond
   ((array? seq)
    (olin:vector-sorted? less-key? (array->vector seq)))
   ((vector? seq)
    (olin:vector-sorted? less-key? seq))
   (else
    (olin:list-sorted? less-key? seq))))

(define (merge a b less? . opt-key)
  (define key (if (null? opt-key) (lambda (x) x) (car opt-key)))
  (define less-key? (lambda (a b) (less? (key a) (key b))))
  (cond
   ((and (vector? a) (vector? b))
    (olin:vector-merge less-key? a b))
   (else
    (olin:list-merge less-key? a b))))

(define (merge! a b less? . opt-key)
  (define key (if (null? opt-key) (lambda (x) x) (car opt-key)))
  (define less-key? (lambda (a b) (less? (key a) (key b))))
  (cond
   ((and (vector? a) (vector? b))
    (let ((v (make-vector (+ (vector-length a) (vector-length b)))))
      (olin:vector-merge! less-key? v a b)
      v))
   (else
    (olin:list-merge! less-key? a b))))

(define (sort seq less? . opt-key)
  (define key (if (null? opt-key) (lambda (x) x) (car opt-key)))
  (define less-key? (lambda (a b) (less? (key a) (key b))))
  (cond 
   ((vector? seq)
    (olin:vector-sort less-key? seq))
   ((array? seq)
    (apply vector->array 
	   (olin:vector-sort less-key? (array->vector seq))
	   '#()
	   (array-dimensions seq)))
   (else
    (olin:list-sort less-key? seq))))

(define (sort! seq less? . opt-key)
  (define key (if (null? opt-key) (lambda (x) x) (car opt-key)))
  (define less-key? (lambda (a b) (less? (key a) (key b))))
  (cond 
   ((vector? seq)
    (olin:vector-sort! less-key? seq)
    seq)
   ((array? seq)
    (let ((v (olin:vector-sort less-key? (array->vector seq))))
      (apply vector->array v '#() (array-dimensions seq))))
   (else
    (olin:list-sort! less-key? seq))))
