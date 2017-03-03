; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Robert Ransom


; This is file util.scm.

;;;; Utilities

; we import something equivalent to this from `primitives':
; (define (unspecific) (if #f #f))

(define (reduce cons nil list)		;used by length, append, etc.
  (if (null? list)
      nil
      (cons (car list) (reduce cons nil (cdr list)))))

(define (filter pred lst)
  (reduce (lambda (x rest)
	    (if (pred x) (cons x rest) rest))
	  '()
	  lst))

; Position of an object within a list

(define (pos pred)
  (lambda (thing l)
    (let loop ((i 0) (l l))
      (cond ((null? l) #f)
	    ((pred thing (car l)) i)
	    (else (loop (+ i 1) (cdr l)))))))

(define posq (pos eq?))
(define posv (pos eqv?))
(define posqual (pos equal?))

; Is pred true of any element of l?

(define (any pred l)
  ;; (reduce or #f l), sort of
  (if (null? l)
      #f
      (or (pred (car l))
	  (any pred (cdr l)))))

; Is pred true of every element of l?

(define (every pred l)
  ;; (reduce and #t l), sort of
  (if (null? l)
      #t
      (and (pred (car l)) (every pred (cdr l)))))

(define (sublist l start end)
  (if (> start 0)
      (sublist (cdr l) (- start 1) (- end 1))
      (let recur ((l l) (end end))
	(if (= end 0)
	    '()
	    (cons (car l) (recur (cdr l) (- end 1)))))))

(define (last x)
  (if (null? (cdr x))
      (car x)
      (last (cdr x))))

(define (insert x l <)
  (cond ((null? l) (list x))
        ((< x (car l)) (cons x l))
        (else (cons (car l) (insert x (cdr l) <)))))

(define (symbol-append . syms)
  (string->symbol (apply string-append
                         (map symbol->string syms))))

(define (x->boolean x)
  (if x #t #f))

;----------------
; Variations on a theme.
;
; FOLD is a tail-recursive version of REDUCE.

(define (fold folder list accumulator)
  (do ((list list (cdr list))
       (accum accumulator (folder (car list) accum)))
      ((null? list)
       accum)))

(define (fold->2 folder list acc0 acc1)
  (let loop ((list list) (acc0 acc0) (acc1 acc1))
    (if (null? list)
	(values acc0 acc1)
	(call-with-values
	 (lambda ()
	   (folder (car list) acc0 acc1))
	 (lambda (acc0 acc1)
	   (loop (cdr list) acc0 acc1))))))

(define (fold->3 folder list acc0 acc1 acc2)
  (let loop ((list list) (acc0 acc0) (acc1 acc1) (acc2 acc2))
    (if (null? list)
	(values acc0 acc1 acc2)
	(call-with-values
	 (lambda ()
	   (folder (car list) acc0 acc1 acc2))
	 (lambda (acc0 acc1 acc2)
	   (loop (cdr list) acc0 acc1 acc2))))))

(define-syntax receive
  (syntax-rules ()
    ((receive ?vars ?producer . ?body)
     (call-with-values (lambda () ?producer)
       (lambda ?vars . ?body)))))
