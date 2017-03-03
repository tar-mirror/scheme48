; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees

(define (make-population)
  (list '<population>))

(define (add-to-population! x pop)
  (if (not x) (assertion-violation 'add-to-population! "can't put #f in a population"))
  (if (not (weak-memq x (cdr pop)))
      (set-cdr! pop (cons (make-weak-pointer x) (cdr pop)))))

(define (weak-memq x weaks)
  (if (null? weaks)
      #f
      (if (eq? x (weak-pointer-ref (car weaks)))
	  weaks
	  (weak-memq x (cdr weaks)))))

(define (population-reduce cons nil pop)
  (do ((l (cdr pop) (cdr l))
       (prev pop l)
       (m nil (let ((w (weak-pointer-ref (car l))))
		(if w
		    (cons w m)
		    (begin (set-cdr! prev (cdr l))
			   m)))))
      ((null? l) m)))

(define (population->list pop)
  (population-reduce cons '() pop))

(define (walk-population proc pop)
  (population-reduce (lambda (thing junk) (proc thing))
		     #f
		     pop))

