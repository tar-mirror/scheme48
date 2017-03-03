; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees

; Condition variables (cells for which readers block until the next write).
;
; (make-condvar [id]) -> condvar
; (condvar-has-value? condvar) -> boolean
; (condvar-value condvar) -> value
; (maybe-commit-and-wait-for-condvar condvar) -> boolean
; (condvar-set! condvar value)

(define-synchronized-record-type condvar :condvar
  (really-make-condvar queue has-value? id)
  (has-value? value)		; synchronize on these
  condvar?
  (queue condvar-queue)
  (has-value? condvar-has-value? set-condvar-has-value?!)
  (value condvar-value set-condvar-value!)
  (id condvar-id))

(define-record-discloser :condvar
  (lambda (condvar)
    (if (condvar-id condvar)
	(list 'condvar (condvar-id condvar))
	'(condvar))))

(define (make-condvar . id-option)
  (really-make-condvar (make-queue)
		       #f
		       (if (null? id-option)
			   #f
			   (car id-option))))

(define (maybe-commit-and-wait-for-condvar condvar . maybe-deadlock?)
  (apply maybe-commit-and-block-on-queue (condvar-queue condvar) maybe-deadlock?))

(define (maybe-commit-and-set-condvar! condvar value)
  (set-condvar-value! condvar value)
  (set-condvar-has-value?! condvar #t)
  (maybe-commit-and-make-ready (condvar-queue condvar)))

(define (condvar-has-waiters? condvar)
  (not (thread-queue-empty? (condvar-queue condvar))))
