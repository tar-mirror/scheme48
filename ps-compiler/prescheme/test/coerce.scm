; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey



(define-local-syntax (define-primitive id nargs)
  (let ((args (reverse (list-tail '(z y x) (- '3 nargs)))))
    `(define (,id . ,args)
       (call-primitively ,id . ,args))))

(define-primitive make-byte-vector 1)
(define-primitive null-pointer? 1)

(define (byte-vector-ref vec index)
  (call-primitively byte-contents-int8 (pointer-add vec index)))

(define (byte-vector-set! vec index value)
  (call-primitively byte-set-contents-int8! (pointer-add vec index) value))

(define (pointer->integer x)
  (call-primitively coerce x '(pointer int8) 'int32))

(define (integer->pointer x)
  (call-primitively coerce x 'int32 '(pointer int8)))

(define (test)
  (let ((bv (make-byte-vector 10)))
    (if (null-pointer? bv)
	100
	(pointer->integer bv))))
