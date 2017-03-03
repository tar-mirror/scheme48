; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey


(define-local-syntax (define-primitive id nargs)
  (let ((args (reverse (list-tail '(z y x) (- '3 nargs)))))
    `(define (,id . ,args)
       (call-primitively ,id . ,args))))

(define-primitive ashr 2)

(define high-bits ashr)

