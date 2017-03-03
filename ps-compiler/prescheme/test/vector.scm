; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey


; Oops - this is polymorphic!

(define (vector+length-fill! v length x)
  (do ((i 0 (+ i 1)))
      ((>= i length))
    (vector-set! v i x)))

(define *v* (unassigned))

(define (test x)
  (set! *v* (make-vector 10))
  (vector+length-fill! *v* 10 3)
  (vector-ref *v* x))

;(define (find-port-index)
;  (let loop ((i 0))
;    (cond ((>= i 10)
;           -1)
;          ((= 3 (vector-ref *v* i))
;           i)
;          (else (loop (+ i 1))))))
;
;(define (foo)
;  (let loop ((i (find-port-index)))
;    (if (>= i 5)
;        (let ((v *v*))
;          (bar)
;          (vector-set! v i (baz)))
;        (loop (find-port-index)))))
