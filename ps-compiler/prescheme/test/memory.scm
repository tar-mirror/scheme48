; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey


(define (test x i)
  (let ((m (allocate-memory 10)))
    (unsigned-byte-set! (+ m i) x)
    (word-set! (+ m i) (+ 1 (word-ref (+ m i))))
    (unsigned-byte-ref (+ m i))))
