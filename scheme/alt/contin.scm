; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees


; Continuations implemented as vectors.

(define *continuation-marker* (list '*continuation-marker*))

(define (make-continuation len init)
  (let ((c (make-vector (+ len 1) init)))
    (vector-set! c 0 *continuation-marker*)
    c))

(define (continuation? obj)
  (and (vector? obj)
       (> (vector-length obj) 0)
       (eq? (vector-ref obj 0) *continuation-marker*)))

(define (continuation-length c) (- (vector-length c) 1))
(define (continuation-ref c i) (vector-ref c (+ i 1)))
(define (continuation-set! c i x) (vector-set! c (+ i 1) x))
