; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Robert Ransom

; Required structures:
;   scheme-level-0
;   byte-vectors
;   (also requires not and eqv?, loaded from rts/base.scm into scheme-level-1)

(define (equal?-recursor x y
                         memo)
  (or (eqv? x y)
      (let ((memo-pair (assq x memo)))
        (and (pair? memo-pair)
             (eqv? (cdr memo-pair) y)))
        (cond
         ((pair? x)
          (and (pair? y)
               (let ((new-memo (cons (cons x y) memo)))
                 (and (equal?-recursor (car x) (car y) new-memo)
                      (equal?-recursor (cdr x) (cdr y) new-memo)))))
         ((vector? x)
          (and (vector? y)
               (let ((vlx (vector-length x)))
                 (and (= vlx (vector-length y))
                      (or (= vlx 0)
                          (let ((new-memo (cons (cons x y) memo)))
                            (do ((i 0 (+ i 1)))
                                ((or (= i vlx)
                                     (not (equal?-recursor (vector-ref x i)
                                                           (vector-ref y i)
                                                           new-memo)))
                                 (= i vlx)))))))))
         ((string? x)
          (and (string? y)
               (string=? x y)))
         ((byte-vector? x)
          (and (byte-vector? y)
               (byte-vector=? x y)))
         (else #f))))

(define (equal? x y)
  (equal?-recursor x y
                   '()))
