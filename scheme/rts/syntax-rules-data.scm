; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Mike Sperber

; These define the format for compiled SYNTAX-RULES patterns and templates,
; which are the shared data between the compilation and expansion phases.

;----------------

(define (make-pattern-variable v rank)
  (vector 'var v rank))

(define (pattern-variable? x)
  (and (vector? x)
       (eq? (vector-ref x 0) 'var)))

(define (pattern-variable-name pattern-var) (vector-ref pattern-var 1))
(define (pattern-variable-rank pattern-var) (vector-ref pattern-var 2))

;----------------

(define (make-ellipsis-form form vars)
  (vector 'ellipsis form vars))

(define (ellipsis-form? x)
  (and (vector? x)
       (eq? (vector-ref x 0) 'ellipsis)))

(define (ellipsis-form-body ellipsis) (vector-ref ellipsis 1))
(define (ellipsis-form-vars ellipsis) (vector-ref ellipsis 2))

;----------------
; Because we use vectors for pattern variables and ellipses we need to
; escape any actual vectors that occur.  This isn't as bad as it might
; seem because vectors in patterns and templates need to be converted
; to lists in any case.

(define (make-vector-marker contents)
  (vector 'vector contents))

(define (vector-marker? x)
  (and (vector? x)
       (eq? (vector-ref x 0) 'vector)))

(define (vector-marker-contents vector-marker) (vector-ref vector-marker 1))
