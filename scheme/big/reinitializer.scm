; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber

; A record type whose only purpose is to run some code when we start up an
; image.

(define-record-type reinitializer :reinitializer
  (make-reinitializer thunk)
  reinitializer?
  (thunk reinitializer-thunk))

(define-record-discloser :reinitializer
  (lambda (r)
    (list 'reinitializer (reinitializer-thunk r))))

(define-record-resumer :reinitializer
  (lambda (r)
    ((reinitializer-thunk r))))

(define-syntax define-reinitializer
  (syntax-rules ()
    ((define-reinitializer name thunk)
     (define name
       (make-reinitializer thunk)))))

  
