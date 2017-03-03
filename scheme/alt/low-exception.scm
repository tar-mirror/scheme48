; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber

; The new LOW-EXCEPTIONS in terms of the old SIGNALS

(define signals:error error)
(define signals:note note)

(define (error who message . irritants)
  (apply signals:error message (cons who irritants)))

(define (assertion-violation who message . irritants)
  (error who message irritants))

(define (implementation-restriction-violation who message . irritants)
  (error who message irritants))

(define (warning who message . irritants)
  (apply warn message (cons who irritants)))

(define (syntax-violation who message form . maybe-subform)
  (apply syntax-error message (cons message (cons form maybe-subform))))

(define (note who message . irritants)
  (apply signals:note message (cons message irritants)))

