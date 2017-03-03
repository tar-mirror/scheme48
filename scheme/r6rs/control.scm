; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Robert Ransom

(define-syntax when
  (syntax-rules ()
    ((when expr body ...)
     (if expr (begin body ...)))))

(define-syntax unless
  (syntax-rules ()
    ((unless expr body ...)
     (if (not expr) (begin body ...)))))
