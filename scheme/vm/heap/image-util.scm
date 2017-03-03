; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, David Frese, Mike Sperber

(define-enumeration area-type-size
  (small large weaks))

; at least one of these needs to be synchronized with what the linker writes
(define-enumeration image-format
  (two-space bibop)) 

(define (valid-image-format? f)
  (enum-case image-format f
    ((two-space bibop) #t)
    (else #f)))
