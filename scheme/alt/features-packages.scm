; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees



; The following several packages have Scheme-implementation-specific 
; variants that are much better for one reason or another than
; the generic versions defined here.

(define-structures ((low-exceptions low-exceptions-interface)
		    (handle (export ignore-errors))
		    (features features-interface))
  (open scheme-level-2)
  (files features))

(define-structure records records-interface
  (open scheme-level-2 problems)
  (files record))

(define-structure ascii (export ascii->char char->ascii)
  (open scheme-level-2 problems)
  (files ascii))

(define-structure bitwise bitwise-interface
  (open scheme-level-2 problems)
  (files bitwise))

(define-structure code-vectors code-vectors-interface
  (open scheme-level-1)
  (files code-vectors))
