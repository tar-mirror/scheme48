; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Marcus Crestani


; VM-ARCHITECTURE is used in a FOR-SYNTAX clause in the VM package definitions.

(define-structures ((vm-architecture (export stob-data)))
  (open scheme enumerated platform)
  (files (interp arch)))
