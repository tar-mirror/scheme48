; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: David Frese, Marcus Crestani


; Platform-specific constants

(define-structure ps-platform platform-interface
  (open prescheme)
  (files ((data) platform-32)))
