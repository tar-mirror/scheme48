; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: David Frese

;; exported for use by c/bibop/generation_gc.c

(define (s48-trace-continuation contents-pointer size)
  (trace-continuation contents-pointer size)
  (unspecific))

