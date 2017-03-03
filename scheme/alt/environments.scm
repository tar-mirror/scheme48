; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees


(define (*structure-ref struct name)
  (eval name (interaction-environment)))

