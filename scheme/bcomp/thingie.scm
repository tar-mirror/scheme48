; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber

; A thingie (placecard?) is used to hold a spot for a location that is to be
; found later.  The compiler sticks them in templates and the module system
; later replaces them with locations.
;
; We can't use (BEGIN ...) for this trivial package because it is loaded
; by flatload, which can't handle them.

(define-record-type thingie :thingie
  (make-thingie binding name assigned?)
  thingie?
  (binding thingie-binding)
  (name thingie-name)
  (assigned? thingie-assigned? set-thingie-assigned?!))

