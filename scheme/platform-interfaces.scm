; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Marcus Crestani, David Frese


; Platform-specific constants

(define-interface platform-interface
  (export bytes-per-cell
	  log-bytes-per-cell
	  bits-per-byte
	  bits-per-cell
	  addressing-units-per-cell
	  c-useful-bits-per-word
	  s48-useful-bits-per-word
	  unused-field-width
	  tag-field-width
	  data-field-width
	  immediate-type-field-width
	  pre-scheme-integer-size))
