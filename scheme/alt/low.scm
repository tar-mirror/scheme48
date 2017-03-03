; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber


; Portable versions of low-level things that would really like to rely
; on the Scheme 48 VM or on special features provided by the byte code
; compiler.

(define (vector-unassigned? v i) #f)

(define maybe-open-input-file open-input-file)
(define maybe-open-output-file open-output-file)

(define (unspecific) (if #f #f))
