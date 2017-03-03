; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees
; Session data

; The initializers are saved in images.

(define *session-data-initializers* '())

(define (make-session-data-slot! init)
  (let ((slot (length *session-data-initializers*)))
    (set! *session-data-initializers* (cons init *session-data-initializers*))
    (if (vector? (session-data))
	(set-session-data! (list->vector
			    (reverse
			     (cons init
				   (reverse (vector->list (session-data))))))))
    slot))

(define (session-data-ref slot)
  (vector-ref (session-data) slot))

(define (session-data-set! slot value)
  (vector-set! (session-data) slot value))

(define (initialize-session-data!)
  (set-session-data! (list->vector (reverse *session-data-initializers*))))
