; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey


(define (test)
  (let ((v (make-byte-vector 10))
	(out (current-output-port)))
    (write-number (byte-vector-ref v 4) out)
    (byte-vector-set! v 5 100)
    (write-number (byte-vector-ref v 5) out)
    (write-number (byte-vector-word-ref v 4) out)))

