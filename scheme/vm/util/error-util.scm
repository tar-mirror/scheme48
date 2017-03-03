; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees

(define (error? status)
  (not (eq? status (enum errors no-errors))))

(define (write-error-string string)
  (write-string string (current-error-port)))

(define (write-error-integer integer)
  (write-integer integer (current-error-port)))

(define (error-newline)
  (write-char #\newline (current-error-port)))
