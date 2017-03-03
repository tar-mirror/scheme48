; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber


; This is file features.scm.
; Synchronize any changes with all the other *-features.scm files.

; These definitions should be quite portable to any Scheme implementation.
; Assumes Revised^5 Report Scheme, for EVAL and friends.


; LOW-EXCEPTIONS

(define (error who message . irritants)
  (display-error-message "Error" who message irritants)
  (an-error-occurred-now-what?))

(define (assertion-violation who message . irritants)
  (display-error-message "Assertion violation" who message irritants)
  (an-error-occurred-now-what?))
  (error who message irritants))

(define (implementation-restriction-violation who message . irritants)
  (display-error-message "Assertion violation" who message irritants)
  (an-error-occurred-now-what?))

(define (warning who message . irritants)
  (display-error-message "Warning" who message irritants))
  
(define (syntax-violation who message form . maybe-subform)
  (apply display-error-message "Syntax violation" who message form maybe-subform)
  ''syntax-error)

(define (note who message . irritants)
  (display-error-message "Note" who message irritants))
  
(define (display-error-message heading who message irritants)
  (display heading)
  (display " [") (display who) (display "]: ")
  (display message)
  (newline)
  (let ((spaces (list->string
		 (map (lambda (c) #\space) (string->list heading)))))
    (for-each (lambda (irritant)
		(display spaces)
		(write irritant)
		(newline))
	      irritants)))

; Linker also needs SIGNAL, SYNTAX-ERROR, CALL-ERROR

; FEATURES

(define (force-output port) #f)

(define current-noise-port current-output-port)

(define (string-hash s)
  (let ((n (string-length s)))
    (do ((i 0 (+ i 1))
         (h 0 (+ h (char->ascii (string-ref s i)))))
        ((>= i n) h))))

(define (make-immutable! thing) thing)
(define (immutable? thing) #f)

; BITWISE -- use alt/bitwise.scm (!)
; ASCII -- use alt/ascii.scm
; CODE-VECTORS -- use alt/code-vectors.scm
; BINARY I/O -- check out alt/write-byte.scm
