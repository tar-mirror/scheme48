; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber


; This is file pseudoscheme-features.scm.
; Synchronize any changes with all the other *-features.scm files.

(define *scheme-file-type* #f) ;For fun


; SIGNALS

(define (error who message . irritants)
  (apply #'ps:scheme-error message irritants))

(define (assertion-violation who message . irritants)
  (apply #'ps:scheme-error message irritants))

(define (implementation-restriction-violation who message . irritants)
  (apply #'ps:scheme-error message irritants))

(define (warning who message . irritants)
  (apply #'ps:scheme-warn message irritants))

(define (note who message . irritants)
  (apply #'ps:scheme-warn message irritants))

(define (syntax-violation who message form . maybe-subform)
  (apply warning who message form maybe-subform)
  ''syntax-error)

; FEATURES

(define force-output #'lisp:force-output)

(define (string-hash s)
  (let ((n (string-length s)))
    (do ((i 0 (+ i 1))
         (h 0 (+ h (lisp:char-code (string-ref s i)))))
        ((>= i n) h))))

(define (make-immutable! thing) thing)
(define (immutable? thing) #f)


; BITWISE

(define arithmetic-shift #'lisp:ash)
(define bitwise-and #'lisp:logand)
(define bitwise-ior #'lisp:logior)
(define bitwise-not #'lisp:lognot)


; ASCII

(define char->ascii #'lisp:char-code)
(define ascii->char #'lisp:code-char)
(define ascii-limit lisp:char-code-limit)
(define ascii-whitespaces '(32 10 9 12 13))


; CODE-VECTORS

(define (make-code-vector len . fill-option)
  (lisp:make-array len :element-type '(lisp:unsigned-byte 8)
		       :initial-element (if (null? fill-option)
					    0
					    (car fill-option))))

(define (code-vector? obj)
  (ps:true? (lisp:typep obj
			(lisp:quote (lisp:simple-array (lisp:unsigned-byte 8)
						       (lisp:*))))))

(define (code-vector-ref bv k)
  (lisp:aref (lisp:the (lisp:simple-array (lisp:unsigned-byte 8) (lisp:*))
		       bv)
	     k))

(define (code-vector-set! bv k val)
  (lisp:setf (lisp:aref (lisp:the (lisp:simple-array (lisp:unsigned-byte 8)
						     (lisp:*))
				  bv)
			k)
	     val))

(define (code-vector-length bv)
  (lisp:length (lisp:the (lisp:simple-array (lisp:unsigned-byte 8) (lisp:*))
			 bv)))

(define (write-byte byte port)
  (write-char (ascii->char byte) port))

; The rest is unnecessary in Pseudoscheme versions 2.8d and after.

;(define eval #'schi:scheme-eval)
;(define (interaction-environment) schi:*current-rep-environment*)
;(define scheme-report-environment
;  (let ((env (scheme-translator:make-program-env
;              'rscheme
;              (list scheme-translator:revised^4-scheme-module))))
;    (lambda (n)
;      n ;ignore
;      env)))

; Dynamic-wind.
;
;(define (dynamic-wind in body out)
;  (in)
;  (lisp:unwind-protect (body)
;    (out)))
;
;(define values #'lisp:values)
;
;(define (call-with-values thunk receiver)
;  (lisp:multiple-value-call receiver (thunk)))
