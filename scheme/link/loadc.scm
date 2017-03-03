; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees


; Cf. alt/config.scm

(define (load-configuration filename . rest)
  (let ((save filename))
    (dynamic-wind (lambda () (set! *source-file-name* filename))
		  (lambda ()
		    (apply load filename rest))
		  (lambda () (set! *source-file-name* save)))))
(define (%file-name%) *source-file-name*)
(define *source-file-name* "")


; ?

(define-syntax structure-ref
  (syntax-rules ()
    ((structure-ref ?struct ?name)
     (*structure-ref ?struct '?name))))
