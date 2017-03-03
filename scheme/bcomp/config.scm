; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber


; For DEFINE-STRUCTURE macro

(define (make-a-package opens-thunk accesses-thunk tower reader
			dir clauses name)
  (let ((package
	 (make-package opens-thunk accesses-thunk
		       #t    ;unstable
		       tower
		       dir
		       clauses
		       #f
		       name)))
    (set-package-reader! package reader)
    package))

(define (loser . rest)
  (assertion-violation 'init-defpackage! "init-defpackage! neglected"))

(define interface-of structure-interface)

(define *verify-later!* (lambda (thunk) #f))

(define (verify-later! thunk)
  (*verify-later!* thunk))

(define (set-verify-later! proc)
  (set! *verify-later!* proc))

(define (note-name! thing name)
  (cond ((interface? thing)
	 (note-interface-name! thing name))
	((structure? thing)
	 (note-structure-name! thing name)))
  thing)
