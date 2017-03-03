; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees

; Start up a system that has reified packages.
; COMMAND-PROCESSOR might be either the miniature one or the real one.

(define (start structs-thunk)
  (usual-resumer
   (lambda (arg)
     (initialize-interaction-environment! (structs-thunk))
     (command-processor #f arg))))

(define (initialize-interaction-environment! structs)
  (let ((scheme (cdr (assq 'scheme structs))))
    (let ((tower (delay (cons eval (scheme-report-environment 5)))))
      (set-interaction-environment!
         (make-simple-package (map cdr structs) #t tower 'interaction))
      (set-scheme-report-environment!
         5
	 (make-simple-package (list scheme) #t tower 'r5rs)))))
