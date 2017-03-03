; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: David Frese

;; Debugging code

;; To activate/deactivate it, the flag 'debug-mode?' must be set.

;; To see all the debugging infos:
;; set the the flag 'debug-mode?' to #t
;; and remake the GC
;; make compile-twospace-gc or make compile-bibop-gc
;; make scheme48vm
;; make
;; and ./go 
		  
;; Debugging

(define debug-mode? #f)

;; expr is a string
(define (debug expr)
  (if debug-mode? (display-message expr)))

;; expr is an integer
(define (debug-int expr)
  (if debug-mode? (display-integer expr)))

;;just a line
(define (debug-line)
  (if debug-mode?
      (display-message "----------------------")))
