; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Harald Glab-Plhak

;; test for the new ffi

(define-test-suite  ffi-base-tests )

;; test a simple insert with two bindings

(define-test-case ffi-base-test-case ffi-base-tests 
  (check 
   (let ((result (ffi-add-integer 50)))
     (/ result 10))
   => 50))
