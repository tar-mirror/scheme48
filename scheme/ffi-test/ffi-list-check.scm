; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Harald Glab-Plhak

;; test for the new ffi

(define-test-suite  ffi-list-tests )

;; test a simple insert with two bindings

(define-test-case ffi-list-test-case ffi-list-tests 
  (check 
   (let ((first-list '(a b c d e f g )))
     (ffi-get-cons-val "a-list" (ffi-working-on-lists first-list)))
   => (cons #\A (cons "a-list" (vector 'a 'b 'c 'd 'e 'f 'g)))))

(define-test-case ffi-list-car/cdr-test ffi-list-tests
  (check
   (let* ((a-list (cons (cons 8 9) (cons 7 6)))
	  (a-list-car (ffi-car a-list))
	  (a-list-cdr (ffi-cdr a-list)))
     (cons a-list-cdr a-list-car))
   => (cons (cons 7 6) (cons 8 9))))

(define-test-case ffi-list-length-test ffi-list-tests
  (check
   (let ((len (ffi-length '(0 9 8 7 6 5 4 3 2 1))))
     len)
   => 10))
