; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Harald Glab-Plhak

;; test for the new ffi

(define-test-suite  ffi-binding-tests )

;; test a simple insert with two bindings

(define-test-case ffi-export-bind-test-case ffi-binding-tests 
  (check 
   (let ((my-rec (ffi-make-a-record "test-id")))
     (list (a-record-id my-rec) (a-record-type my-rec) (a-record-value my-rec)) )
   => (list "test-id" "type" "a-value")))

(define-test-case ffi-import-bind-test-case ffi-binding-tests 
  (check 
   (let ((the-binding (ffi-get-imp-binding "a-status")))
     (let ((bind-value (ffi-get-imp-value-by-binding the-binding)))
       bind-value))
   => 80))

(define-test-case ffi-bind-test-with-direct-param ffi-binding-tests 
  (check 
   (let ((the-binding (ffi-get-imp-binding "a-status")))
     (ffi-a-status-set-by-binding!  70)
     (let ((bind-value (ffi-get-imp-value-by-binding the-binding)))
       (equal? bind-value (ffi-a-status))))))


(define-test-case ffi-bind-check-and-get-name ffi-binding-tests 
  (check 
   (let ((bind-name "a-status"))
     (equal? bind-name (ffi-check-a-status-and-get-name)))))
