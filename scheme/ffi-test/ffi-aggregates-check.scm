; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Harald Glab-Plhak,

;; test for the new ffi

(define-record-type any-record :any-record
  (make-any-record id name address)
  any-record?
  (id any-record-id)
  (name any-record-name)
  (address any-record-address))

(define-test-suite  ffi-aggregates-tests )

(define-test-case ffi-make-set-refrec/vect ffi-aggregates-tests
  (check 
   (let ((my-rec (make-any-record 10 'Hans 'ZurSchnecke)))
     (ffi-record-set! my-rec 1 'Hugo)
     (let ((result (ffi-make-vector 3 0)))
       (ffi-vector-set! result 0 (any-record-id my-rec))
       (ffi-vector-set! result 1 (ffi-record-ref my-rec 1))
       (ffi-vector-set! result 2 (ffi-record-ref my-rec 2))
       (list (any-record-id my-rec) result)))
   => '(10 #(10 Hugo ZurSchnecke))))

(define-test-case enum-set-test ffi-aggregates-tests
  (check 
   (let ((col-enum-set (ffi-get-color-enum-set 3)))
     (ffi-enums col-enum-set))
   => 3))
