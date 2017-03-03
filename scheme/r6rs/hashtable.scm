; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Harald Glab-Plhak, Marcus Crestani

;; R6RS hashtable functions:


;; default size of a hashtable
(define *hashtable-default-init-size* 20)

;; constructors

(define make-eq-hashtable
  (opt-lambda ((size *hashtable-default-init-size*))
	      (make-eq-tlc-table size)))

(define make-eqv-hashtable
  (opt-lambda ((size *hashtable-default-init-size*))
	      (make-eqv-tlc-table size)))

(define make-hashtable
  (opt-lambda (hash-function  equiv (size *hashtable-default-init-size*))
	      (make-non-default-tlc-table hash-function equiv size #f)))

;; predicate

(define (hashtable? hashtable)
  (tlc-table? hashtable))

;; size

(define (hashtable-size hashtable)
  (tlc-table-size hashtable))

;; getter

(define (hashtable-ref hashtable key default)
  (tlc-table-ref hashtable key default))

;; setter

(define (hashtable-set! hashtable key obj)
  (assert-key-equiv/hash-fun hashtable key)
  (tlc-table-set! hashtable key obj))

;; delete

(define (hashtable-delete! hashtable key)
  (tlc-table-delete! hashtable key #f))

;; contains

(define (hashtable-contains? hashtable key)
  (tlc-table-contains? hashtable key))

;; update

(define (hashtable-update! hashtable key proc default)
  (assert-key-equiv/hash-fun hashtable key)
  (tlc-table-update! hashtable key proc default))

;; copy

(define hashtable-copy
  (opt-lambda (hashtable (mutable? #f))
	      (tlc-table-copy hashtable mutable?)))

;; clear

(define hashtable-clear!
  (opt-lambda (hashtable (k #f))
	      (tlc-table-clear! hashtable)
	      (if k
		  (tlc-table-resize! hashtable k))))

;; inspection

(define (hashtable-keys hashtable)
  (tlc-table-keys hashtable))

(define (hashtable-entries hashtable)
  (tlc-table-entries hashtable))

;; hash functions

(define (hashtable-equivalence-function hashtable)
  (tlc-table-equivalence-function hashtable))

(define (hashtable-hash-function hashtable)
  (tlc-table-hash-function hashtable))


(define (hashtable-mutable? hashtable)
  (not (immutable? hashtable)))

;; check restrictions on hash-function and equiv

(define (key-equiv/hash-fun-checker hashtable key)
  (let* ((equiv (hashtable-equivalence-function hashtable))
	 (hash-fun (hashtable-hash-function hashtable)))
    (values (eq? (hash-fun key) (hash-fun ((lambda (e) e) key)))
	    (cond ((string? key) (equiv key ((lambda(k) k) key)))
		  ((pair? key) (equiv key key))
		  ((list? key) (equiv key key))
		  ((symbol? key) (equiv 'test 'test))
		  ((number? key) (equiv 8.8 8.8))
		  (else #t))
	    hash-fun equiv)))

;; assert restrictions

(define (assert-key-equiv/hash-fun hashtable key)
  (call-with-values
      (lambda () (key-equiv/hash-fun-checker hashtable key))
    (lambda (valid-to-hashfun valid-to-equiv hash-fun equiv)
      (if (not valid-to-hashfun)
	  (assertion-violation 'assert-key-equiv/hash-fun
			       "key does not work correctly with hash-fun"
			       key hash-fun))
      (if (not valid-to-equiv)
	  (assertion-violation 'assert-key-equiv/hash-fun
			       "key does not work correctly with equiv"
			       key equiv)))))
