; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Harald Glab-Plhak, Marcus Crestani

;; test for the new ffi

(define-test-suite ffi-misc-tests)

(define-test-case ffi-call-scheme-test ffi-misc-tests
  (check 
   (let ((result (ffi-call-scheme (lambda args
				    (apply + args)) 
				  
				  3 4 5 6)))
     result)
   => 15))

(define-test-case ffi-call-scheme-assertion ffi-misc-tests
  (call-with-current-continuation
   (lambda (esc)
     (with-exception-handler
      (lambda (c)
        (esc
	 (call-with-values
	   (lambda () (decode-condition c))
	   (lambda (type who message more-stuff)
	     (check type => 'assertion-violation)
	     (check who => 'ffi)))))
      (lambda ()
        (ffi-call-scheme 
	 (lambda (ignore-1 ignore-2 ignore-3)
	   (assertion-violation 'ffi "Testing if exceptions from externally called Scheme code work.")
	   'ignore)
	 3 esc 2 3)
        (check #f => 'should-never-reach-this-point))))))

(define-test-case ffi-values-test ffi-misc-tests
  (check
   (let ((value (ffi-make-strange-value 10 "LolliPop")))
     (let ((result(ffi-strange-value->list value)))
       (ffi-strange-value-free value)
       result))
   => '(10 . "LolliPop")))

(define-test-case ffi-weak-pointer ffi-misc-tests
  (let ((w (make-weak-pointer (cons 23 42))))
    (check (equal? (cons 23 42) (weak-pointer-ref w)))
    (collect)
    (check (not (weak-pointer-ref w)))))

(define-test-case ffi-weak-pointer-2 ffi-misc-tests
  (let ((w (ffi-make-weak-pointer (cons 23 42))))
    (check (ffi-weak-pointer? w))
    (check (equal? (cons 23 42) (ffi-weak-pointer-ref w)))
    (collect)
    (check (not (ffi-weak-pointer-ref w)))))

(define-test-case ffi-strings-test ffi-misc-tests
  (let ((string "Grüße"))
    (let ((latin-1 (ffi-check-string-latin-1 string))
	  (utf-8 (ffi-check-string-utf-8 string)))
      (check (eq? (byte-vector-ref latin-1 2) 252))
      (check (eq? (byte-vector-ref latin-1 3) 223))
      (check (eq? (byte-vector-ref utf-8 3) 188))
      (check (eq? (byte-vector-ref utf-8 5) 159)))))

(define-test-case ffi-extract-byte-vector-test ffi-misc-tests
  (let ((bv (make-byte-vector 10 97)))
    (check (ffi-extract-byte-vector bv))))

(define-test-case ffi-extract-byte-vector-test-readonly ffi-misc-tests
  (let ((bv (make-byte-vector 10 97)))
    (check (ffi-extract-byte-vector-readonly bv))
    (check (byte-vector-ref bv 4) => 97)
    (check (byte-vector-ref bv 8) => 97)))

(define-test-case ffi-extract-and-modify-byte-vector-test ffi-misc-tests
  (let ((bv (make-byte-vector 10 97)))
    (check (ffi-extract-and-modify-byte-vector bv))
    (check (eq? (byte-vector-ref bv 5) 53))))

(define-test-case ffi-extract-twice-and-modify-byte-vector-test ffi-misc-tests
  (let ((bv (make-byte-vector 10 97)))
    (check (ffi-extract-twice-and-modify-byte-vector bv))
    (check (eq? (byte-vector-ref bv 4) 52))
    (check (eq? (byte-vector-ref bv 8) 56))))

(define (maybe-make-unmovable-byte-vector count init)
  (call-with-current-continuation
   (lambda (esc)
     (with-exception-handler
      (lambda (c)
	(esc
	 (call-with-values
           (lambda () (decode-condition c))
           (lambda (type who message more-stuff)
             (check type => 'assertion-violation)
             (check who => 'make-unmovable-byte-vector)
	     (check message => "unimplemented instruction")
	     (newline)
	     (display "Warning: Unmovable byte vectors are not supported by the GC.")(newline)
	     (display "         Using a regular byte vector for this test.")(newline)
	     (make-byte-vector count init)))))
      (lambda ()
        (make-unmovable-byte-vector count init))))))


(define-test-case ffi-extract-unmovable-byte-vector-test ffi-misc-tests
  (let ((bv (maybe-make-unmovable-byte-vector 10 97)))
    (check (ffi-extract-byte-vector bv))))

(define-test-case ffi-extract-and-modify-unmovable-byte-vector-test ffi-misc-tests
  (let ((bv (maybe-make-unmovable-byte-vector 10 97)))
    (check (ffi-extract-and-modify-byte-vector bv))
    (check (eq? (byte-vector-ref bv 5) 53))))

(define-test-case ffi-extract-twice-and-modify-unmovable-byte-vector-test ffi-misc-tests
  (let ((bv (maybe-make-unmovable-byte-vector 10 97)))
    (check (ffi-extract-twice-and-modify-byte-vector bv))
    (check (eq? (byte-vector-ref bv 4) 52))
    (check (eq? (byte-vector-ref bv 8) 56))))

(define-test-case ffi-extract-byte-vector-and-call-scheme-test ffi-misc-tests
  (let* ((bv (make-byte-vector 10 97))
	 (callback (lambda ()
		     (check (byte-vector-ref bv 4) => 52)
		     (check (byte-vector-ref bv 8) => 56)
		     (byte-vector-set! bv 4 98)
		     (byte-vector-set! bv 8 98))))
    (check (ffi-extract-byte-vector-and-call-scheme bv callback))
    (check (byte-vector-ref bv 4) => 56)
    (check (byte-vector-ref bv 8) => 52)))

(define-test-case ffi-extract-byte-vector-assertion-test ffi-misc-tests
  (let ((bv (make-byte-vector 10 97)))
    (call-with-current-continuation
     (lambda (esc)
       (with-exception-handler
	(lambda (c)
	  (esc
	   (call-with-values
	     (lambda () (decode-condition c))
	     (lambda (type who message more-stuff)
	       (check type => 'assertion-violation)
	       (check who => "ffi_extract_byte_vector_assertion")
	       (check message => "throw back to Scheme")
	       (check (byte-vector-ref bv 4) => 52)
	       (check (byte-vector-ref bv 8) => 56)
               (let ((thrown-bv (car more-stuff)))
  	         (check (byte-vector-ref thrown-bv 4) => 52)
	         (check (byte-vector-ref thrown-bv 8) => 56))
	       (byte-vector-set! bv 4 98)
	       (byte-vector-set! bv 8 98)))))
	(lambda ()
	  (ffi-extract-byte-vector-assertion bv)
	  (check #f => 'should-never-reach-this-point)))))))

(define-test-case ffi-extract-unmovable-byte-vector-and-call-scheme-test ffi-misc-tests
  (let* ((bv (maybe-make-unmovable-byte-vector 10 97))
	 (callback (lambda ()
		     (check (byte-vector-ref bv 4) => 52)
		     (check (byte-vector-ref bv 8) => 56)
		     (byte-vector-set! bv 4 98)
		     (byte-vector-set! bv 8 98))))
    (check (ffi-extract-byte-vector-and-call-scheme bv callback))
    (check (byte-vector-ref bv 4) => 56)
    (check (byte-vector-ref bv 8) => 52)))

(define-test-case ffi-extract-unmovable-byte-vector-assertion-test ffi-misc-tests
  (let ((bv (maybe-make-unmovable-byte-vector 10 97)))
    (call-with-current-continuation
     (lambda (esc)
       (with-exception-handler
	(lambda (c)
	  (esc
	   (call-with-values
	     (lambda () (decode-condition c))
	     (lambda (type who message more-stuff)
	       (check type => 'assertion-violation)
	       (check who => "ffi_extract_byte_vector_assertion")
	       (check message => "throw back to Scheme")
	       (check (byte-vector-ref bv 4) => 52)
	       (check (byte-vector-ref bv 8) => 56)
               (let ((thrown-bv (car more-stuff)))
  	         (check (byte-vector-ref thrown-bv 4) => 52)
	         (check (byte-vector-ref thrown-bv 8) => 56))
	       (byte-vector-set! bv 4 98)
	       (byte-vector-set! bv 8 98)))))
	(lambda ()
	  (ffi-extract-byte-vector-assertion bv)
	  (check #f => 'should-never-reach-this-point)))))))
