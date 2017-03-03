; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber

(define-test-suite r6rs-records-syntactic-tests)

(define-record-type (pare kons pare?)
  (fields kar kdr))

(define-test-case pare r6rs-records-syntactic-tests
  (let ((p1 (kons 1 2)))
    (check (pare? p1))
    (check (not (pare? 5)))
    (check (pare-kar p1) => 1)
    (check (pare-kdr p1) => 2)))

(define-record-type point
  (fields (immutable x)
          (mutable y))
  (nongenerative
    point-4893d957-e00b-11d9-817f-00111175eb9e))

(define-record-type (cpoint make-cpoint cpoint?)
  (parent point)
  (protocol
   (lambda (n)
     (lambda (x y c) 
       ((n x y) (color->rgb c)))))
  (fields
    (mutable rgb cpoint-rgb cpoint-rgb-set!)))

(define (color->rgb c)
  (cons 'rgb c))

(define-test-case point r6rs-records-syntactic-tests
  (let ((p1 (make-point 1 2))
	(p2 (make-cpoint 3 4 'red)))
    (check (point? p1) => #t)
    (check (point-x p1) => 1)
    (check (point-y p1) => 2)
    (check (point? (vector)) => #f)
    (check (point? (cons 'a 'b)) => #f)
    (point-y-set! p1 17)
    (check (point-y p1) => 17)
    (check (record-rtd p1) => (record-type-descriptor point))))

(define-test-case cpoint r6rs-records-syntactic-tests
  (let ((p1 (make-point 1 2))
	(p2 (make-cpoint 3 4 'red)))
    (check (cpoint? p1) => #f)
    (check (point? p2) => #t)
    (check (cpoint? p2) => #t)
    (check (point-x p2) => 3)
    (check (point-y p2) => 4)
    (check (cpoint-rgb p2) => '(rgb . red))))
    
(define-record-type (ex1 make-ex1 ex1?)
  (protocol (lambda (p) (lambda a (p a))))
  (fields (immutable f ex1-f)))

(define-test-case ex1 r6rs-records-syntactic-tests
  (let ((ex1-i1 (make-ex1 1 2 3)))
    (check (ex1-f ex1-i1) => '(1 2 3))))

(define-record-type (ex2 make-ex2 ex2?)
  (protocol
    (lambda (p) (lambda (a . b) (p a b))))
  (fields (immutable a ex2-a)
          (immutable b ex2-b)))

(define-test-case ex2 r6rs-records-syntactic-tests
  (let ((ex2-i1 (make-ex2 1 2 3)))
    (check (ex2-a ex2-i1) => 1)
    (check (ex2-b ex2-i1) => '(2 3))))

(define-record-type (unit-vector
                     make-unit-vector
                     unit-vector?)
  (protocol
   (lambda (p)
     (lambda (x y z)
       (let ((length 
               (sqrt (+ (* x x)
                        (* y y)
                        (* z z)))))
         (p (/ x length)
            (/ y length)
            (/ z length))))))
  (fields (immutable x unit-vector-x)
          (immutable y unit-vector-y)
          (immutable z unit-vector-z)))

(define *ex3-instance* #f)

(define-record-type ex3
  (parent cpoint)
  (protocol
   (lambda (n)
     (lambda (x y t)
       (let ((r ((n x y 'red) t)))
         (set! *ex3-instance* r)
         r))))
  (fields 
   (mutable thickness))
  (sealed #t) (opaque #t))

(define-test-case ex3 r6rs-records-syntactic-tests
  (let ((ex3-i1 (make-ex3 1 2 17)))
    (check (ex3? ex3-i1) => #t)
    (check (cpoint-rgb ex3-i1) => '(rgb . red))
    (check (ex3-thickness ex3-i1) => 17)
    (ex3-thickness-set! ex3-i1 18)
    (check (ex3-thickness ex3-i1) => 18)
    (check *ex3-instance* => ex3-i1)
    (check (record? ex3-i1) => #f)))

; static record type with parent
(define-record-type ppoint
  (parent point)
  (fields
    (immutable smell)))

(define-test-case ppoint r6rs-records-syntactic-tests
  (let ((pp (make-ppoint 1 2 'bad)))
    (check (point-x pp) => 1)
    (check (point-y pp) => 2)
    (check (ppoint-smell pp) => 'bad)))
