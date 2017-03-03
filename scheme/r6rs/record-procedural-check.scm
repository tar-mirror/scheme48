; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber

(define-test-suite r6rs-records-procedural-tests)

(define :point
  (make-record-type-descriptor
   'point #f
   #f #f #f 
   '#((mutable x) (mutable y))))
(define :point-cd
  (make-record-constructor-descriptor :point #f #f))
(define make-point (record-constructor :point-cd))

(define point? (record-predicate :point))
(define point-x (record-accessor :point 0))
(define point-y (record-accessor :point 1))
(define point-x-set! (record-mutator :point 0))
(define point-y-set! (record-mutator :point 1))

(define-test-case point r6rs-records-procedural-tests
  (let ((p1 (make-point 1 2)))
      
    (check (point? p1))
    (check (point-x p1) => 1)
    (check (point-y p1) => 2)
    (point-x-set! p1 5)
    (check (point-x p1) => 5)))

(define :point2
  (make-record-type-descriptor
    'point2 :point 
    #f #f #f '#((mutable x) (mutable y))))

(define make-point2
  (record-constructor
    (make-record-constructor-descriptor :point2
      #f #f)))

(define point2? (record-predicate :point2))
(define point2-xx (record-accessor :point2 0))
(define point2-yy (record-accessor :point2 1))

(define-test-case point2 r6rs-records-procedural-tests
  (let ((p2 (make-point2 1 2 3 4)))
    (check (point? p2) => #t)
    (check (point-x p2) => 1)
    (check (point-y p2) => 2)
    (check (point2-xx p2) => 3)
    (check (point2-yy p2) => 4)))

(define :point-cd/abs
  (make-record-constructor-descriptor
   :point #f
   (lambda (new)
     (lambda (x y)
       (new (abs x) (abs y))))))

(define make-point/abs
  (record-constructor :point-cd/abs))

(define-test-case point/abs r6rs-records-procedural-tests
  (check (point-x (make-point/abs -1 -2))
	 => 1)
  (check (point-y (make-point/abs -1 -2)) 
	 => 2))

(define :cpoint
  (make-record-type-descriptor
   'cpoint :point
   #f #f #f
   '#((mutable rgb))))

(define make-cpoint
  (record-constructor
   (make-record-constructor-descriptor
    :cpoint :point-cd
    (lambda (p)
      (lambda (x y c)
        ((p x y) (color->rgb c)))))))

(define make-cpoint/abs
  (record-constructor
   (make-record-constructor-descriptor
    :cpoint :point-cd/abs
    (lambda (p)
      (lambda (x y c)
        ((p x y) (color->rgb c)))))))

(define cpoint-rgb
  (record-accessor :cpoint 0))

(define (color->rgb c)
  (cons 'rgb c))

(define-test-case cpoint r6rs-records-procedural-tests
  (check (cpoint-rgb (make-cpoint -1 -3 'red))
	 => '(rgb . red))
  (check (point-x (make-cpoint -1 -3 'red)) 
	 => -1)
  (check (point-x (make-cpoint/abs -1 -3 'red)) 
	 => 1))
