; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Robert Ransom

(define-test-suite r6rs-comparison-tests)

(define-test-case boolean=?/2 r6rs-comparison-tests
  (check
   (boolean=? #f #f) => #t)
  (check
   (boolean=? #f #t) => #f)
  (check
   (boolean=? #t #f) => #f)
  (check
   (boolean=? #t #t) => #t)
  (check-exception
   (boolean=? 'foo 'foo))
  (check-exception
   (boolean=? 'foo #f))
  (check-exception
   (boolean=? #f 'foo)))

(define-test-case boolean=?/3 r6rs-comparison-tests
  (check
   (boolean=? #f #f #f) => #t)
  (check
   (boolean=? #f #f #t) => #f)
  (check
   (boolean=? #f #t #f) => #f)
  (check
   (boolean=? #f #t #t) => #f)
  (check
   (boolean=? #t #f #f) => #f)
  (check
   (boolean=? #t #f #t) => #f)
  (check
   (boolean=? #t #t #f) => #f)
  (check
   (boolean=? #t #t #t) => #t)
  (check-exception
   (boolean=? #f 'foo 'foo))
  (check-exception
   (boolean=? #f 'foo #f))
  (check-exception
   (boolean=? #f #f 'foo))
  (check-exception
   (boolean=? #f #t 'foo))
  (check-exception
   (boolean=? 'foo #f #f)))

(define-test-case symbol=?/2 r6rs-comparison-tests
  (check
   (symbol=? 'foo 'foo) => #t)
  (check
   (symbol=? 'foo 'bar) => #f)
  (check-exception
   (symbol=? #f 'foo))
  (check-exception
   (symbol=? 'foo #f))
  (check-exception
   (symbol=? #f #f)))

(define-test-case symbol=?/3 r6rs-comparison-tests
  (check
   (symbol=? 'foo 'foo 'foo) => #t)
  (check
   (symbol=? 'foo 'foo 'bar) => #f)
  (check
   (symbol=? 'foo 'bar 'foo) => #f)
  (check
   (symbol=? 'foo 'bar 'bar) => #f)
  (check-exception
   (symbol=? 'foo 'foo #f))
  (check-exception
   (symbol=? 'foo 'bar #f))
  (check-exception
   (symbol=? #f 'foo 'foo))
  (check-exception
   (symbol=? 'foo #f 'foo)))

(define-test-case string=?/2 r6rs-comparison-tests
  (check
   (string=? "foo" "Foo") => #f)
  (check
   (string=? "foo" "foo") => #t)
  (check
   (string=? "foo" "bar") => #f)
  (check-exception
   (string=? "foo" 'bar))
  (check-exception
   (string=? 'foo "bar"))
  (check-exception
   (string=? 'foo 'bar)))

(define-test-case string=?/3 r6rs-comparison-tests
  (check
   (string=? "foo" "foo" "foo") => #t)
  (check
   (string=? "foo" "foo" "Foo") => #f)
  (check
   (string=? "foo" "Foo" "foo") => #f)
  (check
   (string=? "foo" "Foo" "Foo") => #f)
  (check
   (string=? "Foo" "foo" "foo") => #f)
  (check
   (string=? "Foo" "foo" "Foo") => #f)
  (check
   (string=? "Foo" "Foo" "foo") => #f)
  (check
   (string=? "Foo" "Foo" "Foo") => #t)
  (check-exception
   (string=? "foo" "foo" 'foo))
  (check-exception
   (string=? "foo" "bar" 'foo)))

(define-test-case string<?/2 r6rs-comparison-tests
  (check
   (string<? "abb" "abc") => #t)
  (check
   (string<? "abb" "abb") => #f)
  (check-exception
   (string<? "abb" 'abc)))

(define-test-case string<?/3 r6rs-comparison-tests
  (check
   (string<? "abb" "abc" "abc") => #f)
  (check
   (string<? "abb" "abc" "abd") => #t)
  (check
   (string<? "abb" "abb" "abd") => #f)
  (check-exception
   (string<? "abb" "abc" 3))
  (check-exception
   (string<? "abb" "abb" 3)))

; For the remaining (non-case-insensitive) string comparisons, just check
;     that the correct 2-ary comparison is performed.

; An operator (roughly) from Haskell.
; TODO - move into a utility package
(define (liftM2-list-uncurried f xs ys)
  (srfi-1:append-map (lambda (x) (map (lambda (y) (f x y)) ys)) xs))

(define-test-case liftM2-list-uncurried r6rs-comparison-tests
  (check
   (liftM2-list-uncurried list '(1 2 3) '(4 5 6))
   => '((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6))))

(define test-list-1 '("foo" "bar" "baz"))

(define-test-case other-non-ci-comparisons r6rs-comparison-tests
  (check
   (liftM2-list-uncurried string<=? test-list-1 test-list-1)
   => (liftM2-list-uncurried prim:string<=? test-list-1 test-list-1))
  (check
   (liftM2-list-uncurried string>? test-list-1 test-list-1)
   => (liftM2-list-uncurried prim:string>? test-list-1 test-list-1))
  (check
   (liftM2-list-uncurried string>=? test-list-1 test-list-1)
   => (liftM2-list-uncurried prim:string>=? test-list-1 test-list-1)))

; TODO? - move into a utility package?
(define (int-permutations n)
  (cond
   ((not (and (integer? n)
              (exact? n)
              (not (negative? n))))
    (assertion-violation 'int-permutations
                         "expected non-negative exact integer"
                         n))
   ((zero? n)
    '())
   ((prim:= n 1)
    '((0)))
   (else
    (let ((ps-n-1 (int-permutations (- n 1))))
      (let loop ((i (- n 1))
                 (acc '()))
        (if (negative? i)
            acc
            (loop (- i 1)
                  (append (map (lambda (p)
                                 (let ((f (lambda (j)
                                            (if (prim:>= j i)
                                                (+ j 1)
                                                j))))
                                   (cons i (map f p))))
                               ps-n-1)
                          acc))))))))

(define-test-case int-permutations r6rs-comparison-tests
  (check
   (int-permutations 0) => '())
  (check
   (int-permutations 1) => '((0)))
  (check
   (int-permutations 2) => '((0 1) (1 0)))
  (check
   (int-permutations 3) => '((0 1 2)
                             (0 2 1)
                             (1 0 2)
                             (1 2 0)
                             (2 0 1)
                             (2 1 0)))
  (check
   (length (int-permutations 4)) => 24)
  (check
   (length (int-permutations 5)) => 120)
  (check
   (length (int-permutations 6)) => 720))
; (int-permutations 8) overflows the default maximum heap size

; TODO? - move into a utility package?
(define (vector->list-of-permutations v)
  (let* ((n (vector-length v))
         (ps (int-permutations n)))
    (map (lambda (p)
           (map (lambda (i) (vector-ref v i)) p))
         ps)))

(define-test-case vector->list-of-permutations r6rs-comparison-tests
  (check
   (vector->list-of-permutations '#(foo bar baz)) => '((foo bar baz)
                                                       (foo baz bar)
                                                       (bar foo baz)
                                                       (bar baz foo)
                                                       (baz foo bar)
                                                       (baz bar foo))))

(define sharp-s-str (string (integer->char #xDF)))

(define-test-case string-ci=?/4 r6rs-comparison-tests
  (check
   (map (lambda (p) (apply string-ci=? p))
        (vector->list-of-permutations (vector "strasse"
                                              (string-append "Stra" sharp-s-str "e")
                                              "STRASSE"
                                              (string-append "stra" sharp-s-str "e"))))
   => (srfi-1:make-list 24 #t))
  (check
   (map (lambda (p) (apply string-ci=? p))
        (vector->list-of-permutations '#("Hello"
                                         "hello"
                                         "HELLO"
                                         "world")))
   => (srfi-1:make-list 24 #f))
  (check-exception
   (string-ci=? "foo" "foo" 'baz))
  (check-exception
   (string-ci=? "foo" "bar" 'baz)))

(define-test-case string-ci<?/2 r6rs-comparison-tests
  (check
   (string-ci<? "bar" "foo") => #t)
  (check
   (string-ci<? "bar" "FOO") => #t)
  (check
   (string-ci<? "BAR" "bar") => #f)
  (check
   (string-ci<? "FOO" "bar") => #f)
  (check-exception
   (string-ci<? "foo" 'bar)))

(define-test-case string-ci<=?/2 r6rs-comparison-tests
  (check
   (string-ci<=? "bar" "foo") => #t)
  (check
   (string-ci<=? "bar" "FOO") => #t)
  (check
   (string-ci<=? "BAR" "bar") => #t)
  (check
   (string-ci<=? "FOO" "bar") => #f)
  (check-exception
   (string-ci<=? "foo" 'bar)))

(define-test-case string-ci>?/2 r6rs-comparison-tests
  (check
   (string-ci>? "foo" "bar") => #t)
  (check
   (string-ci>? "FOO" "bar") => #t)
  (check
   (string-ci>? "bar" "BAR") => #f)
  (check
   (string-ci>? "bar" "FOO") => #f)
  (check-exception
   (string-ci>? "foo" 'bar)))

(define-test-case string-ci>=?/2 r6rs-comparison-tests
  (check
   (string-ci>=? "foo" "bar") => #t)
  (check
   (string-ci>=? "FOO" "bar") => #t)
  (check
   (string-ci>=? "bar" "BAR") => #t)
  (check
   (string-ci>=? "bar" "FOO") => #f)
  (check-exception
   (string-ci>=? "foo" 'bar)))
