; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber, Robert Ransom

(define-test-suite matchers-tests)

(define-test-case is matchers-tests
  (check (matches? (is number?) 5))
  (check (not (matches? (is number?) #t)))
  (check (matches? (is equal? 7) 7))
  (check (matches? (is 7) 7))
  (check (not (matches? (is equal? 7) 5))))

(define-test-case anything matchers-tests
  (check (matches? (anything) 7))
  (check (matches? (anything) #f)))

(define-test-case opposite matchers-tests
  (check (not (matches? (opposite (anything)) 7)))
  (check (not (matches? (opposite (anything)) #f))))

(define-test-case is-true matchers-tests
  (check (matches? (is-true) #t))
  (check (matches? (is-true) 5)))

(define-test-case is-false matchers-tests
  (check (matches? (is-false) #f))
  (check (not (matches? (is-false) #t)))
  (check (not (matches? (is-false) 5))))

(define-test-case is-null matchers-tests
  (check (matches? (is-null) '()))
  (check (not (matches? (is-null) #f)))
  (check (not (matches? (is-null) #t)))
  (check (not (matches? (is-null) 5))))

(define-test-case is-within matchers-tests
  (check (matches? (is-within 5 0.01) 5))
  (check (matches? (is-within 5 0.01) 5.0001))
  (check (not (matches? (is-within 5 0.01) 5.1))))

(define-test-case all-of matchers-tests
  (check (matches? (all-of (is-true) (is 5)) 5))
  (check (not (matches? (all-of (is-true) (is-false)) #t))))

(define-test-case any-of matchers-tests
  (check (matches? (any-of (is-false) (is 5)) 5))
  (check (matches? (any-of (is 5) (is-false)) 5))
  (check (not (matches? (any-of (is 5) (is-false)) #t))))

(define-test-case list-where-all matchers-tests
  (check (matches? (list-where-all (is odd?)) '(1 3 5)))
  (check (not (matches? (list-where-all (is odd?)) '(1 2 5)))))

(define-test-case list-where-any matchers-tests
  (check (matches? (list-where-any (is even?)) '(1 2 5)))
  (check (not (matches? (list-where-any (is even?)) '(1 3 5)))))

