; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Michael Zabka, Marcus Crestani

(define-structure srfi-13-test (export srfi-13-tests)
  (open (modify scheme
		(hide string-fill! string->list string-copy))
	test-suites matchers
	srfi-13)
  (files srfi-13-check))

(define-structure srfi-14-test (export srfi-14-tests)
  (open scheme test-suites matchers
	unicode
	srfi-14)
  (files srfi-14-check))

(define-structure srfi-19-test (export srfi-19-tests)
  (open scheme matchers
	srfi-9 ; DEFINE-RECORD-PROCEDURES
        (modify os-time (hide time?))
        srfi-19
	test-suites
        formats)
  (files srfi-19-check))

(define-structure srfi-95-test (export srfi-95-tests)
  (open (modify scheme (hide equal?)) test-suites srfi-63 srfi-95)
  (files srfi-95-check))

(define-structure portable-srfi-test (export portable-srfi-tests)
  (open scheme test-suites
	srfi-13-test srfi-14-test srfi-95-test srfi-19-test)
  (begin
    (define-test-suite portable-srfi-tests 
      (srfi-19-tests srfi-13-tests srfi-14-tests srfi-95-tests))))

(define-structure srfi-test (export portable-srfi-tests posix-srfi-tests srfi-tests)
  (open scheme test-suites
	portable-srfi-test
	srfi-19-test)
  (begin
    (define-test-suite posix-srfi-tests (srfi-19-tests))
    (define-test-suite srfi-tests (portable-srfi-tests posix-srfi-tests))))
