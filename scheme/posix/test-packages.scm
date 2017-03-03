; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Robert Ransom

(define-structure posix-core-test (export posix-core-tests)
  (open scheme test-suites matchers sort threads
	util		; every
        (with-prefix srfi-1 srfi-1:)
        threads
        (subset threads-internal (terminate-thread!))
        placeholders
        queues
	posix-files
	posix-time
	posix-users
	posix-i/o
	posix-process-data
	posix-processes
	os-strings)
  (files check))

(define-structure regexp-test (export regexp-tests)
  (open scheme test-suites matchers
	regexps)
  (files regexp-check))

(define-structure posix-test (export posix-tests)
  (open scheme test-suites
	posix-core-test
	regexp-test)
  (begin
    (define-test-suite posix-tests (posix-core-tests regexp-tests))))
