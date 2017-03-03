; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber

(define-structure sicp-test (export sicp-tests)
  (open scheme
	test-suites
	sicp)
  (files sicp-check))

(define-structure misc-test (export misc-tests)
  (open scheme test-suites
	sicp-test)
  (begin
    (define-test-suite misc-tests (sicp-tests))))
