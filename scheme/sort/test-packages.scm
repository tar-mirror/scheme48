; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber

(define-structure sort-test (export sort-tests)
  (open scheme test-suites matchers
	srfi-27
	list-merge-sort
	vector-heap-sort vector-merge-sort vector-insertion-sort
	vector-quick-sort vector-quick-sort3
	sorted)
  (files check))
