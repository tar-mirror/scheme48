; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber

(define-structure cml-test (export rendezvous-channels-tests
				   rendezvous-jars-tests
				   rendezvous-placeholders-tests
				   with-nack-tests
				   cml-tests ; all combined
				   )
  (open scheme test-suites matchers
	threads
	(subset srfi-1 (identity iota lset= lset<=))
	rendezvous rendezvous-channels
	rendezvous-jars
	rendezvous-placeholders)
  (files cml-check))

