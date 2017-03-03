; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Michael Zabka

; ,config ,load scheme/big/os-time-check.scm
; (define-test-suite all-tests (os-time-tests))
; (run-test-suite all-tests)

(define-test-suite os-time-tests)

(define-test-case time/1 os-time-tests
  (let ((current-time (current-utc-time)))
    (check (and (time? current-time)
		(number? (time-seconds current-time))
		(number? (time-microseconds current-time))
		(<= 0 (time-microseconds current-time) (expt 10 6))))))

(define-test-case timezone-offset os-time-tests
  (let ((current-offset (timezone-offset)))
    (check (and (number? current-offset)
		(<= -43200 current-offset 50400)))))
