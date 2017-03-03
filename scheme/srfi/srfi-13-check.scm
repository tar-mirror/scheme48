; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber

(define-test-suite srfi-13-tests)

(define-test-case string-contains srfi-13-tests
  (check (string-contains "ab" "ab") => 0)
  (check (string-contains "xabc" "ab") => 1)
  (check (string-contains "aabc" "ab") => 1)
  (check (string-contains "abaabaaabaaaa" "aaa") => 5)
  (check-that (string-contains "abcdef" "cdf") (is-false)))

