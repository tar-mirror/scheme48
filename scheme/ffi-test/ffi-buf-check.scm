; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Harald Glab-Plhak

;; test for the new ffi

(define-test-suite ffi-buf-tests)

(define-test-case ffi-local-bufs ffi-buf-tests
  (check (external-ffi-make-local-buf))
  (check (external-ffi-free-local-buf))
  (check (external-ffi-free-local-buf-1))
  (check (external-ffi-free-local-buf-2))
  (check (external-ffi-free-local-buf-3)))
