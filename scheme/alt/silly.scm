; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees



(define (reverse-list->string l n)
  ;; Significantly faster than (list->string (reverse l))
  (let ((s (make-string n #\x)))
    (let loop ((i (- n 1)) (l l))
      (if (< i 0) s (begin (string-set! s i (car l))
			   (loop (- i 1) (cdr l)))))))
