; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Marcus Crestani

; R6RS Scheme reader with ##

; ## should evaluate to the last REPL result.

(define-sharp-macro #\#
  (lambda (c port)
    (read-char port)
    ((current-sharp-sharp) port)))

; Read a single form, allowing ## as a way to refer to last command
; output.
(define (read-form port)
  (with-sharp-sharp (make-node (get-operator 'quote)
			       (list 'quote (focus-object)))
    (lambda () (get-datum port))))
