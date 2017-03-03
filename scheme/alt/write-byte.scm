; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber

;; It's the best shot, given R5RS.

(define (set-port-crlf?! port val)
  (values))

(define (write-byte byte port)
  (write-char (ascii->char byte) port))
  
