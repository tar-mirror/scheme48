; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber, Robert Ransom
; Copyright (c) 2005-2006 by Basis Technology Corporation. 

; Utilities for setting and constructing text codecs

; This follows Richard Gillam: Unicode Demystified, Chapter 6.
; The BOM is U+FEFF---we look for one at the beginning of the stream.
; Note if this fails, you better re-open the port from the start

; Note that the UTF-32 detection suggested in Gillam's book is not
; practical, as it may confuse valid UTF-16 with UTF-32.

(define (guess-port-text-codec-according-to-bom port)
  (let ((first (peek-byte port)))
    (case first
      ((#xfe)
       (read-byte port)
       (if (eqv? #xff (read-byte port))
	   utf-16be-codec
	   #f))
      ((#xff)
       (read-byte port)
       (if (eqv? #xfe (read-byte port))
	   utf-16le-codec
	   #f))
      ((#xef)
       (read-byte port)
       (if (and (eqv? #xbb (read-byte port))
		(eqv? #xbf (read-byte port)))
	   utf-8-codec
	   #f))
      (else
       #f))))

; The caller really should check the return code
(define (set-port-text-codec-according-to-bom! port)
  (cond
   ((guess-port-text-codec-according-to-bom port)
    => (lambda (text-codec)
	 (set-port-text-codec! port text-codec)
	 #t))
   (else
    #f)))
