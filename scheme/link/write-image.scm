; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber


; Writing out a Scheme 48 image

(define (write-image file start-proc id-string)
  (if (not (= 0 (remainder bits-per-cell bits-per-io-byte)))
      (assertion-violation 'write-image "io-bytes to not fit evenly into cells"))
  (initialize-memory)
  (call-with-output-file file
    (lambda (port)
      (set-port-crlf?! port #f)
      (let ((start (transport start-proc)) ; transport the start-proc
	    (false (transport #f)))
	(display id-string port)
	(newline port)
	(write-page port)
	(newline port)
	(display architecture-version port)
	(newline port)
        (display "0" port)		; image format; must be synchronized with
					; IMAGE-FORMAT in image-util.scm
        (newline port)
	(boot-write-number bytes-per-cell        port)
	(boot-write-number 0                     port) ; newspace begin
	(boot-write-number (a-units->cells *hp*) port)
	(boot-write-number false                 port) ; symbol table
	(boot-write-number false                 port) ; imported bindings
	(boot-write-number false                 port) ; exported bindings
	(boot-write-number false                 port) ; resumer records
	(boot-write-number start                 port) ; start-proc
	(write-page port)
	(write-descriptor 1 port)	; endianness indicator
	(write-heap port))))		; write out the heap
  )

(define bits-per-io-byte 8)    ; for writing images

(define (write-page port)
  (write-char (ascii->char 12) port))

(define io-byte-mask
  (low-bits -1 bits-per-io-byte))

;(define bits-per-cell    -- defined in data.scm
;  (* bits-per-byte bytes-per-cell))

(define (big-endian-write-descriptor thing port)
  (let loop ((i (- bits-per-cell bits-per-io-byte)))
    (cond ((>= i 0)
           (write-byte (bitwise-and io-byte-mask
				    (arithmetic-shift thing (- 0 i))) port)
           (loop (- i bits-per-io-byte))))))

(define (little-endian-write-descriptor thing port)
  (let loop ((i 0))
    (cond ((< i bits-per-cell)
           (write-byte (bitwise-and io-byte-mask
				    (arithmetic-shift thing (- 0 i))) port)
           (loop (+ i bits-per-io-byte))))))

(define write-descriptor little-endian-write-descriptor)

;; writing characters as Unicode code points
(define bits-per-scalar-value-unit
  (* bits-per-byte bytes-per-scalar-value-unit))

(define (write-scalar-value scalar-value port)
  (let loop ((i 0))
    (cond ((< i bits-per-scalar-value-unit)
           (write-byte (bitwise-and io-byte-mask
				    (arithmetic-shift scalar-value (- 0 i)))
		       port)
           (loop (+ i bits-per-io-byte))))))

(define (boot-write-number n port)
  (display n port)
  (newline port))
