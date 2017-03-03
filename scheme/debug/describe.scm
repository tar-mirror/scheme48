; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees


(define (describe x)
  (if (and (stob? x)
	   (< (stob-type x) least-b-vector-type))
      (let ((tag (string-append (number->string x) ": "))
	    (len (bytes->cells (stob-length-in-bytes x))))
	(do ((i -1 (+ i 1)))
	    ((= i len))
	  (describe-1 (stob-ref x i) tag)))
      (describe-1 x "")))



(define (describe-1 x addr)
  (cond ((fixnum? x) (display " fixnum ") (write (extract-fixnum x)))
	((header? x)
	 (display addr)
	 (if (immutable-header? x)
	     (display " immutable"))
	 (display " header ")
	 (let ((type (header-type x)))
	   (if (< type stob-count)
	       (write (vector-ref stob type))
	       (write type)))
	 (display " ")
	 (write (header-length-in-bytes x)))
	((immediate? x)
	 (cond (else
		(display " immediate ")
		(let ((type (immediate-type x)))
		  (if (< type imm-count)
		      (write (vector-ref imm type))
		      (write type)))
		(display " ")
		(write (immediate-info x)))))
	((stob? x)
	 (display " stob ") (write x))
	(else (display " ? ") (write x)))
  (newline))
