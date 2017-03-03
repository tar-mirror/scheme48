; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees

(define make-record-type   #'scheme-translator::make-record-type)
(define record-constructor #'scheme-translator::record-constructor)
(define record-accessor    #'scheme-translator::record-accessor)
(define record-modifier    #'scheme-translator::record-modifier)
(define record-predicate   #'scheme-translator::record-predicate)
(define define-record-discloser #'scheme-translator::define-record-discloser)

(define (record-type? x)
  (lisp:if (scheme-translator::record-type-descriptor-p x) #t #f))
(define record-type-field-names #'scheme-translator::rtd-field-names)
(define record-type-name	#'scheme-translator::rtd-identification)

; Internal record things, for inspector or whatever
(define disclose-record #'scheme-translator::disclose-record)
(define record-type #'scheme-translator::record-type)
(define (record? x) (lisp:if (scheme-translator::record-type x) #t #f))

