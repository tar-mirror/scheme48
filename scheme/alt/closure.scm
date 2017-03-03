; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees



; Closures

(define closure-rtd      (make-record-type 'closure '(template env)))
(define closure?         (record-predicate closure-rtd))
(define make-closure     (record-constructor closure-rtd '(template env)))
(define closure-template (record-accessor closure-rtd 'template))
(define closure-env	 (record-accessor closure-rtd 'env))
