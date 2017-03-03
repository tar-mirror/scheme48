; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber

; Retrofit the RTS's condition type as R6RS records.

(define-retrofitted-record-type &condition rts:&condition #f #f #f)

(define-retrofitted-record-type (&message &condition) rts:&message #f #f #f (immutable message))
(define-retrofitted-record-type (&warning &condition) rts:&warning #f #f #f)
(define-retrofitted-record-type (&serious &condition) rts:&serious #f #f #f)
(define-retrofitted-record-type (&error &serious)  rts:&error #f #f #f)
(define-retrofitted-record-type (&violation &serious) rts:&violation #f #f #f)
(define-retrofitted-record-type (&non-continuable &violation) rts:&non-continuable #f #f #f)
(define-retrofitted-record-type (&implementation-restriction &violation) rts:&implementation-restriction #f #f #f)
(define-retrofitted-record-type (&lexical &violation) rts:&lexical #f #f #f)
(define-retrofitted-record-type (&syntax &violation) rts:&syntax  #f #f #f 
  (immutable form) (immutable subform))
(define-retrofitted-record-type (&undefined &violation) rts:&undefined #f #f #f)
(define-retrofitted-record-type (&assertion &violation) rts:&assertion #f #f #f)
(define-retrofitted-record-type (&irritants &condition) rts:&irritants #f #f #f)
(define-retrofitted-record-type (&who &condition) rts:&who #f #f #f)

(define-syntax define-condition-type
  (syntax-rules ()
    ((define-condition-type ?name ?supertype ?constructor ?predicate
       (?field1 ?accessor1) ...)
     (begin
       (define rts-supertype (record-type-descriptor ?supertype))
       (rts:define-condition-type rts-name rts-supertype ?constructor ?predicate
				  (?field1 ?accessor1) ...)
       ;; the default discloser uses the wrong name; overwrite
       (define-record-discloser rts-name
	 (lambda (r)
	   (list '?name (?accessor1 r) ...)))
       (define-retrofitted-record-type (?name ?supertype) rts-name #f #f #f
	 (immutable ?field1) ...)))))
