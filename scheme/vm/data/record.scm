; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber

; What the VM knows about the layout of records and record types.
; Needs to be synchronized with scheme/rts/record.scm

(define (typed-record? thing)
  (and (record? thing)
       (< 0 (record-length thing))
       (possibly-record-type? (record-ref thing 0))))

(define (record-type record)
  (record-ref record 0))

(define (record-has-type? record type)
  (record-type<=? (record-type record) type))

(define *first-extension-slot* 11)

(define (not-record-type? thing) ; only the RTS knows for sure
  (or (not (record? thing))
      (not (vm-symbol? (record-ref thing 3)))
      (< (record-length thing) (+ *first-extension-slot* 1))))

(define (possibly-record-type? thing)
  (not (not-record-type? thing)))

(define (record-type-name record)
  (vm-symbol->string (record-ref (record-type record) 3)))

(define (record-type-extension-count record)
  (extract-fixnum (record-ref record 8)))

(define (record-type-base record index)
  (record-ref record (+ index *first-extension-slot*)))

(define (record-type<=? rt1 rt2)
  (or (vm-eq? rt1 rt2)
      (let ((ec2 (record-type-extension-count rt2)))
	(and (>= (record-type-extension-count rt1)
		 ec2)
	     (vm-eq? (record-type-base rt1 ec2) rt2)))))
