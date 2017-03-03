; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees


; Locations

(define location-rtd
  (make-record-type 'location '(id defined? contents)))

(define-record-discloser location-rtd
  (lambda (l) `(location ,(location-id l))))

(define make-undefined-location
  (let ((make (record-constructor location-rtd
				  '(id defined? contents))))
    (lambda (id)
      (make id #f '*empty*))))

(define location? (record-predicate location-rtd))
(define location-id        (record-accessor location-rtd 'id))
(define set-location-id!   (record-modifier location-rtd 'id))
(define location-defined?  (record-accessor location-rtd 'defined?))
(define contents  (record-accessor location-rtd 'contents))

(define set-defined?! (record-modifier location-rtd 'defined?))

(define (set-location-defined?! loc ?)
  (set-defined?! loc ?)
  (if (not ?)
      (set-contents! loc '*empty*)))

(define set-contents!		(record-modifier location-rtd 'contents))
