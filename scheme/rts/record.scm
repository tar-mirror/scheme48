; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber

;;;; Records

; Every record in the image is assumed to be made either by MAKE-RECORD-TYPE
; or by a procedure returned by record-constructor.  A record-type is a
; record that describes a type of record.  At the end of the file we create
; a record type that describes record types.

; We number the record types for debugging purposes.

(define *record-type-uid* -1)

; This is the record type that describes record types.  It is set a the end
; of the file.  Its first slot points to itself.

(define *record-type* #f)

; Make a record type from a name, used for printing and debugging, and
; a list of field names.
;
; The VM references both the record type and the resumer (in
; heap/write-image.scm), as well as the extension-slot index and the
; extension count (in vm/data/record.scm), so their offsets should
; not be changed.

(define *first-extension-slot* 11)

(define (make-record-type name field-names . maybe-parent)
  (set! *record-type-uid* (+ 1 *record-type-uid*))
  (let* ((parent
	  (if (pair? maybe-parent)
	      (car maybe-parent)
	      #f))
	 (extension-count
	  (if parent
	      (+ 1 (record-type-extension-count parent))
	      0))
	 (size (length field-names))
	 (rt (make-record (+ 1 extension-count *first-extension-slot*) (unspecific))))

    (record-set! rt 0 *record-type*)
    (record-set! rt 1 default-record-resumer)
    (record-set! rt 2 *record-type-uid*)
    (record-set! rt 3 name)
    (record-set! rt 4 field-names)
    (record-set! rt 5 size)
    (record-set! rt 6 (make-default-record-discloser name))
    (record-set! rt 7 parent)
    (record-set! rt 8 extension-count)
    (record-set! rt 9 (+ (if parent
			     (record-type-size parent)
			     0)
			 size))
    
    (if parent
	(do ((i 0 (+ 1 i)))
	    ((= i extension-count))
	  (record-set! rt (+ i *first-extension-slot*)
		       (record-type-base parent i))))
    (record-set! rt (+ extension-count *first-extension-slot*) rt)

    rt))

(define (record-type? obj)
  (and (record? obj)
       (eq? (record-type obj) *record-type*)))

; The various fields in a record type.

(define (record-type-resumer rt)          (record-ref rt 1))
(define (set-record-type-resumer! rt r)   (record-set! rt 1 r))
(define (record-type-uid rt)              (record-ref rt 2))
(define (record-type-name rt)             (record-ref rt 3))
(define (record-type-field-names rt)      (record-ref rt 4))
(define (record-type-number-of-fields rt) (record-ref rt 5))
(define (record-type-discloser rt)        (record-ref rt 6))
(define (set-record-type-discloser! rt d) (record-set! rt 6 d))
(define (record-type-parent rt)           (record-ref rt 7))
(define (record-type-extension-count rt)  (record-ref rt 8))
(define (record-type-size rt)             (record-ref rt 9))
; for additional stuff not used here, i.e. sealed, opaque etc.
(define (record-type-data rt)             (record-ref rt 10))
(define (set-record-type-data! rt d)      (record-set! rt 10 d))
(define (record-type-base rt i)
  (record-ref rt (+ i *first-extension-slot*)))


; This is a hack; it is read by the script that makes c/scheme48.h.

(define record-type-fields
  '(resumer uid name field-names number-of-fields discloser parent extension-count size data base))

;----------------

; Given a record type and the name of a field, return the field's index.

(define (record-field-index rt name)
  (let loop ((names (record-type-field-names rt))
	     (i 1))
    (cond ((null? names)
	   (assertion-violation 'record-field-index
				"unknown field"
				(record-type-name rt)
				name))
	  ((eq? name (car names))
	   (+ (record-type-parent-size rt) i))
	  (else
	   (loop (cdr names) (+ i 1))))))

(define (record-type-parent-size rt)
  (cond
   ((record-type-parent rt)
    => record-type-size)
   (else 0)))

; Return procedure for constructing records of type RT.  NAMES is a list of
; field names which the constructor will take as arguments.  Other fields are
; uninitialized.

; This is legacy code needed for linking, so don't change.  Works only
; for base types.

(define (record-constructor rt names)
  (if (record-type-parent rt)
      (assertion-violation 'record-constructor "works only for base types" rt names))
  (let ((indexes (map (lambda (name)
			(record-field-index rt name))
		      names))
	(size (+ 1 (record-type-size rt))))
    (lambda args
      (let ((r (make-record size (unspecific))))
	(record-set! r 0 rt)
	(let loop ((is indexes) (as args))
	  (cond
	   ((null? as)
	    (if (null? is)
		r
		(assertion-violation 'record-constructor
				     "too few arguments to record constructor"
				     rt names args)))
	   ((null? is)
	    (assertion-violation 'record-constructor
				 "too many arguments to record constructor"
				 rt names args))
	   (else
	    (record-set! r (car is) (car as))
	    (loop (cdr is) (cdr as)))))))))

; Return procedure for constructing records of type RT.  Takes as
; many arguments as there are fields.

(define (record-standard-constructor rt)
  (let ((size (+ 1 (record-type-size rt))))
    (lambda args
      (let ((r (make-record size (unspecific))))
	(record-set! r 0 rt)
	(let loop ((i 1) (as args))
	  (cond
	   ((null? as)
	    (if (= i size)
		r
		(assertion-violation 'record-constructor
				     "too few arguments to record constructor"
				     rt args)))
	   ((= i size)
	    (assertion-violation 'record-constructor
				 "too many arguments to record constructor"
				 rt args))
	   (else
	    (record-set! r i (car as))
	    (loop (+ 1 i) (cdr as)))))))))

; Making accessors, modifiers, and predicates for record types.

(define (record-accessor rt name)
  (let ((index (record-field-index rt name))
	(error-cruft `(record-accessor ,rt ',name)))
    (lambda (r)
      (if (record-type<=? (record-type r) rt)
	  (record-ref r index)
	  (assertion-violation 'record-accessor "invalid record access"
			       error-cruft r)))))

(define (record-modifier rt name)
  (let ((index (record-field-index rt name))
	(error-cruft `(record-modifier ,rt ',name)))
    (lambda (r x)
      (if (record-type<=? (record-type r) rt)
	  (record-set! r index x)
	  (assertion-violation 'record-modifier "invalid record modification"
			       error-cruft r x)))))

(define (record-predicate rt)
  (lambda (x)
    (and (record? x)
	 (record-type<=? (record-type x) rt))))

;----------------
; A discloser is a procedure that takes a record of a particular type and
; returns a list whose head is a string or symbol and whose tail is other
; stuff.
;
; Set the discloser for record type RT.

(define (define-record-discloser rt proc)
  (if (and (record-type? rt)
	   (procedure? proc))
      (set-record-type-discloser! rt proc)
      (assertion-violation 'define-record-discloser "invalid argument" rt proc)))

; By default we just return the name of the record type.

(define (make-default-record-discloser record-type-name)
  (lambda (r)
    (list record-type-name)))

; DISCLOSE-RECORD calls the record's discloser procedure to obtain a list.

(define (disclose-record r)
  (if (record? r)
      (let ((rt (record-type r)))
	(if (record-type? rt)
	    ((record-type-discloser rt) r)
	    #f))
      #f))

;----------------
; A resumer is a procedure that the VM calls on all records of a given
; type on startup.
;
; A resumer may be:
;  #t -> do nothing on startup.
;  #f -> records of this type do not survive a dump/resume; in images they
;        are replaced by their first slot (so we make sure they have one)
;  a one-argument procedure -> pass the record to this procedure
;
; Resumers are primarily intended for use by external code which keeps
; fields in records which do not survive a dump under their own power.
; For example, a record may contain a reference to a OS-dependent value.
;
; Resumers are called by the VM on startup.

(define (define-record-resumer rt resumer)
  (if (and (record-type? rt)
	   (or (eq? #t resumer)
	       (and (eq? #f resumer)
		    (< 0 (record-type-size rt)))
	       (procedure? resumer)))
      (set-record-type-resumer! rt resumer)
      (assertion-violation 'define-record-resumer "invalid argument" rt resumer)))

; By default we leave records alone.

(define default-record-resumer
  #t)

(define (initialize-records! resumer-records)
  (if (vector? resumer-records)
      (do ((i 0 (+ i 1)))
	  ((= i (vector-length resumer-records)))
	(resume-record (vector-ref resumer-records i)))))

(define (resume-record record)
  ((record-type-resumer (record-type record))
     record))

;----------------
; Initializing *RECORD-TYPE* and making a type.

(set! *record-type*
      (make-record-type 'record-type record-type-fields))

(record-set! *record-type* 0 *record-type*)

(define :record-type *record-type*)

(define-record-discloser :record-type
  (lambda (rt)
    (list 'record-type
	  (record-type-uid rt)
	  (record-type-name rt))))

