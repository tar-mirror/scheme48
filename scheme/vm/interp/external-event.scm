; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber

; External events from C code

; The external events are organized into types: We only record *that*
; an external event has happened, not how many times, or any other
; associated information.

; We need to distinguish between signalling an external event from C
; code to the VM, and shuffling that to Scheme.

; Every type of external event gets a unique uid.  We use the shared
; bindings to preserve them in the image.

(define *number-of-event-types* 100)

; vector of all event types
(define *event-types*)

(define-record-type event-type :event-type
  (make-event-type uid used? next)
  (uid    integer event-type-uid)
  (used?  boolean event-type-used? set-event-type-used?!)
  ;; the pending external events form a queue, just like the channels
  (next   event-type event-type-next set-event-type-next!))

; The *pending* event types form a linked list of all event types that
; haven't been shuffled to Scheme.

(define *pending-event-types-head*)
(define *pending-event-types-tail*)

; Some tail of the list of pending event types hasn't even been
; shuffled to the VM yet; those are the *ready* types.

(define *pending-event-types-ready*)

; Unused types form a list, also linked by next
(define *unused-event-types-head*) 

(define (initialize-external-events)
  (set! *event-types* (make-vector *number-of-event-types* (null-pointer)))
  (if (null-pointer? *event-types*)
      (error "out of memory, unable to continue"))
  (let ((event-types-count *number-of-event-types*))
    (set! *number-of-event-types* 0)
    (set! *unused-event-types-head* (null-pointer))
    (if (not (add-external-event-types event-types-count))
	(error "out of memory, unable to continue")))
  (set! *pending-event-types-head* (null-pointer))
  (set! *pending-event-types-tail* (null-pointer))
  (set! *pending-event-types-ready* (null-pointer)))

; increase the number of external event types
(define (add-external-event-types min-count)
  (let ((old-event-types *event-types*)
	(old-count *number-of-event-types*)
	(new-event-types (make-vector min-count (null-pointer))))
    (if (null-pointer? new-event-types)
	#f
	(let loop ((i 0))
	  (cond
	   ((= i min-count)
	    (set! *event-types* new-event-types)
	    (set! *number-of-event-types* min-count)
	    (deallocate old-event-types)
	    #t)
	   ((< i old-count)
	    (vector-set! new-event-types i
			 (vector-ref old-event-types i))
	    (goto loop (+ 1 i)))
	   (else
	    (let ((t (make-event-type i #f *unused-event-types-head*)))
	      (if (null-pointer? t)
		  (begin
		    (set! *event-types* new-event-types)
		    (set! *number-of-event-types* i)
		    #f)
		  (begin
		    (vector-set! new-event-types i t)
		    (set! *unused-event-types-head* t)
		    (goto loop (+ 1 i)))))))))))

; mark an event type as used
(define (use-event-type-uid! id)
  (let ((type (vector-ref *event-types* id)))
    (if (event-type-used? type)
	(begin
	  (write-error-string "trying to use an event uid that's already in use : ")
	  (write-error-integer id)
	  (write-error-newline)
	  (error "assertion violation")))

    (set-event-type-used?! type #t)
    ;; delete from linked list
    (let loop ((previous (null-pointer))
	       (unused-type *unused-event-types-head*))	; usually, it should be the first
      (cond 
       ((null-pointer? unused-type)
	(unspecific))
       ((not (eq? type unused-type))
	(loop unused-type (event-type-next unused-type)))
       ((null-pointer? previous)
	(set! *unused-event-types-head* (event-type-next unused-type)))
       (else
	(set-event-type-next! previous (event-type-next unused-type)))))

    (set-event-type-next! type (null-pointer))))

; mark an event type as unused
(define (mark-event-type-uid-unused! uid)
  (let ((type (vector-ref *event-types* uid)))
    (cond
     ((not (null-pointer? (event-type-next type)))
      (write-error-string "trying to unregister external event that is still in use : ")
      (write-error-integer uid)
      (write-error-newline)
      (error "assertion violation"))
     (else
      (set-event-type-next! type *unused-event-types-head*)
      (set-event-type-used?! type #f)
      (set! *unused-event-types-head* type)))))

; return an unused event-type uid
; returns -1 on out-of-memory
(define (unused-event-type-uid)
  (cond
   ((not (null-pointer? *unused-event-types-head*))
    (event-type-uid *unused-event-types-head*))
   ((add-external-event-types (* 2 *number-of-event-types*))
    (unused-event-type-uid))
   (else -1)))

; return an unused event-type uid; for temporary use
(define (external-event-uid)
  (let ((uid (unused-event-type-uid)))
    (if (= -1 uid)
	uid
	(begin
	  (use-event-type-uid! uid)
	  uid))))
  
; return an unused event-type uid; for permanent use
(define (permanent-external-event-uid binding)
  (let* ((uid-val (shared-binding-ref binding)))

    (define (indeed uid)
      (shared-binding-set! binding (enter-fixnum uid))
      (use-event-type-uid! uid)
      uid)

    (if (fixnum? uid-val)
	(begin
	  (let ((uid (extract-fixnum uid-val)))
	    (cond
	     ((< uid *number-of-event-types*)
	      (indeed uid))
	     ((add-external-event-types (+ 1 uid))
	      (indeed uid))
	     (else -1)))) ; out of memory
	(let ((uid (unused-event-type-uid)))
	  (if (= -1 uid)
	      uid
	      (indeed uid))))))

; unregister an external-event type registered via `s48-external-event-uid'
(define (unregister-external-event-uid! index)
 
  (define (lose/invalid)
    (write-error-string "trying to unregister invalid external event: ")
    (write-error-integer index)
    (write-error-newline)
    (error "assertion violation")) 

  (if (>= index *number-of-event-types*)
      (lose/invalid))

  (let ((type (vector-ref *event-types* index)))
    (cond
     ((not (event-type-used? type))
      (lose/invalid))
     ((not (null-pointer? (event-type-next type)))
      (write-error-string "trying to unregister external event that is still in use : ")
      (write-error-integer index)
      (write-error-newline)
      (error "assertion violation"))
     (else
      (mark-event-type-uid-unused! index)))))

; Pending events

; This is intended to be called by the C code, but will generally
; need some sort of mutex protection there.

(define (s48-external-event-ready?/unsafe)
  (not (null-pointer? *pending-event-types-ready*)))

; removes the event type from pending
(define (s48-external-event-pending?/unsafe)
  (if (s48-external-event-ready?/unsafe)
      (begin
	(set! *pending-event-types-ready* (event-type-next *pending-event-types-ready*))
	#t)
      #f))

; signal an external event
(define (s48-note-external-event!/unsafe index)

  (define (lose)
    (write-error-string "invalid external event: ")
    (write-error-integer index)
    (write-error-newline)
    (error "assertion-violation"))

  (if (>= index *number-of-event-types*)
      (lose))

  (let ((type (vector-ref *event-types* index)))
    (cond
     ((not (event-type-used? type)) (lose))
     ((or (not (null-pointer? (event-type-next type))) ; already queued
	  (eq? type *pending-event-types-head*)  ; first and only
	  (eq? type *pending-event-types-tail*)) ; last
      (unspecific))
     ((null-pointer? *pending-event-types-head*)
      (set! *pending-event-types-head* type)
      (set! *pending-event-types-tail* type)
      (set! *pending-event-types-ready* type))
     (else
      (set-event-type-next! *pending-event-types-tail* type)
      (set! *pending-event-types-tail* type)
      (if (null-pointer? *pending-event-types-ready*)
	  (set! *pending-event-types-ready* type))))))

; returns a uid and a boolean indicating whether more events are
; pending afterwards
(define (s48-dequeue-external-event!/unsafe)
  (let* ((type *pending-event-types-head*)
	 (next (event-type-next type)))
    (set! *pending-event-types-head* next)
    (set-event-type-next! type (null-pointer))
    (if (null-pointer? next)
	(set! *pending-event-types-tail* (null-pointer)))
    (values (event-type-uid type)
	    (s48-external-event-ready?/unsafe))))
