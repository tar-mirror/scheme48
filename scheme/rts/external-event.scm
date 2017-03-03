; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber, Robert Ransom

;----------------
; External events

(define (initialize-external-events!)
  (set-interrupt-handler! (enum interrupt external-event)
			  external-event-handler))

;----------------

; A session slot contains an alist mapping external-event uids to
; condvars for external events on that uid.  This works analogously to
; channels.

(define external-events-wait-condvars-slot
  (make-session-data-slot! '()))

(define (external-event-condvars)
  (session-data-ref external-events-wait-condvars-slot))

(define (set-external-event-condvars! condvars)
  (session-data-set! external-events-wait-condvars-slot condvars))

(define (add-external-event-condvar! uid condvar)
  (set-external-event-condvars! (cons (cons uid condvar)
				      (external-event-condvars))))

(define (notify-external-event-condvar! condvar)
  (with-new-proposal (lose)
    (or (maybe-commit-and-set-condvar! condvar #t)
	(lose))))

(define (external-event-handler uid enabled-interrupts)
  (cond
   ((fetch-external-event-condvar! uid)
    => notify-external-event-condvar!)))

; the condvar will be set when the event occurs
(define (register-condvar-for-external-event! uid condvar)
  (let ((ints (disable-interrupts!)))
    (add-external-event-condvar! uid condvar)
    (set-enabled-interrupts! ints)))

; make a new temporary event type and a condvar for it; return uid and condvar
(define (new-external-event)
  (let ((event-uid (new-external-event-uid #f))
	(condvar (make-condvar)))
    (register-condvar-for-external-event! event-uid condvar)
    (values event-uid condvar)))

; actually wait for the event
(define (wait-for-external-event condvar)
  (with-new-proposal (lose)
    (or (if (condvar-has-value? condvar)
	    (maybe-commit)
	    (maybe-commit-and-wait-for-condvar condvar #f))
	(lose))))

; This just deletes from the alist.

(define (fetch-external-event-condvar! uid)
  (let ((condvars (external-event-condvars)))
    (cond ((null? condvars)
	   #f)
	  ((= uid (caar condvars))
	   (set-external-event-condvars! (cdr condvars))
	   (cdar condvars))
	  (else
	   (let loop ((condvars (cdr condvars)) (prev condvars))
	     (cond ((null? condvars)
		    #f)
		   ((= uid (caar condvars))
		    (set-cdr! prev (cdr condvars))
		    (cdar condvars))
		   (else
		    (loop (cdr condvars) condvars))))))))

; Zap the condvars that no longer have waiters.  This assumes disabled
; interrupts.  The root scheduler typically calls this.

(define (zap-external-event-orphans!)
  (let loop ((condvars (external-event-condvars)) (okay '()))
    (if (null? condvars)
	(set-external-event-condvars! okay)
	(let ((condvar (cdar condvars)))
	  (loop (cdr condvars)
		(if (condvar-has-waiters? condvar)
		    (cons (car condvars) okay)
		    (begin
		      (notify-external-event-condvar! condvar)
		      okay)))))))
