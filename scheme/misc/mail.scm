; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees



; Mailboxes (to be used with the threads package)

(define (make-mailbox)
  (vector (make-lock) (make-queue) (make-queue)))

(define (mailbox-lock	  mbx) (vector-ref mbx 0))
(define (mailbox-messages mbx) (vector-ref mbx 1))
(define (mailbox-readers  mbx) (vector-ref mbx 2))

(define (mailbox-empty? mbx)
  (queue-empty? (mailbox-readers mbx)))

(define (mailbox-write mbx message)
  (with-lock (mailbox-lock mbx)
    (lambda ()
      (if (queue-empty? (mailbox-readers mbx))
	  (enqueue! (mailbox-messages mbx) message)
	  (condvar-set! (dequeue! (mailbox-readers mbx)) message)))))

(define (mailbox-read mbx)
  ((with-lock (mailbox-lock mbx)
     (lambda ()
       (if (queue-empty? (mailbox-messages mbx))
	   (let ((cv (make-condvar)))
	     (enqueue! (mailbox-readers mbx) cv)
	     ;; The condvar-ref *must* happen after lock is released,
	     ;; otherwise deadlock will result.
	     (lambda () (condvar-ref cv)))
	   (let ((message (dequeue! (mailbox-messages mbx))))
	     (lambda () message)))))))
