; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber
			       
(define (make-buffered-input-port handler data buffer index limit)
  (if (and (okay-buffer? buffer index limit)
	   (port-handler? handler))
      (make-port handler
		 (enum text-encoding-option latin-1)
		 #f
		 (bitwise-ior input-port-mask open-input-port-mask)
		 #f		; timestamp (was lock)
		 data
		 buffer
		 index
		 limit
		 #f             ; pending-cr?
		 #f)            ; pending-eof?
      (assertion-violation 'make-buffered-input-port
			   "invalid argument"
			   handler data buffer index limit)))

(define (make-buffered-output-port handler data buffer index limit)
  (if (and (okay-buffer? buffer index limit)
	   (> limit 0)
	   (port-handler? handler))
      (make-port handler
		 (enum text-encoding-option latin-1)
		 #f
		 open-output-port-status
		 #f		; timestamp (was lock)
		 data
		 buffer
		 index
		 limit
		 #f             ; pending-cr?
		 #f)            ; pending-eof?
      (assertion-violation 'make-buffered-output-port
			   "invalid argument"
			   handler data buffer index limit)))

(define (okay-buffer? buffer index limit)
  (and (byte-vector? buffer)
       (integer? limit)
       (integer? index)
       (exact? limit)
       (exact? index)
       (<= 0 limit (byte-vector-length buffer))
       (<= 0 index limit)))

;----------------
; (buffered-input-port-handler discloser
;                              closer
;                              fill-buffer-proc)  -> handler
;
; (fill-buffer-proc <port> <wait?>)
;    -> <boolean>   ; true if commit works, false if it fails
; Closer must also do a maybe-commit and return the result.
;
; If <wait?> is true then wait for input.  If <wait?> is false then return
; immediately even if no input is available.

(define (make-buffered-input-port-handler discloser
					  closer!
					  buffer-filler!
					  ready?)
  (make-port-handler (lambda (port)
		       (discloser (port-data port)))
		     (lambda (port)
		       (with-new-proposal (lose)
			 (make-input-port-closed! port)
			 (or (closer! (port-data port))
			     (lose))))
		     (make-one-byte-input buffer-filler!)
		     (make-one-char-input buffer-filler!)
		     (make-read-block buffer-filler!)
		     (make-byte-ready? ready? #t)
		     #f))			; force

;----------------
; Rename an old field

(define (note-buffer-reuse! port)
  (provisional-set-port-lock! port (cons 'timestamp '())))

; Calling this has the side-effect of getting the current proposal to check
; the timestamp value when committing.

(define check-buffer-timestamp! provisional-port-lock)

; And a current field.

(define port-flushed port-pending-eof?)
(define set-port-flushed! set-port-pending-eof?!)

;----------------

; The READ? argument says whether we're doing a READ or a PEEK.

(define (make-one-byte-input buffer-filler!)
  (lambda (port read?)
    (with-new-proposal (lose)
      (let ((index (provisional-port-index port))
	    (limit (provisional-port-limit port)))
	(cond ((not (open-input-port? port))
	       (remove-current-proposal!)
	       (assertion-violation (if read? 'read-byte 'peek-byte)
				    "invalid argument"
				    port))
	      ((< index limit)
	       (if read?
		   (provisional-set-port-index! port (+ 1 index)))
	       (let ((b (provisional-byte-vector-ref (port-buffer port)
						     index)))
		 (if (maybe-commit)
		     b
		     (lose))))
	      ((provisional-port-pending-eof? port)
	       (if read?
		   (provisional-set-port-pending-eof?! port #f))
	       (if (maybe-commit)
		   (eof-object)
		   (lose)))
	      (else
	       (provisional-set-port-index! port 0)
	       (provisional-set-port-limit! port 0)
	       (buffer-filler! port #t)
	       (lose)))))))

; The MODE argument says whether we're doing a READ (#f) , a PEEK (#t),
; or a CHAR-READY? ( () )

(define (make-one-char-input buffer-filler!)
  (lambda (port mode)
    (let ((decode
	   (text-codec-decode-char-proc (port-text-codec port))))
      (with-new-proposal (lose)

	(let ((limit (provisional-port-limit port)))
	  (let loop ((index (provisional-port-index port)))
	  
	    (define (consume&deliver decode-count val)
	      (if (not mode)
		  (provisional-set-port-index! port
					       (+ index decode-count)))
	      (if (maybe-commit)
		  val
		  (lose)))

	    (cond ((not (open-input-port? port))
		   (remove-current-proposal!)
		   (assertion-violation (cond
					 ((not mode) 'read-char)
					 ((null? mode) 'char-ready?)
					 (else 'peek-char))
					"invalid argument"
					port))
		  ((< index limit)
		   (let ((buffer (port-buffer port)))
		     (call-with-values
			 (lambda ()
			   (decode buffer index (- limit index)))
		       (lambda (ch decode-count)
			 (cond
			  (ch
			    ;; CR/LF handling. Great.
			   (cond
			    ((port-crlf? port)
			     (cond
			      ((char=? ch cr)
			       (provisional-set-port-pending-cr?! port #t)
			       (consume&deliver decode-count
						(if (null? mode) ; CHAR-READY?
						    #t
						    #\newline)))
			      ((and (char=? ch #\newline)
				    (provisional-port-pending-cr? port))
			       (provisional-set-port-pending-cr?! port #f)
			       (loop (+ index decode-count)))
			      (else
			       (provisional-set-port-pending-cr?! port #f)
			       (consume&deliver decode-count
						(if (null? mode) ; CHAR-READY?
						    #t
						    ch)))))
			    (else
			     (provisional-set-port-pending-cr?! port #f)
			     (consume&deliver decode-count
					      (if (null? mode) ; CHAR-READY?
						  #t
						  ch)))))
			     
			  ((or (not decode-count) ; decoding error
			       (provisional-port-pending-eof? port)) ; partial char
			   (consume&deliver 1
					    (if (null? mode)
						#t
						#\?)))
			  ;; need at least DECODE-COUNT bytes
			  (else
			   (if (> decode-count
				  (- (byte-vector-length buffer)
				     limit))
			      
			       ;; copy what we have to the
			       ;; beginning so there's space at the
			       ;; end we can try to fill
			       (begin
				 ;; (debug-message "aligning port buffer")
				 (attempt-copy-bytes! buffer index
						      buffer 0
						      (- limit index))
				 (provisional-set-port-index! port 0)
				 (provisional-set-port-limit! port (- limit index))))
			   (if (or (not (buffer-filler! port (not (null? mode))))
				   (not (null? mode)))
			       (lose)
			       #f)))))))
		  ((provisional-port-pending-eof? port)
		   (if (not mode)
		       (provisional-set-port-pending-eof?! port #f))
		   (cond
		    ((not (maybe-commit))
		     (lose))
		    ((null? mode) #t)
		    (else (eof-object))))
		  (else
		   (if (= index limit)	; we have zilch
		       (begin
			 (provisional-set-port-index! port 0)
			 (provisional-set-port-limit! port 0))
		       ;; may be out of synch because of CR/LF conversion
		       (provisional-set-port-index! port index))
		   (if (or (not (buffer-filler! port (not (null? mode))))
			   (not (null? mode)))
		       (lose)
		       #f)))))))))

;----------------
; See if there is a byte available.

(define (make-byte-ready? ready? read?)
  (lambda (port)
    (with-new-proposal (lose)
      (cond ((not ((if read?
		       open-input-port?
		       open-output-port?)
		   port))
	     (remove-current-proposal!)
	     (assertion-violation 'byte-ready? "invalid argument" port))
	    ((or (< (provisional-port-index port)
		    (provisional-port-limit port))
		 (and read?
		      (provisional-port-pending-eof? port)))
	     (if (maybe-commit)
		 #t
		 (lose)))
	    (else
	     (call-with-values
	      (lambda ()
		(ready? port))
	      (lambda (okay? ready?)
		(if okay?
		    ready?
		    (lose)))))))))

;----------------
; Block input
;
; If EOF-OKAY? is true the caller will pass an EOF back to the user.  If it's
; false then the caller already has a value to pass back and we have to preserve
; an EOF for the next invocation.

(define (make-read-block buffer-filler!)
  (lambda (port buffer start count wait?)
    (let loop ((have 0) (first? #t))
      (with-new-proposal (lose)
	(if (open-input-port? port)
	    (let ((result (cond ((provisional-port-pending-eof? port)
				 (if (= have 0)
				     (provisional-set-port-pending-eof?! port #f))
				 (eof-object))
				((= count 0)
				 0)
				(else
				 (get-available-bytes! buffer
						       (+ start have)
						       (- count have)
						       port)))))
	      (cond ((not result)
		     (if (or wait? first?)
			 (if (buffer-filler! port wait?)
			     (loop have #f)
			     (lose))
			 (if (maybe-commit)
			     0
			     (lose))))
		    ((not (maybe-commit))
		     (lose))
		    ((eof-object? result)
		     (if (= have 0)
			 result
			 have))
		    (else
		     (let ((have (+ have result)))
		       (if (< have count)
			   (loop have #f)
			   have)))))
	    (begin
	      (remove-current-proposal!)
	      (assertion-violation 'read-block "invalid argument"
				   port buffer start count)))))))

; Copy whatever bytes are currently available.
;
; Reading the timestamp makes its value part of the current proposal.  The
; timestamp is set whenever the buffer is refilled.  Without it the proposal
; could be fooled if the buffer were refilled and the index and limit just
; happened to be reset to their current values.

(define (get-available-bytes! buffer start count port)      
  (let* ((index (provisional-port-index port))
	 (have (- (provisional-port-limit port)
		  index)))
    (if (< 0 have)
	(let ((copy-count (min have count)))
	  (check-buffer-timestamp! port)	; makes the proposal check this
	  (attempt-copy-bytes! (port-buffer port)
			       index
			       buffer
			       start
			       copy-count)
	  (provisional-set-port-index! port
				       (+ index copy-count))
	  copy-count)
	(begin
	  (provisional-set-port-index! port 0)
	  (provisional-set-port-limit! port 0)
	  #f))))

;----------------------------------------------------------------
; Buffered output ports
;
; (buffered-output-port-handler discloser
;                               closer
;                               empty-buffer-proc) -> handler
;
; (empty-buffer-proc <port>) -> whatever
;
; The buffer emptier must call maybe-commit.

(define (make-buffered-output-port-handler discloser
					   closer!
					   buffer-emptier!
					   ready?)
  (make-port-handler (lambda (port)
		       (discloser (port-data port)))
		     (make-closer closer! buffer-emptier!)
		     (make-one-byte-output buffer-emptier!)
		     (make-one-char-output buffer-emptier!)
		     (make-write-block buffer-emptier!)
		     (make-byte-ready? ready? #f)
		     (make-forcer buffer-emptier!)))

(define (make-closer closer! buffer-emptier!)
  (lambda (port)
    (with-new-proposal (lose)
      (let ((index (provisional-port-index port)))
	(cond ((not (open-output-port? port))
	       (remove-current-proposal!)
	       (unspecific))
	      ((< 0 index)
	       (buffer-emptier! port #t)
	       (lose))
	      (else
	       (make-output-port-closed! port)
	       (or (closer! (port-data port))
		   (lose))))))))

; First check that PORT is open and then either put BYTE in PORT's buffer or
; empty the buffer and try again.

(define (make-one-byte-output buffer-emptier!)
  (lambda (port byte)
    (with-new-proposal (lose)
      (let ((index (provisional-port-index port))
	    (limit (byte-vector-length (port-buffer port))))
	(cond ((not (open-output-port? port))
	       (remove-current-proposal!)
	       (assertion-violation 'write-byte "invalid argument" port))
	      ((< index limit)
	       (provisional-byte-vector-set! (port-buffer port)
					     index
					     byte)
	       (provisional-set-port-index! port (+ 1 index))
	       (or (maybe-commit)
		   (lose)))
	      (else
	       (call-to-flush! port (lambda () (buffer-emptier! port #t)))
	       (lose)))))))

(define (make-one-char-output buffer-emptier!)
  (lambda (port ch)
    (let ((encode
	   (text-codec-encode-char-proc (port-text-codec port))))
      (with-new-proposal (lose)
	(let ((index (provisional-port-index port))
	      (limit (byte-vector-length (port-buffer port))))
	  (cond ((not (open-output-port? port))
		 (remove-current-proposal!)
		 (assertion-violation 'write-byte "invalid argument" port))
		((< index limit)
		 (let ((encode-count #f)
		       (ok? #f))
		   (cond
		    ((not
		      (maybe-commit-no-interrupts
		       (lambda ()
			 (if (and (port-crlf? port)
				  (char=? ch #\newline))
			     ;; CR/LF handling ruins our day once again
			     (call-with-values
				 (lambda ()
				   (encode cr
					   (port-buffer port)
					   index (- limit index)))
			       (lambda (the-ok? cr-encode-count)
				 (cond
				  ((or (not the-ok?)
				       (>= (+ index cr-encode-count) limit))
				   (set! ok? #f)
			   (set! encode-count (+ 1 cr-encode-count))) ; LF will take at least one
				  (else
				   (call-with-values
				       (lambda ()
					 (encode #\newline
						 (port-buffer port)
						 (+ index cr-encode-count)
						 (- limit (+ index cr-encode-count))))
				     (lambda (the-ok? lf-encode-count)
				       (set! ok? the-ok?)
				       (if the-ok?
					   (set-port-index! port
							    (+ index
							       cr-encode-count lf-encode-count))
					   (set! encode-count (+ cr-encode-count lf-encode-count)))))))))
			     (call-with-values
				 (lambda ()
				   (encode ch
					   (port-buffer port)
					   index (- limit index)))
			       (lambda (the-ok? the-encode-count)
				 (set! ok? the-ok?)
				 (if the-ok?
				     (set-port-index! port (+ index the-encode-count))
				     (set! encode-count the-encode-count))))))))
		     (lose))
		    (ok?)		; we're done
		    (encode-count	; need more space
		     (with-new-proposal (_)
		       (call-to-flush! port (lambda () (buffer-emptier! port #t))))
		     (lose))
		    (else		; encoding error
		     (set! ch #\?)    ; if we get an encoding error on
					; the second go, we're toast
		     (lose)))))
		(else
		 (call-to-flush! port (lambda () (buffer-emptier! port #t)))
		 (lose))))))))

; We have the following possibilities:
;  - the port is no longer open
;       -> raise an error
;  - there is nothing to write
;       -> do nothing
;  - there is room left in the port's buffer
;       -> copy bytes into it
;  - there is no room left in the port's buffer
;       -> write it out and try again

(define (make-write-block buffer-emptier!)
  (lambda (port buffer start count)
    (let loop ((sent 0))
      (with-new-proposal (lose)
	(cond ((not (open-output-port? port))
	       (remove-current-proposal!)
	       (assertion-violation 'write-block "invalid argument"
				    buffer start count port))
	      ((= count 0)
	       (if (maybe-commit)
		   0
		   (lose)))
	      ((copy-bytes-out! buffer
				(+ start sent)
				(- count sent)
				port)
	       => (lambda (more)
		    (if (maybe-commit)
			(let ((sent (+ sent more)))
			  (if (< sent count)
			      (loop sent)))
			(lose))))
	      (else
	       (call-to-flush! port (lambda () (buffer-emptier! port #t)))
	       (lose)))))))

(define (copy-bytes-out! buffer start count port)
  (let ((index (provisional-port-index port))
	(limit (byte-vector-length (port-buffer port))))
    (if (< index limit)
	(let ((copy-count (min (- limit index)
			       count)))
	  (check-buffer-timestamp! port)	; makes the proposal check this
	  (provisional-set-port-index! port (+ index copy-count))
	  (attempt-copy-bytes! buffer start
			       (port-buffer port) index
			       copy-count)
	  copy-count)
	#f)))

; Write out anything in the buffer.  When called by the auto-forcing code
; this may run across the occasional closed port.
;
; This loops by calling LOSE if the buffer-emptier's commit fails (in which
; case the emptier returns false) or if we are trying to empty the entire
; buffer (indicated by NECESSARY? being true).

(define (make-forcer buffer-emptier!)
  (lambda (port necessary?)
    (with-new-proposal (lose)
      (cond ((not (open-output-port? port))
	     (if necessary?
		 (begin
		   (remove-current-proposal!)
		   (assertion-violation 'force-output "invalid argument" port)))
	     (unspecific))
	    ((< 0 (provisional-port-index port))
	     (if (or (not (call-to-flush port (lambda () (buffer-emptier! port necessary?))))
		     necessary?)
		 (lose)))))))


;----------------

(define (default-buffer-size)
  (channel-parameter (enum channel-parameter-option buffer-size)))

;----------------
; Code to periodically flush output ports.

(define flush-these-ports
  (make-session-data-slot! (list #f)))

(define (periodically-force-output! port)
  (let ((pair (session-data-ref flush-these-ports)))
    (set-cdr! pair
	      (cons (make-weak-pointer port)
		    (cdr pair)))))

; Return a list of thunks that will flush the buffer of each open port
; that contains bytes that have been there since the last time
; this was called.  The actual i/o is done using separate threads to
; keep i/o errors from killing anything vital.
; 
; If USE-FLUSHED-FLAGS? is true this won't flush buffers that have been
; flushed by someone else since the last call.  If it is false then flush
; all non-empty buffers, because the system has nothing to do and is going
; to pause while waiting for external events.

(define (output-port-forcers use-flushed-flags?)
  (let ((pair (session-data-ref flush-these-ports)))
    (let loop ((next (cdr pair))
	       (last pair)
	       (thunks '()))
      (if (null? next)
;	  (begin   (debug-message "[forcing "
;				  (length thunks)
;				  " thunk(s)]")
	  thunks ;)
	  (let ((port (weak-pointer-ref (car next))))
	    (cond ((or (not port)	; GCed or closed so
		       (not (open-output-port? port))) ; drop it from the list
		   (set-cdr! last (cdr next))
		   (loop (cdr next) last thunks))
		  ((eq? (port-flushed port) 'flushing) ; somebody else is doing it
		   (loop (cdr next) next thunks)) 
		  ((and use-flushed-flags? ; flushed recently
			(port-flushed port))
		   (set-port-flushed! port #f)	; race condition, but harmless
		   (loop (cdr next) next thunks))
		  ((< 0 (port-index port)) ; non-empty
		   (loop (cdr next) next
			 (cons (make-forcing-thunk port)
			       thunks)))
		  (else			; empty
		   (loop (cdr next) next thunks))))))))

; Returns a list of the current ports that are flushed whenever.
; This is used to flush channel ports before forking.

(define (periodically-flushed-ports)
  (let* ((ints (set-enabled-interrupts! 0))
	 (pair (session-data-ref flush-these-ports)))
    (let loop ((next (cdr pair))
	       (last pair)
	       (ports '()))
      (if (null? next)
	  (begin
	    (set-enabled-interrupts! ints)
	    ports)
	  (let ((port (weak-pointer-ref (car next))))
	    (cond ((or (not port)                      ; GCed or closed
		       (not (open-output-port? port))) ; so drop it from the list
		   (set-cdr! last (cdr next))
		   (loop (cdr next) last ports))
		  (else
		   (loop (cdr next)
			 next
			 (cons port ports)))))))))

; Write out PORT's buffer.  If a problem occurs it is reported and PORT
; is closed.

(define (make-forcing-thunk port)
  (lambda ()
;    (debug-message "[forcing port]")
    (if (and (report-errors-as-warnings
	       (lambda ()
		 (force-output-if-open port))
	       "error when flushing buffer; closing port"
	       port)
	     (open-output-port? port))
	(report-errors-as-warnings
	  (lambda ()
	    (atomically! (set-port-index! port 0))	; prevent flushing
	    ((port-handler-close (port-handler port))
	     port))
	  "error when closing port"
	  port))))

(define (call-to-flush! port thunk)
  (set-port-flushed! port 'flushing) ; don't let the periodic flusher go crazy
  (thunk)
  (set-port-flushed! port #t))

(define (call-to-flush port thunk)
  (set-port-flushed! port 'flushing) ; don't let the periodic flusher go crazy
  (let ((retval (thunk))) ; one is enough
    (set-port-flushed! port #t)
    retval))
