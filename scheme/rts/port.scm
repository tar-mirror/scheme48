; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Robert Ransom

; Ports and port handlers

;  (discloser <port>) -> (<symbol> <value> ...)
;  (close <port>) -> whatever
;
; Input ports
;  (byte <port> <read?>) -> <byte>
;  (char <port> <mode>) -> <char>
;     <mode> says whether we're doing ...
;     - #t: a PEEK
;     - #f: a READ
;     - (): CHAR-READY? 
;  (block <port> <buffer> <start> <count>) -> <byte count>
;  (ready? <port>) -> <boolean>
;
; Output ports
;  (byte <port> <byte>) -> whatever
;  (char <port> <char>) -> whatever
;  (block <port> <buffer> <start> <count>) -> whatever
;  (ready? <port>) -> <boolean>
;  (force-output <port>) -> whatever

(define-record-type port-handler :port-handler
  (make-port-handler discloser close byte char block ready? force)
  port-handler?
  (discloser port-handler-discloser)
  (close     port-handler-close)
  (byte      port-handler-byte)
  (char	     port-handler-char)
  (block     port-handler-block)
  (ready?    port-handler-ready?)
  (force     port-handler-force))	; only used for output

;----------------
; Disclosing ports by calling the disclose handler.

(define (disclose-port port)
  ((port-handler-discloser (port-handler port))
     port))

(define-method &disclose ((port <input-port>))
  (disclose-port port))

(define-method &disclose ((port <output-port>))
  (disclose-port port))

;----------------
; Set up VM exception handlers for the three unnecessary I/O primitives,
; READ-BYTE, PEEK-BYTE, and WRITE-BYTE.  These do the right thing in
; the case of unbuffered ports or buffer overflow or underflow.
;
; This is abstracted to avoid a circular module dependency.

(define (initialize-i/o-handlers! define-vm-exception-handler signal-exception)
  (define-vm-exception-handler (enum op read-byte)
    (one-arg-proc->handler (lambda (port)
			     ((port-handler-byte (port-handler port))
			      port
			      #t))))
    
  (define-vm-exception-handler (enum op peek-byte)
    (one-arg-proc->handler (lambda (port)
			     ((port-handler-byte (port-handler port))
			      port
			      #f))))

  (define-vm-exception-handler (enum op read-char)
    (one-arg-proc->handler (lambda (port)
			     ((port-handler-char (port-handler port))
			      port
			      #f))))
    
  (define-vm-exception-handler (enum op peek-char)
    (one-arg-proc->handler (lambda (port)
			     ((port-handler-char (port-handler port))
			      port
			      #t))))
  
  (define-vm-exception-handler (enum op write-byte)
    (two-arg-proc->handler (lambda (byte port)
			     ((port-handler-byte (port-handler port))
			      port
			      byte))))

  (define-vm-exception-handler (enum op write-char)
    (two-arg-proc->handler (lambda (ch port)
			     ((port-handler-char (port-handler port))
			      port
			      ch)))))

; Check the VM exception and then lock the port.

(define (one-arg-proc->handler proc)
  (lambda (opcode reason port)
    (if (= reason (enum exception buffer-full/empty))
	(proc port)
	;; note this must be an assertion violation---these only look at the buffer
	(signal-vm-exception opcode reason port))))

; This could be combined with one-arg-... if the port were the first argument.

(define (two-arg-proc->handler proc)
  (lambda (opcode reason arg port)
    (if (= reason (enum exception buffer-full/empty))
	(proc arg port)
	;; note this must be an assertion violation---these only look at the buffer
	(signal-vm-exception opcode reason arg port))))

;----------------
; Wrappers for the various port operations.  These check types and arguments
; and then call the appropriate handler procedure.

(define (real-char-ready? port)
  (cond
   ((open-input-port? port)
    ((port-handler-char (port-handler port)) port '()))
   (else
    (assertion-violation 'char-ready? "invalid argument" port))))

; See if there is a character available.  BYTE-READY? itself is defined
; in current-port.scm as it needs CURRENT-INPUT-PORT when called with
; no arguments.

(define (real-byte-ready? port)
  (if (open-input-port? port)
      ((port-handler-ready? (port-handler port))
         port)
      (assertion-violation 'real-byte-ready? "invalid argument" port)))

; Reading in a block of characters at once.

(define (read-block buffer start count port . maybe-wait?)
  (if (and (port? port)
	   (open-input-port? port)
	   (okay-limits? buffer
			 start
			 count))
      (if (= count 0)
	  0
	 ((port-handler-block (port-handler port))
	    port
	    buffer
	    start
	    count
	    (or (null? maybe-wait?)
		(car maybe-wait?))))
      (assertion-violation 'read-block "invalid argument" buffer start count port)))

; Write the COUNT bytes beginning at START from BUFFER to PORT.

(define (write-block buffer start count port)
  (if (and (port? port)
	   (open-output-port? port)
	   (okay-limits? buffer start count))
      (if (< 0 count)
	  ((port-handler-block (port-handler port))
	     port
	     buffer
	     start
	     count))
      (assertion-violation 'write-block "invalid argument" buffer start count port)))

(define (write-string string port)
  (do ((size (string-length string))
       (i 0 (+ 1 i)))
      ((>= i size) (unspecific))
    (write-char (string-ref string i) port)))

; BYTE-READY? for output ports.

(define (output-port-ready? port)
  (if (open-output-port? port)
      ((port-handler-ready? (port-handler port))
         port)
      (assertion-violation 'output-port-ready? "invalid argument" port)))

; Forcing output.

(define (force-output port)
  (if (open-output-port? port)
      ((port-handler-force (port-handler port))
         port
	 #t)			; raise error if PORT is not open
      (assertion-violation 'force-output "invalid argument" port)))

(define (force-output-if-open port)
  (if (open-output-port? port)
      ((port-handler-force (port-handler port))
         port
	 #f)))			; do not raise error if PORT is not open

; Closing input and output ports.
; RnRS says that CLOSE-{IN|OUT}PUT-PORT is idempotent.

(define (close-input-port port)
  (if (input-port? port)
      (begin
	(if (open-input-port? port)
	    ((port-handler-close (port-handler port))
	       port))
	(unspecific))
      (assertion-violation 'close-input-port "invalid argument" port)))

(define (close-output-port port)
  (if (output-port? port)
      (begin
	(if (open-output-port? port)
	    ((port-handler-close (port-handler port))
	       port))
	(unspecific))
      (assertion-violation 'close-output-port "invalid argument" port)))

;----------------

(define (port-text-codec p)
  (spec->text-codec (port-text-codec-spec p)))

(define (set-port-text-codec! p codec)
  (set-port-text-codec-spec! p (text-codec->spec codec)))

;----------------
; Check that BUFFER contains COUNT characters starting from START.

(define (okay-limits? buffer start count)
  (and (integer? start)
       (exact? start)
       (<= 0 start)
       (integer? count)
       (exact? count)
       (<= 0 count)
       (<= (+ start count)
	   (cond ((byte-vector? buffer)
		  (byte-vector-length buffer))
		 (else
		  -1)))))

;----------------
; Is PORT open?

(define (open-port? port)
  (not (= 0 (bitwise-and open-port-mask (provisional-port-status port)))))

(define open-port-mask
  (bitwise-ior (arithmetic-shift 1 (enum port-status-options open-for-input))
	       (arithmetic-shift 1 (enum port-status-options open-for-output))))

;----------------
; Input ports

(define input-port-mask
  (arithmetic-shift 1
		    (enum port-status-options input)))

(define open-input-port-mask
  (arithmetic-shift 1
		    (enum port-status-options open-for-input)))

(define open-input-port-status
  (bitwise-ior input-port-mask
	       open-input-port-mask))

(define (open-input-port? port)
  (not (= 0 (bitwise-and open-input-port-mask
			 (provisional-port-status port)))))

(define (make-input-port-closed! port)
  (provisional-set-port-status! port
				(bitwise-and (provisional-port-status port)
					     (bitwise-not open-input-port-mask))))

;----------------
; Output ports

(define output-port-mask
  (arithmetic-shift 1
		    (enum port-status-options output)))

(define open-output-port-mask
  (arithmetic-shift 1
		    (enum port-status-options open-for-output)))

(define open-output-port-status
  (bitwise-ior output-port-mask
	       open-output-port-mask))

(define (open-output-port? port)
  (not (= 0 (bitwise-and open-output-port-mask
			 (provisional-port-status port)))))

(define (make-output-port-closed! port)
  (provisional-set-port-status! port
				(bitwise-and (provisional-port-status port)
					     (bitwise-not open-output-port-mask))))

(define (make-unbuffered-output-port handler data)
  (if (port-handler? handler)
      (make-port handler
		 (enum text-encoding-option latin-1)
		 #f
		 open-output-port-status
		 #f		; lock     (not used in unbuffered ports)
		 data
		 (make-byte-vector 128 0) ; buffer
		 #f		; index
		 #f		; limit
		 #f             ; pending-cr?
		 #f)            ; pending-eof?
      (assertion-violation 'make-unbuffered-output-port "invalid argument"
			   handler data)))

(define (make-one-byte-handler write-block)
  (lambda (port byte)
    (let ((buffer (port-buffer port)))
      (byte-vector-set! buffer 0 byte)
      (let loop ()
	(if (= 0 (write-block port buffer 0 1))
	    (loop))))))

(define (make-one-char-handler write-block)
  (lambda (port ch)
    (let ((buffer (port-buffer port))
	  (encode-char
	   (text-codec-encode-char-proc (port-text-codec port))))
      (let ((encode-count
	     (if (and (port-crlf? port)
		      (char=? ch #\newline))
		 (atomically
		  (call-with-values
		      (lambda ()
			(encode-char cr
				     buffer 0 (byte-vector-length buffer)))
		    (lambda (ok? encode-count-cr)
		      ;; OK? must be true
		      (call-with-values
			  (lambda ()
			    (encode-char #\newline
					 buffer 
					 encode-count-cr
					 (- (byte-vector-length buffer) encode-count-cr)))
			(lambda (ok? encode-count-lf)
			  ;; OK? must be true
			  (+ encode-count-cr encode-count-lf))))))
		 (atomically
		  (call-with-values
		      (lambda ()
			(encode-char ch
				     buffer 0 (byte-vector-length buffer)))
		    (lambda (ok? encode-count)
		      (if ok?
			  encode-count
			  ;; hrmpfl ...
			  (call-with-values
			      (lambda ()
				(encode-char #\?
					     buffer 0 (byte-vector-length buffer)))
			    (lambda (ok? encode-count)
			      encode-count)))))))))
	(let loop ((index 0))
	  (let* ((to-write (- encode-count index))
		 (written
		  (write-block port buffer index to-write)))
	    (if (< written to-write)
		(loop (+ index written)))))))))

(define cr (ascii->char 13))

(define (make-write-block-handler write-block)
  (lambda (port buffer start count)
    (let loop ((sent 0))
      (let ((sent (+ sent
		     (write-block port
				  buffer
				  (+ start sent)
				  (- count sent)))))
	(if (< sent count)
	    (loop sent))))))

(define (make-unbuffered-output-port-handler discloser closer! write-block ready?)
  (make-port-handler discloser
		     closer!
		     (make-one-byte-handler write-block)
		     (make-one-char-handler write-block)
		     (make-write-block-handler write-block)
		     ready?
		     (lambda (port error-if-closed?)	; output forcer
		       (unspecific))))

;----------------
; Output ports that just discard any output.

(define null-output-port-handler
  (make-port-handler
    (lambda (ignore)			; disclose
      (list 'null-output-port))
    make-output-port-closed!		; close
    (lambda (port byte)			; one-byte (we just empty the buffer)
      (set-port-index! port 0))
    (lambda (port char)                 ; one-char (we just empty the buffer)
      (set-port-index! port 0))
    (lambda (port buffer start count)	; write-block
      count)
    (lambda (port)			; ready?
      #t)
    (lambda (port error-if-closed?)	; force-output
      (unspecific))))

; They can all share a buffer.  The buffer is needed because the WRITE-BYTE
; byte code actually wants to put characters somewhere.

(define null-output-buffer
  (make-byte-vector 1024 0))

(define (make-null-output-port)
  (make-port null-output-port-handler
	     null-text-codec
	     #f
	     open-output-port-status
	     #f		; timestamp
	     #f		; data
	     null-output-buffer
	     0		; index
	     (byte-vector-length null-output-buffer)	; limit
	     #f         ; pending-cr?
	     #f))	; pending-eof?
