; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Ivan Shmakov

; Scheme analogues of Posix popen() and pclose() library calls.

; Create a pipe and exec COMMAND to talk to one end of the pipe while
; PROC is handed the other end.  If INPUT? is true PROC gets the input
; end of the pipe and COMMAND gets the output end.

(define (call-with-mumble-pipe input?)
  (lambda (command proc . may-be-port)
    (call-with-values open-pipe
      (lambda (input-pipe output-pipe)
	(let ((pid (fork)))
	  (if pid
	      ; Parent
	      (let ((proc-port (if input? input-pipe output-pipe)))
		(close-port (if input? output-pipe input-pipe))
		(call-with-values
		 (lambda ()
		   (proc proc-port))
		 (lambda vals
		   (close-port proc-port)
		   (wait-for-child-process pid)
		   (apply values vals))))
	      ; Child
	      (dynamic-wind
	       (lambda ()
		 #f)
	       (lambda ()
		 (define port
		   (and (not (null? may-be-port))
			(car may-be-port)))
		 (if input?
		     (remap-file-descriptors! (or
					       port
					       (current-input-port))
					      output-pipe
					      (current-error-port))
		     (remap-file-descriptors! input-pipe
					      (or
					       port
					       (current-output-port))
					      (current-error-port)))
		 (cond ((list? command)
			(apply exec command))
		       (else
			;; FIXME: consider using "$SHELL" here
			(exec-file "/bin/sh" "-c" command))))
	       (lambda ()
		 (exit 1)))))))))

(define (close-port port)
  (if (input-port? port)
      (close-input-port port)
      (close-output-port port)))

(define call-with-input-pipe
  (call-with-mumble-pipe #t))

(define call-with-output-pipe
  (call-with-mumble-pipe #f))
