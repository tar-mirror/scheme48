; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Marcus Crestani,
; Will Noble, Roderic Morris


; 3.1 Process Creation and Execution

(import-dynamic-externals "=scheme48external/posix")
;
; FORK returns the child pid in the parent, and #f in the child.

(define (fork)
  (force-channel-output-ports!)
  (let ((pid (posix-fork)))
    (and (> pid 0) (enter-pid pid))))

(import-lambda-definition-2 posix-fork ())

; Fork off a process to execute THUNK, but don't return a pid.  This avoids
; any problem with zombies.

(define (fork-and-forget thunk)
  (cond ((fork)
	 => wait-for-child-process)
	((fork)
	 (exit 0))
	(else
	 (thunk))))

;----------------
; The base Scheme procedure for exec() is EXEC-WITH-ALIAS (the `alias' is
; because the first argument may or may not be the name of the program or file).
;
; (EXEC-WITH-ALIAS program lookup? environment arguments)
;   program: byte vector, name of file or program
;   lookup?: should the program be looked up in PATH?
;   environment: either #f, which uses the parent's environment in the child,
;            or a list of byte vectors, representing text of the form "name=value".
;   arguments: a list of byte vectors

(import-lambda-definition-2 external-exec-with-alias (program lookup? environment arguments)
			  "posix_exec")

(define (thing->exec-arg-byte-string thing)
  (x->os-byte-vector thing))

(define (exec-with-alias program lookup? environment arguments)
  (external-exec-with-alias (thing->exec-arg-byte-string program)
			    lookup?
			    (and environment
				 (map thing->exec-arg-byte-string environment))
			    (map thing->exec-arg-byte-string arguments)))

; Four versions of exec():
;  - program looked up, use default environment
;  - program looked up, environment argument
;  - file used as-is, use default environment
;  - file used as-is, environment argument
;
; In all cases, the program or file is added to the beginning of the list
; of arguments.
;
; When given, ENV should be a list of strings of the form "name=value".

(define (exec program . arguments)
  (exec-with-alias program #t #f (cons program arguments)))

(define (exec-with-environment program env . arguments)
  (exec-with-alias program #t env (cons program arguments)))

(define (exec-file file . arguments)
  (exec-with-alias file #f #f (cons file arguments)))

(define (exec-file-with-environment file env . arguments)
  (exec-with-alias file #f env (cons file arguments)))

;----------------
; Process ids
;
; Threads can wait for child process to terminate.  This requires polling,
; which in turn requires that we store the child's return status or terminating
; signal for later use.  Hence the extra fields.
;
; These should probably not be dumpable.
;
; Because these have state they must be unique.  The C code keeps a weak
; list of existing ones.

(define-record-type process-id :process-id
  (make-process-id uid exit-status terminating-signal placeholder)
  process-id?
  (uid process-id->integer)		; the Unix PID
  ; The rest are initially #F and are set as events warrant.
  (exit-status process-id-exit-status set-process-id-exit-status!)
  (terminating-signal process-id-terminating-signal set-process-id-terminating-signal!)
  (placeholder process-id-placeholder))

(define-record-discloser :process-id
  (lambda (process-id)
    (list 'process-id (process-id->integer process-id))))

; Invalidate process IDs on resuming image.

(define-record-resumer :process-id #f)

(define *process-ids* (make-population))

(define-reinitializer process-id-reinitializer
  (lambda () (set! *process-ids* (make-population))))

(define (process-id=? p1 p2)
  (if (and (process-id? p1)
	   (process-id? p2))
      (eq? p1 p2)
      (assertion-violation 'process-id=? "argument type error" p1 p2)))

(define (enter-pid num)
  (let ((pid (make-process-id num #f #f (make-placeholder))))
    (add-to-population! pid *process-ids*)
    pid))

(define (lookup-pid num)
  (call-with-current-continuation
   (lambda (return)
     (walk-population
      (lambda (pid)
	(if (= num (process-id->integer pid)) (return pid)))
      *process-ids*)
     #f)))

(define (integer->process-id num) (or (lookup-pid num) (enter-pid num)))

;----------------
; 3.2 Process Termination
;
; pid_t wait(int *stat_loc)
; pid_t waitpid(pid_t pid, int *stat_loc, int options)
; void _exit(int status);         Need to do this.

; Wait for a child process.

(define (wait-for-child-process pid)
  (placeholder-value (process-id-placeholder pid) #f)
  (values))

; Waiting for children.  We go through the terminated child processes
; until we we run out.  This needs to be called by the SIGCHLD
; handler.

(define (process-terminated-children)
  (let loop ()
    (cond
     ((posix-waitpid)
      => (lambda (next)
	   (cond
	    ((lookup-pid (vector-ref next 0))
	     => (lambda (pid)
		  (set-process-id-exit-status! pid (vector-ref next 1))
		  (set-process-id-terminating-signal!
		   pid
		   (and (vector-ref next 2)
			(integer->signal (vector-ref next 2))))
		  (placeholder-set! (process-id-placeholder pid) #t))))
	   (loop))))))

(import-lambda-definition-2 posix-waitpid ())

(define (exit status)
  (force-channel-output-ports!)
  (scheme-exit-now status))

