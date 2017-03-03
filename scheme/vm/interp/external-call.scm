; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Marcus Crestani

; Calling C procedures.

; The arguments have been pushed on the stack after the procedure.
; *stack* = procedure name arg1 ... argN rest-list N+1 total-nargs
;
; The procedure must be an external binding whose value is a pointer-sized
; code-vector.  If it is, we actually do the call.
;
; The REMOVE-CURRENT-FRAME call pops all of our values off of the stack.
; In fact we still use them there for a moment (see s48_external_call() in
; external.c) but all of the values are fetched from the stack before
; anything new is pushed on.

(define-primitive call-external-value ()
  (lambda ()
    (let* ((nargs (extract-fixnum (pop)))
	   (stack-nargs (extract-fixnum (pop)))
	   (rest-list (pop)))
      (if (< maximum-external-call-args
	     (- nargs 2))			; procedure & name
	  (raise-exception too-many-arguments-to-external-procedure
			   0
			   (stack-ref (- stack-nargs 1))
			   nargs)
	  (begin
	    (do ((rest-list rest-list (vm-cdr rest-list)))
		((vm-eq? rest-list null))
	      (push (vm-car rest-list)))
	    (let ((proc (stack-ref (- nargs 1)))
		  (name (stack-ref (- nargs 2)))
		  (args (pointer-to-stack-arguments)))
	      (if (and (vm-string? name)
		       (code-vector? proc)
		       (= (code-vector-length proc)
			  (cells->bytes 1)))
		  (begin
		    (remove-current-frame)
		    (let ((result (external-call proc name (- nargs 2) args)))
		      (cond (*external-exception?*
			     (set! *external-exception?* #f)
			     (goto raise *external-exception-nargs*))
			    (else
			     (goto continue-with-value result 0)))))
		  (raise-exception wrong-type-argument 0 proc name))))))))

(define-primitive call-external-value-2 ()
  (lambda ()
    (let* ((nargs (extract-fixnum (pop)))
	   (stack-nargs (extract-fixnum (pop)))
	   (rest-list (pop)))
      (if (< maximum-external-call-args
	     (- nargs 2))			; procedure & name
	  (raise-exception too-many-arguments-to-external-procedure
			   0
			   (stack-ref (- stack-nargs 1))
			   nargs)
	  (begin
	    (do ((rest-list rest-list (vm-cdr rest-list)))
		((vm-eq? rest-list null))
	      (push (vm-car rest-list)))
	    (let ((proc (stack-ref (- nargs 1)))
		  (name (stack-ref (- nargs 2)))
		  (args (pointer-to-stack-arguments)))
	      (if (and (vm-string? name)
		       (code-vector? proc)
		       (= (code-vector-length proc)
			  (cells->bytes 1)))
		  (begin
		    (remove-current-frame)
		    (let ((result (external-call-2 proc name (- nargs 2) args)))
		      (cond (*external-exception?*
			     (set! *external-exception?* #f)
			     (goto raise *external-exception-nargs*))
			    (else
			     (goto continue-with-value result 0)))))
		  (raise-exception wrong-type-argument 0 proc name))))))))

;----------------
; Raising exceptions from C.

; True if the C procedure is raising an exception instead of doing a normal
; return.

(define *external-exception?* #f)

; The number of arguments being passed to the exception handler.

(define *external-exception-nargs*)

; These are for exceptions raised by external code.  They work pretty
; much in the same way as other VM instructions raise exceptions.
; Note that this doesn't actually perform the raise; it just sets
; *EXTERNAL-EXCEPTION?* to be true, so that the EXTERNAL-CALL procedure
; will do the raise.
;
; If you extend the maximum number of arguments, be sure to adjust the
; definition of STACK-SLACK in stack.scm.  It needs to know the maximum
; number of values pushed by an exception handler, which is the number
; of arguments pushed here, plus the procedure.

(define (s48-setup-external-exception why nargs)
  (push-exception-setup! why 1)		; 1 = one-byte instruction
  (if (< 10 nargs)
      (error "too many arguments from external exception"))
  (set! *external-exception-nargs* nargs)
  (set! *external-exception?* #t))

; The external code needs to piggyback an exception on top of one already
; being raised.  We increase the argument count and return the old exception.

(define (s48-resetup-external-exception new-why additional-nargs)
  (let* ((old-nargs *external-exception-nargs*)
	 (old-why (stack-ref old-nargs)))
    (stack-set! old-nargs (enter-fixnum new-why))
    (set! *external-exception-nargs* (+ old-nargs additional-nargs))
    old-why))

; Shared bindings

(define-primitive find-undefined-imported-bindings ()
  (lambda ()
    (let loop ((first? #t))
      (let ((vector (s48-gather-objects shared-binding-undefined?
					for-each-imported-binding)))
	(cond ((not (false? vector))
	       (goto return vector))
	      (first?
	       ;; if the result vector couldn't be created force a
	       ;; major collection and try again once.
	       (s48-collect #t)
	       (loop #f))
	      (else
	       (raise-exception heap-overflow 0)))))))

(define-consing-primitive lookup-shared-binding (string-> boolean->)
  (lambda (ignore)
    shared-binding-size)
  (lambda (name is-import? key)
    (goto return
	  (if is-import?
	      (lookup-imported-binding name key)
	      (lookup-exported-binding name key)))))

(define-primitive undefine-shared-binding (string-> boolean->)
  (lambda (name is-import?)
    (undefine-shared-binding! (if is-import?
				  (s48-imported-bindings)
				  (s48-exported-bindings))
			      name)
    (goto continue 0)))
