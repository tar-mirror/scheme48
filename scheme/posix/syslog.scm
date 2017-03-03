; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber

; Error codes

(import-dynamic-externals "=scheme48external/posix")

(define-enumerated-type syslog-option :syslog-option
  syslog-option?
  the-syslog-options
  syslog-option-name
  syslog-option-index
  ;; The order of these is known to the C code.
  (console
   delay
   no-delay
   log-pid
   no-wait))

(define-enum-set-type syslog-options :syslog-options
  syslog-options?
  make-syslog-options
  
  syslog-option
  syslog-option?
  the-syslog-options
  syslog-option-index)

(define default-syslog-options (syslog-options))

(define-enumerated-type syslog-facility :syslog-facility
  syslog-facility?
  syslog-facilities
  syslog-facility-name
  syslog-facility-index
  ;; Options for openlog
  ;; The order of these is known to the C code.
  (authorization
   cron
   daemon
   kernel
   lpr
   mail
   news
   user
   uucp
   local0 local1 local2 local3 local4 local5 local6 local7))

(define default-syslog-facility (syslog-facility user))

(define-enumerated-type syslog-level :syslog-level
  syslog-level?
  syslog-levels
  syslog-level-name
  syslog-level-index
  ;; The order of these is known to the C code.
  (emergency	
   alert	
   critical	
   error	
   warning	
   notice	
   info	
   debug))

(define-enum-set-type syslog-mask :syslog-mask
  syslog-mask?
  make-syslog-mask

  syslog-level
  syslog-level?
  syslog-levels
  syslog-level-index)

(define (syslog-mask-upto level)
  (let loop ((index (syslog-level-index level)) (levels '()))
    (if (< index 0)
	(make-syslog-mask levels)
	(loop (- index 1)
	      (cons (vector-ref syslog-levels index)
		    levels)))))

(define syslog-mask-all (make-syslog-mask (vector->list syslog-levels)))

(define default-syslog-mask syslog-mask-all)

; Low-level interface

(import-lambda-definition-2 posix-openlog (ident options facility) "posix_openlog")
(import-lambda-definition-2 posix-setlogmask (logmask) "posix_setlogmask")
(import-lambda-definition-2 posix-syslog (level facility message) "posix_syslog")
(import-lambda-definition-2 posix-closelog () "posix_closelog")

(define (openlog ident options facility)
  (if (not (syslog-options? options))
      (assertion-violation 'openlog "options argument is not a :syslog-options object" options))
  (posix-openlog (x->os-byte-vector ident)
		 (enum-set->integer options)
		 (syslog-facility-index facility)))

(define (setlogmask! logmask)
  (if (not (syslog-mask? logmask))
      (assertion-violation 'openlog "mask argument is not a :syslog-mask object" logmask))
  (posix-setlogmask (enum-set->integer logmask)))

(define (syslog-internal level facility message)
  (posix-syslog (syslog-level-index level)
		(and facility
		     (syslog-facility-index facility))
		(x->os-byte-vector message)))


(define (closelog)
  (posix-closelog))

; High-level interface

(define-record-type syslog-channel :syslog-channel
  (really-make-syslog-channel ident options facility mask)
  syslog-channel?
  (ident syslog-channel-ident)
  (options syslog-channel-options)
  (facility syslog-channel-facility)
  (mask syslog-channel-mask))

(define (make-syslog-channel ident options facility mask)
  (really-make-syslog-channel (x->os-string ident)
			       options facility mask))

(define (syslog-channel-equivalent? channel-1 channel-2)
  (and (os-string=? (syslog-channel-ident channel-1)
		    (syslog-channel-ident channel-2))
       (enum-set=? (syslog-channel-options channel-1)
		   (syslog-channel-options channel-2))
       ;; facility can be specified with each syslog-write
       (enum-set=? (syslog-channel-mask channel-1)
		   (syslog-channel-mask channel-2))))

(define current-syslog-channel 'unitinialized)
(define current-syslog-channel-lock 'unitinialized)

(define (initialize-syslog)
  (set! current-syslog-channel #f)
  (set! current-syslog-channel-lock (make-lock)))

(define open-syslog-channel make-syslog-channel)

(define (close-syslog-channel channel)
  (obtain-lock current-syslog-channel-lock)
  (if (syslog-channel-equivalent? channel
				  current-syslog-channel)
      (closelog))
  (release-lock current-syslog-channel-lock))

(define (with-syslog-channel channel thunk)
  (dynamic-wind
   (lambda ()
     (obtain-lock current-syslog-channel-lock))
   (lambda ()
     (if (or (not current-syslog-channel)
	     (not (syslog-channel-equivalent? channel
					      current-syslog-channel)))
	 (begin
	   (if current-syslog-channel
	       (closelog))
	   (openlog (syslog-channel-ident channel)
		    (syslog-channel-options channel)
		    (syslog-channel-facility channel))
	   (if (not (enum-set=? (syslog-channel-mask channel)
				default-syslog-mask))
	       (setlogmask! (syslog-channel-mask channel)))
	   (set! current-syslog-channel channel)))
     (thunk))
   (lambda ()
     (release-lock current-syslog-channel-lock))))

(define (syslog-write level message channel)
  (with-syslog-channel
   channel
   (lambda ()
     (syslog-internal level (syslog-channel-facility channel) message))))

(define (change-syslog-channel channel ident options facility mask)
  (make-syslog-channel (if ident
			   (x->os-string ident)
			   (syslog-channel-ident channel))
		       (or options
			   (syslog-channel-options channel))
		       (or facility
			   (syslog-channel-facility channel))
		       (or mask
			   (syslog-channel-mask channel))))

; This is a thread fluid in scsh
(define dynamic-syslog-channel
  (make-fluid
   (make-syslog-channel "s48"
			default-syslog-options
			default-syslog-facility
			default-syslog-mask)))

(define (syslog level message . rest)
  (syslog-write level message
		(cond
		 ((null? rest)
		  (fluid dynamic-syslog-channel))
		 ((and (null? (cdr rest))
		       (syslog-channel? (car rest)))
		  (car rest))
		 (else
		  ;; this might be a little excessive allocation
		  (apply change-syslog-channel
			 (fluid dynamic-syslog-channel)
			 (append rest '(#f)))))))

(define (with-syslog-destination ident options facility mask thunk)
  (let-fluid dynamic-syslog-channel
	     (change-syslog-channel
	      (fluid dynamic-syslog-channel)
	      ident options facility mask)
	     thunk))

;----------------
; A record type whose only purpose is to run some code when we start up an
; image.

(define-record-type reinitializer :reinitializer
  (make-reinitializer thunk)
  reinitializer?
  (thunk reinitializer-thunk))

(define-record-discloser :reinitializer
  (lambda (r)
    (list 'reinitializer (reinitializer-thunk r))))

(define-record-resumer :reinitializer
  (lambda (r)
    ((reinitializer-thunk r))))

(initialize-syslog)

(define syslog-reinitializer
  (make-reinitializer initialize-syslog))
