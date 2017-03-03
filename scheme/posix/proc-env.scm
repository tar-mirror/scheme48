; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Roderic Morris,
; Marcus Crestani, Will Noble


; Access to Posix process environment

(import-dynamic-externals "=scheme48external/posix")

; We multiplex a bunch of these to save typing.

;----------------
; 4.1 Process Identification

(define (get-process-id)
  (integer->process-id (posix-get-pid #f)))

(define (get-parent-process-id)
  (integer->process-id (posix-get-pid #t)))

;----------------
; 4.2 User Identification and 4.3 Process Groups

(define (get-user-id)
  (call-imported-binding-2 posix-get-id #t #t))

(define (get-effective-user-id)
  (call-imported-binding-2 posix-get-id #t #f))

(define (get-group-id)
  (call-imported-binding-2 posix-get-id #f #t))

(define (get-effective-group-id)
  (call-imported-binding-2 posix-get-id #f #f))

(define (set-user-id! user-id)
  (call-imported-binding-2 posix-set-id! #t #t user-id))

(define (set-effective-user-id! user-id)
  (call-imported-binding-2 posix-set-id! #t #f user-id))

(define (set-group-id! group-id)
  (call-imported-binding-2 posix-set-id! #f #t group-id))

(define (set-effective-group-id! group-id)
  (call-imported-binding-2 posix-set-id! #f #f group-id))

(import-lambda-definition-2 posix-get-pid (parent?))
(import-definition posix-get-id)
(import-definition posix-set-id! "posix_set_id")

(define (get-groups)
  (call-imported-binding-2 posix-get-groups))

(define (get-login-name)
  (let ((name (call-imported-binding-2 posix-get-login)))
    (if name
        (byte-vector->os-string name)
        name)))

(import-definition posix-get-groups)
(import-definition posix-get-login)

(import-lambda-definition-2 posix-set-sid ())

;----------------
; 4.4 System Identification
;
; The five values returned by uname().

(import-lambda-definition-2 posix-sys-name (which))

(define (os-name)         (posix-sys-name 0))
(define (os-node-name)    (posix-sys-name 1))
(define (os-release-name) (posix-sys-name 2))
(define (os-version-name) (posix-sys-name 3))
(define (machine-name)    (posix-sys-name 4))

;----------------
; 4.5 Get Process Times
;
;----------------
; 4.6 Environment Variables

; We cheat here by using one type for both the variable names and
; their values.  The rules are the same for both, after all.

(define (lookup-environment-variable name)
  (cond
   ((external-lookup-environment-variable (x->os-byte-vector name))
    => x->os-string)
   (else #f)))

(define (lookup-environment-variable->string name)
  (cond
   ((lookup-environment-variable name)
    => os-string->string)
   (else #f)))

(define (set-environment-variable! name value)
  (external-set-environment-variable! (x->os-byte-vector name)
                                      (x->os-byte-vector value)))

(define (environment-alist)
  (map (lambda (pair)
	 (cons (x->os-string (car pair))
	       (x->os-string (cdr pair))))
       (external-environment-alist)))

(define (environment-alist-as-strings)
  (map (lambda (pair)
	 (cons (os-string->string (car pair))
	       (os-string->string (cdr pair))))
       (environment-alist)))

(import-lambda-definition-2 external-lookup-environment-variable (name) "posix_get_env")
(import-lambda-definition-2 external-set-environment-variable! (name value) "posix_set_env")
(import-lambda-definition-2 external-environment-alist () "posix_get_env_alist")

;----------------
; 4.7 Terminal Identification

; See io.scm.

;----------------
; 4.8 Configurable System Variables
