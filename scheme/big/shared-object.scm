; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber, Marcus Crestani

; This is file shared-object.scm.

; Dynamically load external object files.

(define-record-type shared-object :shared-object
  (make-shared-object name complete-name? c-handle)
  shared-object?
  (name shared-object-name)
  ;; says whether the OS should add a system-dependent extension
  ;; (such as .so or .dll) or do some other such transformation
  (complete-name? shared-object-complete-name?)
  (c-handle shared-object-c-handle
	    set-shared-object-c-handle!))

(define-record-discloser :shared-object
  (lambda (shared-object)
    (list 'shared-object
	  (shared-object-name shared-object))))

;; Doing better would be quite a bit of work: Each shared object would
;; need to maintain a population of addresses derived from it, so we
;; can avoid ordering problems.  Morever, the filename specified might
;; be relative, causing further problems.

;; On the other hand, the high-level code might have a better approach
;; to this, so we don't forbid dumping on these.

(define-record-resumer :shared-object #t)

(define (open-shared-object name complete-name?)
  (let ((shared-object (make-shared-object name
					   complete-name?
					   (external-dlopen (x->os-byte-vector name)
							    complete-name?))))
    (add-finalizer! shared-object close-shared-object)
    shared-object))

(define (close-shared-object shared-object)
  (let ((c-handle (shared-object-c-handle shared-object)))
    (if c-handle
	(begin
	  (external-dlclose c-handle)
	  (set-shared-object-c-handle! shared-object #f)))))

(define-record-type shared-object-address :shared-object-address
  (make-shared-object-address object
			      name
			      value)
  shared-object-address?
  (object shared-object-address-object)
  (name shared-object-address-name)
  (value shared-object-address-value))

(define-record-discloser :shared-object-address
  (lambda (shared-object-address)
    (list 'shared-object-address
	  (shared-object-address-object shared-object-address)
	  (shared-object-address-name shared-object-address))))

(define-record-resumer :shared-object-address #f)

(define (shared-object-address shared-object name)
  (make-shared-object-address shared-object
			      name
			      (external-dlsym (shared-object-c-handle shared-object)
					      (os-string->byte-vector
					       (call-with-os-string-text-codec
						utf-8-codec
						(lambda ()
						  (x->os-string name)))))))

(define (shared-object-address-or-false shared-object name)
  (guard (c (else #f))
    (shared-object-address shared-object name)))

;; This simply calls a C function with no parameters and no return
;; value.  It's typically for calling the initialization function; we
;; can't use any of the regular external-calling mechanisms because
;; they expect a s48_value return value, where the initialization
;; function has void.

(define (call-shared-object-address s-o-address)
  (external-call-thunk (shared-object-address-value s-o-address)))

(import-lambda-definition-2 external-dlopen (name generate-name?)
			  "shared_object_dlopen")
(import-lambda-definition-2 external-dlsym (handle name)
			  "shared_object_dlsym")
(import-lambda-definition-2 external-dlclose (shared-object)
			  "shared_object_dlclose")
(import-lambda-definition-2 external-call-thunk (address)
			  "shared_object_call_thunk")
