; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber

; Bindings: used to store bindings in packages.

; Representation is type place operator-or-transform-or-#f.
; PLACE is a unique (to EQ?) value, usually a location.

(define-record-type binding :binding
  (really-make-binding type place static)
  binding?
  (type binding-type set-binding-type!)
  (place binding-place set-binding-place!)
  (static binding-static set-binding-static!))

(define-record-discloser :binding
  (lambda (b)
    (list 'binding
	  (binding-type b)
	  (binding-place b)
	  (binding-static b))))

(define (make-binding type place static)
  (really-make-binding type place static))

; Used when updating a package binding.

(define (clobber-binding! binding type place static)
  (set-binding-type! binding type)
  (if place
      (set-binding-place! binding place))
  (set-binding-static! binding static))

; Return a binding that's similar to the given one, but has its type
; replaced with the given type.

(define (impose-type type binding integrate?)
  (if (or (eq? type syntax-type)
	  (not (binding? binding)))
      binding
      (make-binding (if (eq? type undeclared-type)
			(let ((type (binding-type binding)))
			  (if (variable-type? type)
			      (variable-value-type type)
			      type))
			type)
		    (binding-place binding)
		    (if integrate?
			(binding-static binding)
			#f))))

; Return a binding that's similar to the given one, but has any
; procedure integration or other unnecesary static information
; removed.  But don't remove static information for macros (or
; structures, interfaces, etc.)

(define (forget-integration binding)
  (if (and (binding-static binding)
	   (subtype? (binding-type binding) any-values-type))
      (make-binding (binding-type binding)
		    (binding-place binding)
		    #f)
      binding))

; Do X and Y denote the same thing?

(define (same-denotation? x y)
  (or (eq? x y)	    ; was EQUAL? because of names, now just for nodes
      (and (binding? x)
	   (binding? y)
	   (eq? (binding-place x)
		(binding-place y)))))

; Special kludge for shadowing and package mutation.
; Ignore this on first reading.  See env/shadow.scm.

(define (maybe-fix-place! binding)
  (let ((place (binding-place binding)))
    (if (and (location? place)
             (vector? (location-id place)))
        (set-binding-place! binding (follow-forwarding-pointers place))))
  binding)

(define (follow-forwarding-pointers place)
  (let ((id (location-id place)))
    (if (vector? id)
        (follow-forwarding-pointers (vector-ref id 0))
        place)))

