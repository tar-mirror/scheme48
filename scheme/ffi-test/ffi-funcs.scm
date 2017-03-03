; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Harald Glab-Plhak

(import-dynamic-externals "=scheme48external/ffi-test")

(define (ffi-add-integer int-arg)
  (external-ffi-add-integer int-arg))

(define-enumerated-type color :color
  color?
  colors
  color-name
  color-index
  (red blue green))

(define-enum-set-type color-set :color-set
                      color-set?
                      make-color-set
  color color? colors color-index)

(define-record-type a-record :a-record
  (ffi-make-a-record id type value)
  a-record?
  (id a-record-id)
  (type a-record-type)
  (value a-record-value))

(define-record-discloser :a-record
  (lambda (r)
    (list 'a-record-fields
	  (a-record-id r)
	  (a-record-type r)
	  (a-record-value r))))

(define (ffi-make-a-record string)
  ( external-ffi-make-a-record string))

(define (ffi-working-on-lists in)
  (external-ffi-working-on-lists in))

(define (ffi-get-cons-val first second)
  (external-ffi-get-cons-val first second))

(define (ffi-pair? in)
  (external-ffi-pair? in))

(define (ffi-car in)
  (external-ffi-car in))

(define (ffi-cdr in)
  (external-ffi-cdr in))

(define (ffi-length in)
  (external-ffi-length in))

(define (ffi-record-set! rec ind val) 
  (external-ffi-record-set! rec ind val))

(define (ffi-record-ref rec ind ) 
  (external-ffi-record-ref rec ind))

(define (ffi-vector-set! vect ind val) 
  (external-ffi-vector-set! vect ind val))

(define (ffi-vector-ref vect ind) 
  (external-ffi-vector-ref vect ind))

(define (ffi-make-byte-vector length)
  (external-ffi-make-byte-vector length))
			   

(define (ffi-make-vector length value)
  (external-ffi-make-vector length value))
			    
(define (ffi-enums enum)
  (external-ffi-enums enum))

(define (ffi-get-color-enum-set mask)
  (external-ffi-get-color-enum-set mask))

(define (ffi-call-scheme proc nargs parm-1 parm-2 parm-3)
  (external-ffi-call-scheme proc nargs parm-1 parm-2 parm-3))

(define (ffi-a-status-set-and-export! value)
  (external-ffi-a-status-set-and-export! value))

(define (ffi-a-status-set! value)
  (external-ffi-a-status-set! value))

(define (ffi-a-status-set-by-binding! value)
  (let ((a-status-binding (lookup-imported-binding "a-status")))
    (external-ffi-a-status-set-by-binding! a-status-binding value)))

(define (ffi-a-status)
  (let ((a-status-binding (lookup-imported-binding "a-status")))
    (external-ffi-a-status a-status-binding)))

(define (ffi-export-bindings)
  (let ((binding (external-ffi-export-bindings)))
    (ffi-propagate-binding-global binding)))

(define (ffi-propagate-binding)
  (let ((a-status-binding (lookup-imported-binding "a-status")))
    (external-ffi-propagate-binding a-status-binding)))

(define (ffi-propagate-binding-global binding)
    (external-ffi-propagate-binding-global binding))

(define (ffi-check-a-status-and-get-name)
  (external-ffi-check-a-status-and-get-name))

(define (ffi-make-strange-value id name)
  (external-ffi-make-strange-value id name))
  
(define (ffi-strange-value->list value)
  (external-ffi-strange-value->list value))

(define (ffi-strange-value-free value)
  (external-ffi-strange-value-free value))

(define (ffi-make-weak-pointer value)
  (external-ffi-make-weak-pointer value))

(define  (ffi-weak-pointer? pointer)
  (external-ffi-weak-pointer? pointer))

(define (ffi-weak-pointer-ref pointer)
  (external-ffi-weak-pointer-ref pointer))

(define (ffi-check-string-latin-1 string)
  (external-ffi-check-string-latin-1 string))

(define (ffi-check-string-utf-8 string)
  (external-ffi-check-string-utf-8 string))

(define (ffi-check-string-utf-16 string)
  (external-ffi-check-string-utf-16 string))


;; definitions needed for access external code

;; bindings 
;; the two scheme48 procedures to get a binding and 
;; its value
;; (lookup-imported-binding "name") ; the shared binding
;; (shared-binding-ref shared-binding) ;returns the value 

;; procedure to define a exported binding
;; (define-exported-binding "a-record-record-type" :a-record)


(define (ffi-get-imp-binding-value bind-name)
  (let ((the-binding (lookup-imported-binding bind-name))) ; the shared binding
    (let ((the-binding-value (shared-binding-ref the-binding))) ;returns the value 
      the-binding-value)))

(define (ffi-get-imp-binding bind-name)
  (let ((the-binding (lookup-imported-binding bind-name))) ; the shared binding
      the-binding))

(define (ffi-get-imp-value-by-binding the-binding)
  (let ((the-binding-value (shared-binding-ref the-binding))) ;returns the value 
    the-binding-value))



(define-exported-binding "a-record-record-type" :a-record)
(define-exported-binding "color-set-type" :color-set)

(import-lambda-definition-2 external-ffi-add-integer
			    (int-arg)
			    "ffi_add_integer")

(import-lambda-definition-2 external-ffi-working-on-lists
			    (lst)
			    "ffi_working_on_lists")

(import-lambda-definition-2 external-ffi-get-cons-val
			    (first second)
			    "ffi_get_cons_val")

(import-lambda-definition-2 external-ffi-pair?
			    (a-pair)
			    "ffi_pair_p")

(import-lambda-definition-2 external-ffi-car
			    (a-pair)
			    "ffi_car")

(import-lambda-definition-2 external-ffi-cdr
			    (a-pair)
			    "ffi_cdr")

(import-lambda-definition-2 external-ffi-length
			    (a-pair)
			    "ffi_length")

(import-lambda-definition-2 external-ffi-make-a-record
                          (string)
                          "ffi_make_a_record")

(import-lambda-definition-2 external-ffi-record-set!
			    (rec ind val)
			    "ffi_record_set")

(import-lambda-definition-2 external-ffi-record-ref
			    (rec ind)
			    "ffi_record_ref")

(import-lambda-definition-2 external-ffi-vector-set! 
			    (vect ind val)
			    "ffi_vector_set")

(import-lambda-definition-2 external-ffi-vector-ref
			    (vect ind)
			    "ffi_vector_ref")

(import-lambda-definition-2 external-ffi-make-byte-vector 
			    (length)
			    "ffi_make_byte_vector")

(import-lambda-definition-2 ffi-extract-byte-vector 
			    (byte-vector)
			    "ffi_extract_byte_vector")

(import-lambda-definition-2 ffi-extract-byte-vector-readonly
			    (byte-vector)
			    "ffi_extract_byte_vector_readonly")

(import-lambda-definition-2 ffi-extract-and-modify-byte-vector 
			    (byte-vector)
			    "ffi_extract_and_modify_byte_vector")

(import-lambda-definition-2 ffi-extract-twice-and-modify-byte-vector 
			    (byte-vector)
			    "ffi_extract_twice_and_modify_byte_vector")

(import-lambda-definition-2 ffi-extract-byte-vector-and-call-scheme
			    (byte-vector callback)
			    "ffi_extract_byte_vector_and_call_scheme")

(import-lambda-definition-2 ffi-extract-byte-vector-assertion
			    (byte-vector)
			    "ffi_extract_byte_vector_assertion")

(import-lambda-definition-2 external-ffi-make-vector
			    (length value)
			    "ffi_make_vector")

(import-lambda-definition-2 external-ffi-enums
			    (enum)
			    "ffi_enums")

(import-lambda-definition-2 external-ffi-get-color-enum-set
			    (mask)
			    "ffi_get_color_enum_set")

(import-lambda-definition-2 external-ffi-call-scheme
			    (proc nargs parm-1 parm-2 parm-3)
			    "ffi_call_scheme")

(import-lambda-definition-2 external-ffi-a-status-set-and-export!
			    (value)
			    "ffi_a_status_set_and_export")

(import-lambda-definition-2 external-ffi-a-status-set-by-binding!
			    (binding value)
			    "ffi_a_status_set_by_binding")

(import-lambda-definition-2 external-ffi-a-status-set!
			    (value)
			    "ffi_a_status_set")

(import-lambda-definition-2 external-ffi-a-status
			    (binding)
			    "ffi_a_status")

(import-lambda-definition-2 external-ffi-export-bindings
			    ()
			    "ffi_export_bindings")

(import-lambda-definition-2  external-ffi-propagate-binding
			     (binding)
			     "ffi_propagate_binding")

(import-lambda-definition-2  external-ffi-propagate-binding-global
			     (binding)
			     "ffi_propagate_binding_global")
(import-lambda-definition-2 external-ffi-check-a-status-and-get-name
			    ()
			    "ffi_check_a_status_and_get_name")

(import-lambda-definition-2 external-ffi-make-strange-value 
			    (id name)
			    "ffi_make_strange_value")

(import-lambda-definition-2 external-ffi-strange-value->list 
			    (strange-val)
			    "ffi_strange_value_to_list")

(import-lambda-definition-2 external-ffi-strange-value-free 
			    (strange-val)
			    "ffi_strange_value_free")

(import-lambda-definition-2 external-ffi-make-local-buf
			    ()
			    "ffi_make_local_buf")

(import-lambda-definition-2 external-ffi-free-local-buf
			    ()
			    "ffi_free_local_buf")

(import-lambda-definition-2 external-ffi-free-local-buf-1
			    ()
			    "ffi_free_local_buf1")

(import-lambda-definition-2 external-ffi-free-local-buf-2
			    ()
			    "ffi_free_local_buf2")

(import-lambda-definition-2 external-ffi-free-local-buf-3
			    ()
			    "ffi_free_local_buf3")

(import-lambda-definition-2 external-ffi-make-weak-pointer
			    (value)
			    "ffi_make_weak_pointer")

(import-lambda-definition-2 external-ffi-weak-pointer?
			    (pointer)
			    "ffi_weak_pointer_p")

(import-lambda-definition-2 external-ffi-weak-pointer-ref
			    (pointer)
			    "ffi_weak_pointer_ref")

(import-lambda-definition-2 external-ffi-check-string-latin-1
			    (string)
			    "ffi_check_string_latin_1")
(import-lambda-definition-2 external-ffi-check-string-utf-8
			    (string)
			    "ffi_check_string_utf_8")
(import-lambda-definition-2 external-ffi-check-string-utf-16
			    (string)
			    "ffi_check_string_utf_16")

;; initialization
(ffi-export-bindings)
