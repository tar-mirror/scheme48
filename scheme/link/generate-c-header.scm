; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Marcus Crestani,
; Harald Glab-Phlak

; [This is a kludge.  Richard is loathe to include it in the
; distribution.  But now the system itself uses it, so we're stuck.]

; Reads arch.scm and data.scm and writes out a C .h file with constants
; and macros for dealing with Scheme 48 data structures.

; Needs Big Scheme.

; (make-c-header-file "scheme48.h" "scheme48.h.in"
;                     "vm/arch.scm" "vm/data.scm" "rts/record.scm")

(define (make-c-header-file c-file c-in-file arch-file data-file record-file)
  (receive (stob-list stob-data exception-list channel-status-list)
      (search-file arch-file
		   '("stob enumeration"
		     "(define stob-data ...)"
		     "exception enumeration"
		     "channel-status enumeration")
		   (defines-enum? 'stob)
		   enum-definition-list
		   (lambda (x)
		     (and (eq? (car x) 'define)
			  (eq? (cadr x) 'stob-data)))
		   (lambda (x) (cadr (caddr x)))
		   (defines-enum? 'exception)
		   enum-definition-list
		   (defines-enum? 'channel-status-option)
		   enum-definition-list)
    (receive (tag-list immediate-list)
	(search-file data-file
		     '("tag enumeration" "imm enumeration")
		     (defines-enum? 'tag)
		     enum-definition-list
		     (defines-enum? 'imm)
		     enum-definition-list)
      (let ((record-type-fields
	     (search-file record-file
			  '("(define record-type-fields ...")
			  (lambda (x)
			    (and (eq? (car x) 'define)
				 (eq? (cadr x) 'record-type-fields)))
			  (lambda (x) (cadr (caddr x))))))
	(with-output-to-file c-file
	  (lambda ()
	    (format #t "/* This file was generated automatically.~%")
	    (format #t "   It's probably not a good idea to change it. */~%")
	    (newline)
	    (format #t "#ifndef _H_SCHEME48~%")
	    (format #t "#define _H_SCHEME48~%")
	    (newline)
	    (format #t "#include <scheme48arch.h>~%")
	    (newline)
	    (format #t "#ifdef __cplusplus~%")
	    (format #t "extern \"C\"~%")
	    (format #t "{~%")
	    (format #t "#endif~%")
	    (newline)
	    (copy-file c-in-file)
	    (newline)
	    (format #t "/* New FFI */~%")
	    (newline)
	    (tag-stuff-2 tag-list)
	    (newline)
	    (immediate-stuff-2 immediate-list)
	    (newline)
	    (stob-stuff-2 stob-list stob-data)
	    (newline)
	    (enumeration-stuff-2 record-type-fields
			       "s48_record_type_~A_2(c, x) s48_unsafe_record_ref_2(c, (x), ~D)")
	    (newline)
	    (enumeration-stuff-2 exception-list "s48_exception_~A ~D")
	    (newline)
	    (enumeration-stuff-2 channel-status-list
		       "s48_channel_status_~A_2(c) s48_unsafe_enter_long_as_fixnum_2(c, ~D)")
	    (newline)
	    (format #t "#ifndef NO_OLD_FFI~%")
	    (newline)
	    (tag-stuff tag-list)
	    (newline)
	    (immediate-stuff immediate-list)
	    (newline)
	    (stob-stuff stob-list stob-data)
	    (newline)
	    (enumeration-stuff record-type-fields
			       "S48_RECORD_TYPE_~A(x) S48_RECORD_REF((x), ~D)")
	    (newline)
	    (enumeration-stuff channel-status-list
		       "S48_CHANNEL_STATUS_~A S48_UNSAFE_ENTER_FIXNUM(~D)")
	    (newline)
	    (format #t "#endif /* !NO_OLD_FFI */~%")
	    (newline)
	    (enumeration-stuff exception-list "S48_EXCEPTION_~A ~D")
	    (newline)
	    (format #t "#include <scheme48write-barrier.h>~%")
	    (newline)
	    (format #t "#ifdef __cplusplus~%")
	    (format #t "/* closing brace for extern \"C\" */~%")
	    (format #t "}~%")
	    (format #t "#endif~%")
	    (newline)
	    (format #t "#endif /* _H_SCHEME48 */")
	    (newline)))))))

(define (tag-stuff-2 tag-list)
  (do ((tags tag-list (cdr tags))
       (i 0 (+ i 1)))
      ((null? tags))
    (let ((name (downcase (car tags))))
      (c-define "s48_~A_tag ~D" name i)
      (c-define "s48_~A_p_2(c,x) (((long)s48_deref(x) & 3L) == s48_~A_tag)" name name)))
  ;; The write barrier of the bibop garbage collector needs S48_STOB_P.
  ;; Make sure it is defined for the combination of BIBOP and new FFI.
  (format #t "#if defined(S48_GC_BIBOP) && defined(NO_OLD_FFI)~%")
  (c-define "S48_STOB_P(x) (((long)(x) & 3L) == s48_stob_tag)")
  (format #t "#endif~%")
  (newline)
  (c-define "s48_unsafe_enter_long_as_fixnum_2(c, n)   (s48_make_local_ref(c,(s48_value)((n) << 2)))")
  (c-define "s48_unsafe_extract_long_2(c, x) ((long)s48_deref(x) >> 2)"))

(define (immediate-stuff-2 imm-list)
  (c-define "MISC_IMMEDIATE_INTERNAL_2(n) (s48_immediate_tag | ((n) << 2))")
  (do ((imm imm-list (cdr imm))
       (i 0 (+ i 1)))
      ((null? imm))
    (let ((name (downcase (car imm))))
      (c-define "_s48_value_~A    MISC_IMMEDIATE_INTERNAL_2(~D)" name i)
      (c-define "s48_~A_2(c)   s48_make_local_ref(c, _s48_value_~A)" name name)))
  (newline)
  (c-define "s48_unsafe_enter_char_2(call, c) s48_make_local_ref (call, _s48_value_char | ((c) << 8))")
  (c-define "s48_unsafe_extract_char_2(c,x) ((long)(s48_deref(x) >> 8))")
  (c-define "s48_char_p_2(c, x) ((((long) s48_deref(x)) & 0xff) == _s48_value_char)"))

(define (enumeration-stuff-2 names format-string)
  (do ((names names (cdr names))
       (i 0 (+ 1 i)))
      ((null? names))
    (let ((name (downcase (car names))))
      (c-define format-string name i))))

(define (stob-stuff-2 stob-list stob-data)
  (let ((type-mask (let ((len (length stob-list)))
		     (do ((i 2 (* i 2)))
			 ((>= i len) (- i 1))))))
    (c-define "ADDRESS_AFTER_HEADER_INTERNAL_2(x, type) ((type *)((x) - s48_stob_tag))")
    (c-define "STOB_REF_INTERNAL_2(x, i) ADDRESS_AFTER_HEADER_INTERNAL_2(x, s48_value)[i]")
    (c-define "STOB_BYTE_REF_INTERNAL_2(x, i) (((char *) ADDRESS_AFTER_HEADER_INTERNAL_2(x, s48_value))[i])")
    (c-define "s48_address_after_header_2(c, x, type) ADDRESS_AFTER_HEADER_INTERNAL_2(s48_deref(x), type)")
    (c-define "s48_unsafe_stob_ref_2(c, x, i) s48_make_local_ref(c, (STOB_REF_INTERNAL_2(s48_deref(x), i)))")
    (c-define "s48_unsafe_stob_byte_ref_2(c, x, i) STOB_BYTE_REF_INTERNAL_2(s48_deref(x), i)")
    (c-define (string-append
	       "s48_unsafe_stob_set_2(c, x, i, r) "
	       "do { "
	       "s48_ref_t __stob_set_x_ref = (x); "
	       "s48_ref_t __stob_set_r_ref = (r); "
	       "long __stob_set_i = (i); "
	       "s48_value __stob_set_x = s48_deref(__stob_set_x_ref); "
	       "s48_value __stob_set_v = s48_deref(__stob_set_r_ref); "
	       "if (s48_stob_immutablep_2(c, (x))) "
	       "s48_assertion_violation_2(c, NULL, \"immutable stob\", 1, __stob_set_x); "
	       "else { "
	       "S48_WRITE_BARRIER((__stob_set_x), "
	       "(char *) (&(STOB_REF_INTERNAL_2((__stob_set_x), (__stob_set_i)))),"
	       "(__stob_set_v)); "
	       "*(&STOB_REF_INTERNAL_2((__stob_set_x), (__stob_set_i))) = (__stob_set_v); "
	       "} "
	       "} while (0)"))
    (c-define (string-append
	       "s48_unsafe_stob_byte_set_2(c, x, i, v) "
	       "do { "
	       "long __stob_set_i = (i); "
	       "char __stob_set_v = (v); "
	       "s48_value __stob_set_x = s48_deref(x); "
	       "if (s48_stob_immutablep_2(c, (x))) "
	       "s48_assertion_violation(NULL, \"immutable stob\", 1, __stob_set_x); "
	       "else "
	       "*(&STOB_BYTE_REF_INTERNAL_2((__stob_set_x), (__stob_set_i))) = (__stob_set_v); "
	       "} while (0)"))
    (c-define "s48_stob_header_2(c, x) (STOB_REF_INTERNAL_2(s48_deref(x), -1))")
    (c-define "s48_stob_type_2(c, x)   ((s48_stob_header_2(c, x)>>2)&~D)" type-mask)
    (c-define "s48_stob_address_2(c, x) (&(s48_stob_header_2(c, x)))")
    (c-define "s48_unsafe_stob_byte_length_2(c, x) (s48_stob_header_2(c, x) >> 8)")
    (c-define "s48_unsafe_stob_descriptor_length_2(c, x) (s48_unsafe_stob_byte_length_2(c, x) >> S48_LOG_BYTES_PER_CELL)")
    (c-define "s48_stob_immutablep_2(c, x) ((s48_stob_header_2(c, x)>>7) & 1)")
    (c-define "s48_stob_make_immutable_2(c, x) ((s48_stob_header_2(c, x)) |= (1<<7))")
    (newline)
    (do ((stob stob-list (cdr stob))
	 (i 0 (+ i 1)))
	((null? stob))
      (let ((name (downcase (car stob))))
	(c-define "s48_stobtype_~A ~D" name i)
	(c-define "s48_~A_p_2(c, x) (s48_stob_has_type_2(c, x, ~D))" name i)))
    (newline)
    (for-each (lambda (data)
		(let ((type (downcase (car data))))
		  (do ((accs (cdddr data) (cdr accs))
		       (i 0 (+ i 1)))
		      ((null? accs))
		    (let ((name (downcase (caar accs))))
		      (c-define "s48_~A_offset ~D" name i)
		      (c-define "s48_~A_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_~A, ~D))"
				name type i)
		      (c-define "s48_unsafe_~A_2(c, x) (s48_unsafe_stob_ref_2(c, (x), ~D))" name i))
		    (if (not (null? (cdar accs)))
			(let ((name (downcase (cadar accs))))
			  (c-define "s48_~A_2(c, x, r) (s48_stob_set_2(c, (x), s48_stobtype_~A, ~D, (r)))"
				    name type i)
			  (c-define "s48_unsafe_~A_2(c, x, r) s48_unsafe_stob_set_2(c, (x), ~D, (r))" name i))))))
	      stob-data)
    (newline)
    (for-each (lambda (type index)
		(c-define "s48_~A_length_2(c, x) (s48_stob_length_2(c, (x), s48_stobtype_~A))"
			  type type)
		(c-define "s48_unsafe_~A_length_2(c, x) (s48_unsafe_stob_descriptor_length_2(c, x))"
			  type)
		(c-define "s48_unsafe_~A_ref_2(c, x, i) (s48_unsafe_stob_ref_2(c, (x), ~A))"
			  type index)
		(c-define "s48_unsafe_~A_set_2(c, x, i, r) s48_unsafe_stob_set_2(c, (x), ~A, (r))"
			  type index)
		(c-define "s48_~A_ref_2(c, x, i) (s48_stob_ref_2(c, (x), s48_stobtype_~A, ~A))"
			  type type index)
		(c-define "s48_~A_set_2(c, x, i, r) s48_stob_set_2(c, (x), s48_stobtype_~A, ~A, (r))"
			  type type index))
	      '("vector" "record")
	      '("(i)" "(i) + 1"))
    (c-define "s48_record_type_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_record, 0))")
    (c-define "s48_unsafe_record_type_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 0))")
    (for-each (lambda (type)
		(c-define "s48_~A_length_2(c, x) (s48_stob_byte_length_2(c, (x), s48_stobtype_~A))"
			  type type)
		(c-define "s48_~A_ref_2(c, x, i) (s48_stob_byte_ref_2(c, (x), s48_stobtype_~A, (i)))"
			  type type)
		(c-define "s48_~A_set_2(c, x, i, v) (s48_stob_byte_set_2(c, (x), s48_stobtype_~A, (i), (v)))"
			  type type)
		(c-define "s48_unsafe_~A_length_2(c, x) (s48_unsafe_stob_byte_length_2(c, (x), s48_stobtype_~A))"
			  type type)
		(c-define "s48_unsafe_~A_ref_2(c, x, i) (s48_stob_byte_ref_2(c, (x), s48_stobtype_~A, (i)))"
			  type type)
		(c-define "s48_unsafe_~A_set_2(c, x, i, v) (s48_stob_byte_set_2(c, (x), s48_stobtype_~A, (i), (v)))"
			  type type))
	      '("byte_vector"))
    (c-define "s48_unsafe_extract_byte_vector_2(c, x) (s48_address_after_header_2(c, (x), char))")

    (c-define (string-append "s48_extract_external_object_2(c, x, type) "
			     "((type *)(s48_address_after_header_2(c, x, long)+1))"))))

(define (tag-stuff tag-list)
  (do ((tags tag-list (cdr tags))
       (i 0 (+ i 1)))
      ((null? tags))
    (let ((name (upcase (car tags))))
      (c-define "S48_~A_TAG ~D" name i)
      (c-define "S48_~A_P(x) (((long)(x) & 3L) == S48_~A_TAG)" name name)))
  (newline)
  (c-define "S48_UNSAFE_ENTER_FIXNUM(n)   ((s48_value)((n) << 2))")
  (c-define "S48_UNSAFE_EXTRACT_FIXNUM(x) ((long)(x) >> 2)"))

(define (immediate-stuff imm-list)
  (c-define "S48_MISC_IMMEDIATE(n) (S48_IMMEDIATE_TAG | ((n) << 2))")
  (do ((imm imm-list (cdr imm))
       (i 0 (+ i 1)))
      ((null? imm))
    (let ((name (upcase (car imm))))
      (c-define "S48_~A    (S48_MISC_IMMEDIATE(~D))" name i)))
  (newline)
  (c-define "S48_UNSAFE_ENTER_CHAR(c) (S48_CHAR | ((c) << 8))")
  (c-define "S48_UNSAFE_EXTRACT_CHAR(x) ((long)((x) >> 8))")
  (c-define "S48_CHAR_P(x) ((((long) (x)) & 0xff) == S48_CHAR)"))

(define (stob-stuff stob-list stob-data)
  (let ((type-mask (let ((len (length stob-list)))
		     (do ((i 2 (* i 2)))
			 ((>= i len) (- i 1))))))
    (c-define "ADDRESS_AFTER_HEADER_INTERNAL(x, type) ((type *)((x) - S48_STOB_TAG))")
    (c-define "STOB_REF_INTERNAL(x, i) ADDRESS_AFTER_HEADER_INTERNAL(x, s48_value)[i]")
    (c-define "STOB_BYTE_REF_INTERNAL(x, i) (((char *) ADDRESS_AFTER_HEADER_INTERNAL(x, s48_value))[i])")
    (c-define "S48_ADDRESS_AFTER_HEADER(x, type) ADDRESS_AFTER_HEADER_INTERNAL(x, type)")
    (c-define "S48_STOB_REF(x, i) STOB_REF_INTERNAL((x), i)")
    (c-define "S48_STOB_BYTE_REF(x, i) STOB_BYTE_REF_INTERNAL((x), i)")
    (c-define (string-append
	       "S48_STOB_SET(x, i, v) "
	       "do { "
	       "s48_value __stob_set_x = (x); "
	       "long __stob_set_i = (i); "
	       "s48_value __stob_set_v = (v); "
	       "if (S48_STOB_IMMUTABLEP(__stob_set_x)) "
	       "s48_assertion_violation(NULL, \"immutable stob\", 1, __stob_set_x); "
	       "else { "
	       "S48_WRITE_BARRIER((__stob_set_x), "
	       "(char *) (&S48_STOB_REF((__stob_set_x), (__stob_set_i))),"
	       "(__stob_set_v)); "
	       "*(&S48_STOB_REF((__stob_set_x), (__stob_set_i))) = (__stob_set_v); "
	       "} "
	       "} while (0)"))
    (c-define (string-append
	       "S48_STOB_BYTE_SET(x, i, v) "
	       "do { "
	       "s48_value __stob_set_x = (x); "
	       "long __stob_set_i = (i); "
	       "char __stob_set_v = (v); "
	       "if (S48_STOB_IMMUTABLEP(__stob_set_x)) "
	       "s48_assertion_violation(NULL, \"immutable stob\", 1, __stob_set_x); "
	       "else "
	       "*(&S48_STOB_BYTE_REF((__stob_set_x), (__stob_set_i))) = (__stob_set_v); "
	       "} while (0)"))
    (c-define "S48_STOB_HEADER(x) (S48_STOB_REF((x),-1))")
    (c-define "S48_STOB_TYPE(x)   ((S48_STOB_HEADER(x)>>2)&~D)" type-mask)
    (c-define "S48_STOB_ADDRESS(x) (&(S48_STOB_HEADER(x)))")
    (c-define "S48_STOB_BYTE_LENGTH(x) ((unsigned long)S48_STOB_HEADER(x) >> 8)")
    (c-define "S48_STOB_DESCRIPTOR_LENGTH(x) (S48_STOB_BYTE_LENGTH(x) >> S48_LOG_BYTES_PER_CELL)")
    (c-define "S48_STOB_IMMUTABLEP(x) ((S48_STOB_HEADER(x)>>7) & 1)")
    (c-define "S48_STOB_MAKE_IMMUTABLE(x) ((S48_STOB_HEADER(x)) |= (1<<7))")
    (newline)
    (do ((stob stob-list (cdr stob))
	 (i 0 (+ i 1)))
	((null? stob))
      (let ((name (upcase (car stob))))
	(c-define "S48_STOBTYPE_~A ~D" name i)
	(c-define "S48_~A_P(x) (s48_stob_has_type(x, ~D))" name i)))
    (newline)
    (for-each (lambda (data)
		(let ((type (upcase (car data))))
		  (do ((accs (cdddr data) (cdr accs))
		       (i 0 (+ i 1)))
		      ((null? accs))
		    (let ((name (upcase (caar accs))))
		      (c-define "S48_~A_OFFSET ~D" name i)
		      (c-define "S48_~A(x) (s48_stob_ref((x), S48_STOBTYPE_~A, ~D))"
				name type i)
		      (c-define "S48_UNSAFE_~A(x) (S48_STOB_REF((x), ~D))" name i))
		    (if (not (null? (cdar accs)))
			(let ((name (upcase (cadar accs))))
			  (c-define "S48_~A(x, v) (s48_stob_set((x), S48_STOBTYPE_~A, ~D, (v)))"
				    name type i)
			  (c-define "S48_UNSAFE_~A(x, v) S48_STOB_SET((x), ~D, (v))" name i))))))
	      stob-data)
    (newline)
    (for-each (lambda (type index)
		(c-define "S48_~A_LENGTH(x) (s48_stob_length((x), S48_STOBTYPE_~A))"
			  type type)
		(c-define "S48_UNSAFE_~A_LENGTH(x) (S48_STOB_DESCRIPTOR_LENGTH(x))"
			  type)
		(c-define "S48_~A_REF(x, i) (s48_stob_ref((x), S48_STOBTYPE_~A, ~A))"
			  type type index)
		(c-define "S48_~A_SET(x, i, v) (s48_stob_set((x), S48_STOBTYPE_~A, ~A, (v)))"
			  type type index)
		(c-define "S48_UNSAFE_~A_REF(x, i) (S48_STOB_REF((x), ~A))"
			  type index)
		(c-define "S48_UNSAFE_~A_SET(x, i, v) S48_STOB_SET((x), ~A, (v))"
			  type index))
	      '("VECTOR" "RECORD")
	      '("(i)" "(i) + 1"))
    (c-define "S48_RECORD_TYPE(x) (s48_stob_ref((x), S48_STOBTYPE_RECORD, 0))")
    (c-define "S48_UNSAFE_RECORD_TYPE(x) (S48_STOB_REF((x), 0))")
    (for-each (lambda (type)
		(c-define "S48_~A_LENGTH(x) (s48_stob_byte_length((x), S48_STOBTYPE_~A))"
			  type type)
		(c-define "S48_~A_REF(x, i) (s48_stob_byte_ref((x), S48_STOBTYPE_~A, (i)))"
			  type type)
		(c-define "S48_~A_SET(x, i, v) (s48_stob_byte_set((x), S48_STOBTYPE_~A, (i), (v)))"
			  type type)
		(c-define "S48_UNSAFE_~A_REF(x, i) (S48_STOB_BYTE_REF((x), (i)))"
			  type)
		(c-define "S48_UNSAFE_~A_SET(x, i, v) S48_BYTE_STOB_SET((x), (i), (v))"
			  type))
	      '("BYTE_VECTOR"))
    (c-define "S48_UNSAFE_BYTE_VECTOR_LENGTH(x) (S48_STOB_BYTE_LENGTH(x))")
    (c-define "S48_UNSAFE_EXTRACT_BYTE_VECTOR(x) (S48_ADDRESS_AFTER_HEADER((x), char))")

    (c-define "S48_STRING_LENGTH(s) (s48_string_length(s))")
    (c-define "S48_STRING_REF(s, i) (s48_string_ref((s), (i)))")
    (c-define "S48_STRING_SET(s, i, v) (s48_string_set((s), (i), (v)))")

    (c-define (string-append "S48_EXTRACT_EXTERNAL_OBJECT(x, type) "
			     "((type *)(S48_ADDRESS_AFTER_HEADER(x, long)+1))"))))

(define (enumeration-stuff names format-string)
  (do ((names names (cdr names))
       (i 0 (+ 1 i)))
      ((null? names))
    (let ((name (upcase (car names))))
      (c-define format-string name i))))

; - becomes _ > becomes TO_ (so -> turns into _TO_)
; ? becomes P
; ! disappears

(define (upcase symbol)
  (do ((chars (string->list (symbol->string symbol)) (cdr chars))
       (res '() (case (car chars)
		  ((#\>) (append (string->list "_OT") res))
		  ((#\-) (cons #\_ res))
		  ((#\?) (cons #\P res))
		  ((#\/ #\!) res)
		  (else (cons (char-upcase (car chars)) res)))))
      ((null? chars)
       (list->string (reverse res)))))


(define (downcase symbol)
  (do ((chars (string->list (symbol->string symbol)) (cdr chars))
       (res '() (case (car chars)
		  ((#\>) (append (string->list "_ot") res))
		  ((#\-) (cons #\_ res))
		  ((#\?) (cons #\p res))
		  ((#\/ #\!) res)
		  (else (cons (char-downcase (car chars)) res)))))
      ((null? chars)
       (list->string (reverse res)))))

(define (c-define string . stuff)
  (format #t "#define ~?~%" string stuff))
    
(define (defines-enum? name)
  (lambda (x)
    (and (eq? (car x) 'define-enumeration)
	 (eq? (cadr x) name))))

(define enum-definition-list caddr)

; Copy the file to the current-output-file.

(define (copy-file filename)
  (call-with-input-file filename
      (lambda (in)
	(let loop ()
	  (let ((c (read-char in)))
	    (if (not (eof-object? c))
		(begin
		  (write-char c)
		  (loop))))))))

; WHAT-FOR is a list of names, used only for debugging.
; PRED+EXTRACT is a list of <predicate0> <extract0> <predicate1> <extract1> ... .
; Each form in the file is read and passed to the predicates that haven't yet
; matched.  If the predicate matches the corresponding extractor is called on
; the form.  The results of the extractors are returned.
;
; STUFF is list of ((predicate . extract) . name).  <name> is replaced
; with the value when it is found.

(define (search-file file what-for . pred+extract)
  (let ((stuff (do ((p+e pred+extract (cddr p+e))
		    (names what-for (cdr names))
		    (ps '() (cons (cons (cons (car p+e) (cadr p+e))
					(car names))
				  ps)))
		   ((null? p+e) (reverse ps)))))

    (define (search next not-found)
      (let loop ((n-f not-found) (checked '()))
	(cond ((null? n-f)
	       #f)
	      (((caaar n-f) next)
	       (set-cdr! (car n-f) ((cdaar n-f) next))
	       (append (reverse checked) (cdr n-f)))
	      (else
	       (loop (cdr n-f) (cons (car n-f) checked))))))

    (with-input-from-file file
      (lambda ()
	(let loop ((not-found stuff))
	  (let ((next (read)))
	    (cond ((null? not-found)
		   (apply values (map cdr stuff)))
		  ((eof-object? next)
		   (error "file ~S doesn't have ~A" file (cdar not-found)))
		  (else
		   (loop (or (and (pair? next)
				  (search next not-found))
			     not-found))))))))))
