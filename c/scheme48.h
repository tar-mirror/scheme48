/* This file was generated automatically.
   It's probably not a good idea to change it. */

#ifndef _H_SCHEME48
#define _H_SCHEME48

#include <scheme48arch.h>

#ifdef __cplusplus
extern "C"
{
#endif

/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Richard Kelsey, Jonathan Rees, Marcus Crestani, Mike
 * Sperber, Michael Zabka, Harald Glab-Phlak
 */

#if defined HAVE_STDINT_H
#include <stdint.h> /* uintXX_t, C99 */
#endif

#if defined HAVE_SYS_TYPES_H
#include <sys/types.h> /* size_t */
#endif

typedef long	s48_value;

#define NO_ERRORS 0    /* errno value */

#if SIZEOF_VOID_P == 4
#define S48_MAX_FIXNUM_VALUE ((1 << 29) - 1)
#define S48_MIN_FIXNUM_VALUE (-1 << 29)
#define S48_LOG_BYTES_PER_CELL 2
#elif SIZEOF_VOID_P == 8
#define S48_MAX_FIXNUM_VALUE ((1L << 61) - 1)
#define S48_MIN_FIXNUM_VALUE (-1L << 61)
#define S48_LOG_BYTES_PER_CELL 3
#else
#error "What size are your pointers, really?"
#endif


/* New FFI */

typedef struct s48_ref_s  *s48_ref_t;
typedef struct s48_call_s *s48_call_t;

/* local refs */
S48_EXTERN s48_ref_t  s48_make_local_ref(s48_call_t call, s48_value obj);
S48_EXTERN s48_ref_t  s48_copy_local_ref(s48_call_t call, s48_ref_t ref);
S48_EXTERN void       s48_free_local_ref(s48_call_t call, s48_ref_t ref);
S48_EXTERN void       s48_free_local_ref_array(s48_call_t call, s48_ref_t *refs, size_t len);

/* global refs */
S48_EXTERN s48_ref_t  s48_make_global_ref(s48_value obj);
S48_EXTERN void       s48_free_global_ref(s48_ref_t ref);
S48_EXTERN s48_ref_t  s48_local_to_global_ref(s48_ref_t ref);

/* local bufs */
S48_EXTERN void *s48_make_local_buf (s48_call_t call, size_t s);
S48_EXTERN void s48_free_local_buf (s48_call_t call, void *buffer);

/* subcalls */
S48_EXTERN s48_call_t s48_make_subcall(s48_call_t call);
S48_EXTERN void       s48_free_subcall(s48_call_t subcall);
S48_EXTERN s48_ref_t  s48_finish_subcall(s48_call_t call, s48_call_t subcall, s48_ref_t ref);

/* immediate refs */
S48_EXTERN s48_ref_t  s48_get_immediate_ref(long immediate_index);

/* external code should not use this, but might need to... */
S48_EXTERN void       s48_setref(s48_ref_t ref, s48_value obj);
S48_EXTERN s48_value  s48_deref(s48_ref_t ref);


/* Misc stuff */

#define s48_eq_p_2(c, r1, r2) (s48_deref(r1) == s48_deref(r2))
/* Superceded name for the above definition, retained for compatibility. */
#define s48_eq_2(c, r1, r2) (c, s48_deref(r1) == s48_deref(r2)) 

S48_EXTERN int		s48_stob_has_type_2(s48_call_t, s48_ref_t, int);
S48_EXTERN long		s48_stob_length_2(s48_call_t, s48_ref_t, int);
S48_EXTERN long		s48_stob_byte_length_2(s48_call_t, s48_ref_t, int);
S48_EXTERN s48_ref_t	s48_stob_ref_2(s48_call_t, s48_ref_t, int, long);
S48_EXTERN void		s48_stob_set_2(s48_call_t, s48_ref_t, int, long, s48_ref_t);
S48_EXTERN char		s48_stob_byte_ref_2(s48_call_t, s48_ref_t, int, long);
S48_EXTERN void		s48_stob_byte_set_2(s48_call_t, s48_ref_t, int, long, char);

S48_EXTERN s48_ref_t	s48_make_string_2(s48_call_t, int, long);
S48_EXTERN void		s48_string_set_2(s48_call_t, s48_ref_t s, long i, long c);
S48_EXTERN long		s48_string_ref_2(s48_call_t, s48_ref_t s, long i);
S48_EXTERN long		s48_string_length_2(s48_call_t, s48_ref_t s);
S48_EXTERN s48_ref_t	s48_enter_string_latin_1_2(s48_call_t, const char* s);
S48_EXTERN s48_ref_t	s48_enter_string_latin_1_n_2(s48_call_t, const char* s, long count);
S48_EXTERN long		s48_string_latin_1_length_2(s48_call_t, s48_ref_t s);
S48_EXTERN long		s48_string_latin_1_length_n_2(s48_call_t, s48_ref_t s, long start, long count);
S48_EXTERN void		s48_copy_latin_1_to_string_2(s48_call_t, const char* s, s48_ref_t sch_s);
S48_EXTERN void		s48_copy_latin_1_to_string_n_2(s48_call_t, const char* s, long len, s48_ref_t sch_s);
S48_EXTERN void		s48_copy_string_to_latin_1_2(s48_call_t, s48_ref_t sch_s, char* s);
S48_EXTERN void		s48_copy_string_to_latin_1_n_2(s48_call_t, s48_ref_t sch_s, long start, long count, char* s);
S48_EXTERN char	*	s48_extract_latin_1_from_string_2(s48_call_t, s48_ref_t);
S48_EXTERN s48_ref_t	s48_enter_string_utf_8_2(s48_call_t, const char* s);
S48_EXTERN s48_ref_t	s48_enter_string_utf_8_n_2(s48_call_t, const char* s, long count);
S48_EXTERN long		s48_string_utf_8_length_2(s48_call_t, s48_ref_t s);
S48_EXTERN long		s48_string_utf_8_length_n_2(s48_call_t, s48_ref_t s, long start, long count);
S48_EXTERN long		s48_copy_string_to_utf_8_2(s48_call_t, s48_ref_t sch_s, char* s);
S48_EXTERN long		s48_copy_string_to_utf_8_n_2(s48_call_t, s48_ref_t sch_s, long start, long count, char* s);
S48_EXTERN char	*	s48_extract_utf_8_from_string_2(s48_call_t, s48_ref_t);
S48_EXTERN s48_ref_t	s48_enter_string_utf_16be_2(s48_call_t, const uint16_t *);
S48_EXTERN s48_ref_t	s48_enter_string_utf_16be_n_2(s48_call_t, const uint16_t *, long);
S48_EXTERN long		s48_string_utf_16be_length_2(s48_call_t, s48_ref_t);
S48_EXTERN long		s48_string_utf_16be_length_n_2(s48_call_t, s48_ref_t, long, long);
S48_EXTERN uint16_t *	s48_extract_utf_16be_from_string_2(s48_call_t, s48_ref_t);
S48_EXTERN s48_ref_t	s48_enter_string_utf_16le_2(s48_call_t, const uint16_t *);
S48_EXTERN long		s48_copy_string_to_utf_16be_2(s48_call_t, s48_ref_t, uint16_t *);
S48_EXTERN long		s48_copy_string_to_utf_16be_n_2(s48_call_t, s48_ref_t, long, long, uint16_t *);
S48_EXTERN s48_ref_t	s48_enter_string_utf_16le_n_2(s48_call_t, const uint16_t *, long);
S48_EXTERN long		s48_string_utf_16le_length_2(s48_call_t, s48_ref_t);
S48_EXTERN long		s48_string_utf_16le_length_n_2(s48_call_t, s48_ref_t, long, long);
S48_EXTERN long		s48_copy_string_to_utf_16le_2(s48_call_t, s48_ref_t, uint16_t *);
S48_EXTERN long		s48_copy_string_to_utf_16le_n_2(s48_call_t, s48_ref_t, long, long, uint16_t *);
S48_EXTERN uint16_t *	s48_extract_utf_16le_from_string_2(s48_call_t, s48_ref_t);

S48_EXTERN s48_ref_t	s48_enter_char_2(s48_call_t, long);
S48_EXTERN long 	s48_extract_char_2(s48_call_t, s48_ref_t);
S48_EXTERN s48_ref_t	s48_enter_long_as_fixnum_2(s48_call_t, long);
S48_EXTERN s48_ref_t	s48_enter_long_2(s48_call_t, long);
S48_EXTERN long		s48_extract_long_2(s48_call_t, s48_ref_t);
S48_EXTERN s48_ref_t	s48_enter_unsigned_long_2(s48_call_t, unsigned long);
S48_EXTERN unsigned long s48_extract_unsigned_long_2(s48_call_t, s48_ref_t);
S48_EXTERN s48_ref_t	s48_enter_double_2(s48_call_t, double);
S48_EXTERN double	s48_extract_double_2(s48_call_t, s48_ref_t);

S48_EXTERN s48_ref_t	s48_cons_2(s48_call_t, s48_ref_t, s48_ref_t);
S48_EXTERN s48_ref_t	s48_enter_byte_vector_2(s48_call_t, const char *, long);
S48_EXTERN s48_ref_t	s48_enter_unmovable_byte_vector_2(s48_call_t, const char *, long);
S48_EXTERN char *	s48_extract_byte_vector_2(s48_call_t, s48_ref_t);
S48_EXTERN char *	s48_extract_byte_vector_readonly_2(s48_call_t, s48_ref_t);
S48_EXTERN char *       s48_extract_unmovable_byte_vector_2(s48_call_t, s48_ref_t);
S48_EXTERN void		s48_extract_byte_vector_region_2(s48_call_t, s48_ref_t, long, long, char*);
S48_EXTERN void		s48_enter_byte_vector_region_2(s48_call_t, s48_ref_t, long, long, char*);
S48_EXTERN char *	s48_extract_byte_vector_unmanaged_2(s48_call_t, s48_ref_t);
S48_EXTERN void 	s48_release_byte_vector_2(s48_call_t, s48_ref_t, char*);
S48_EXTERN void         s48_copy_from_byte_vector_2(s48_call_t, s48_ref_t, char *);
S48_EXTERN void         s48_copy_to_byte_vector_2(s48_call_t, s48_ref_t, char *);
S48_EXTERN s48_ref_t	s48_make_vector_2(s48_call_t, long, s48_ref_t);
S48_EXTERN s48_ref_t	s48_make_byte_vector_2(s48_call_t, long);
S48_EXTERN s48_ref_t	s48_make_unmovable_byte_vector_2(s48_call_t, long);
S48_EXTERN s48_ref_t	s48_enter_byte_string_2(s48_call_t, const char *);
S48_EXTERN s48_ref_t	s48_enter_byte_substring_2(s48_call_t, const char *, long);
S48_EXTERN s48_ref_t	s48_make_record_2(s48_call_t, s48_ref_t);
S48_EXTERN s48_ref_t	s48_make_weak_pointer_2(s48_call_t, s48_ref_t);
S48_EXTERN void		s48_check_record_type_2(s48_call_t, s48_ref_t, s48_ref_t);
S48_EXTERN s48_ref_t	s48_length_2(s48_call_t, s48_ref_t);

S48_EXTERN s48_ref_t	s48_enter_pointer_2(s48_call_t, void *);
S48_EXTERN void*	s48_extract_pointer_2(s48_call_t, s48_ref_t);
S48_EXTERN s48_ref_t	s48_get_imported_binding_2(char *);
S48_EXTERN s48_ref_t	s48_get_imported_binding_local_2(s48_call_t, char *);
S48_EXTERN s48_ref_t	s48_define_exported_binding_2(s48_call_t, char *, s48_ref_t);

S48_EXTERN s48_ref_t	s48_set_channel_os_index_2(s48_call_t, s48_ref_t, long);
S48_EXTERN s48_ref_t	s48_add_channel_2(s48_call_t, s48_ref_t, s48_ref_t, long);
S48_EXTERN void		s48_close_channel_2(s48_call_t, long);

S48_EXTERN void		s48_check_enum_set_type_2(s48_call_t, s48_ref_t, s48_ref_t);
S48_EXTERN long		s48_enum_set2integer_2(s48_call_t, s48_ref_t);
S48_EXTERN s48_ref_t	s48_integer2enum_set_2(s48_call_t, s48_ref_t, long);

S48_EXTERN s48_ref_t 	s48_call_scheme_2(s48_call_t call, s48_ref_t proc, long nargs, ...);

S48_EXTERN s48_ref_t s48_get_current_time(s48_call_t call);
S48_EXTERN s48_ref_t s48_get_timezone(s48_call_t call);

#define s48_make_value_2(c, type) (s48_make_byte_vector_2(c, sizeof(type)))
#define s48_make_sized_value_2(c, size) (s48_make_byte_vector_2(c, size))
S48_EXTERN void *	s48_value_pointer_2(s48_call_t, s48_ref_t);

#define s48_extract_value_pointer_2(c, x, type) ((type *) s48_value_pointer_2(c, x))
#define s48_extract_value_2(c, x, type) (*(s48_extract_value_pointer_2(c, (x), type)))
#define s48_value_size_2(c, x) (s48_byte_vector_length_2(c, x))
#define s48_set_value_2(c, x, type, v) (s48_extract_value_2(c, (x), type) = (v))

#define s48_unsafe_extract_value_pointer_2(c, x, type)		\
  (s48_address_after_header_2(c, (x), type))
#define s48_unsafe_extract_value_2(c, x, type)			\
  (*(s48_unsafe_extract_value_pointer_2(c, (x), type)))
#define s48_unsafe_set_value_2(c, x, type, v)			\
  (s48_unsafe_extract_value_2(c, (x), type) = (v))

#define s48_unsafe_extract_double_2(c, x)			\
  (*(s48_address_after_header_2(c, (x), double)))

#define s48_arg_ref_2(c, argv, index, argc) ((argv)[(argc)-(index)-1])

/* Exceptions */

S48_EXTERN void s48_error_2(s48_call_t call, const char* who, const char* message,
			    long irritant_count, ...);
S48_EXTERN void s48_assertion_violation_2(s48_call_t call, const char* who, const char* message,
					  long irritant_count, ...);
S48_EXTERN void s48_os_error_2(s48_call_t call, const char* who, int the_errno,
			       long irritant_count, ...);
S48_EXTERN void s48_out_of_memory_error_2(s48_call_t call);

/* Internal use */

S48_EXTERN void s48_raise_scheme_exception_2(s48_call_t call, long type, long nargs, ...);

/* Type checking */

#define s48_check_pair_2(c, v) do { if (!s48_pair_p_2(c, v)) s48_assertion_violation_2(c, NULL, "must be a pair", 1, v); } while (0)
#define s48_check_fixnum_2(c, v) do { if (!s48_fixnum_p_2(c, v)) s48_assertion_violation_2(c, NULL, "must be a fixnum", 1, v); } while (0)
#define s48_check_string_2(c, v) do { if (!s48_string_p_2(c, v)) s48_assertion_violation_2(c, NULL, "must be a string", 1, v); } while (0)
#define s48_check_byte_vector_2(c, v) do { if (!s48_byte_vector_p_2(c, v)) s48_assertion_violation_2(c, NULL, "must be a bytevector", 1, v); } while (0)
#define s48_check_channel_2(c, v) do { if (!s48_channel_p_2(c, v)) s48_assertion_violation_2(c, NULL, "must be a channel", 1, v); } while (0)
#define s48_check_record_2(c, v) do { if (!s48_record_p_2(c, v)) s48_assertion_violation_2(c, NULL, "must be a record", 1, v); } while (0)
#define s48_check_value_2(c, v) do { if (!s48_byte_vector_p_2(c, v)) s48_assertion_violation_2(c, NULL, "must be an external value", 1, v); } while (0)
#define s48_check_export_binding_2(c, v) do { if (!s48_export_binding_p_2(c, v)) s48_assertion_violation_2(c, NULL, "must be an exported value", 1, v ); } while (0)
#define s48_check_boolean_2(c, v) \
  do { if (!s48_false_p_2(c, v) && !s48_true_p_2(c, v))	\
	 s48_assertion_violation_2(c, NULL, "must be a boolean", 1, v); } while (0)

#define s48_value_p_2(c, v) (s48_byte_vector_p_2(c, v))

#define s48_true_p_2(c, r) (s48_deref(r) == _s48_value_true)
#define s48_false_p_2(c, r) (s48_deref(r) == _s48_value_false)
#define s48_null_p_2(c, r) (s48_deref(r) == _s48_value_null)
#define s48_extract_boolean_2(c, r) (!(s48_deref(r) == _s48_value_false))
#define s48_enter_boolean_2(c, v) ((v) ? s48_true_2(c) : s48_false_2(c))

#define s48_shared_binding_check_2(c, binding) \
  do { if (s48_deref(s48_shared_binding_ref_2(c, binding)) == _s48_value_unspecific) \
      s48_raise_scheme_exception_2(c, s48_exception_unbound_external_name, 1, \
				   s48_shared_binding_name_2(c, binding)); \
  } while(0)



#ifndef NO_OLD_FFI

/* Misc stuff */

#define S48_EQ_P(v1, v2) ((v1) == (v2))
/* Superceded name for the above definition, retained for compatibility. */
#define S48_EQ(v1, v2) ((v1) == (v2)) 

S48_EXTERN int		s48_stob_has_type(s48_value, int);
S48_EXTERN long		s48_stob_length(s48_value, int);
S48_EXTERN long		s48_stob_byte_length(s48_value, int);
S48_EXTERN s48_value	s48_stob_ref(s48_value, int, long);
S48_EXTERN void		s48_stob_set(s48_value, int, long, s48_value);
S48_EXTERN char		s48_stob_byte_ref(s48_value, int, long);
S48_EXTERN void		s48_stob_byte_set(s48_value, int, long, char);

S48_EXTERN char *	s48_register_gc_rootB(char *);
S48_EXTERN void		s48_unregister_gc_rootB(char *);
S48_EXTERN void		s48_push_gc_rootsB(char *, long);
S48_EXTERN char		s48_pop_gc_rootsB(void);

S48_EXTERN s48_value	s48_make_string(int, long);
S48_EXTERN void		s48_string_set(s48_value s, long i, long c);
S48_EXTERN long		s48_string_ref(s48_value s, long i);
S48_EXTERN long		s48_string_length(s48_value s);
S48_EXTERN s48_value	s48_enter_string_latin_1(char* s);
S48_EXTERN s48_value	s48_enter_string_latin_1_n(char* s, long count);
S48_EXTERN void		s48_copy_latin_1_to_string(char* s, s48_value sch_s);
S48_EXTERN void		s48_copy_latin_1_to_string_n(char* s, long len, s48_value sch_s);
S48_EXTERN void		s48_copy_string_to_latin_1(s48_value sch_s, char* s);
S48_EXTERN void		s48_copy_string_to_latin_1_n(s48_value sch_s, long start, long count, char* s);
S48_EXTERN s48_value	s48_enter_string_utf_8(char* s);
S48_EXTERN s48_value	s48_enter_string_utf_8_n(char* s, long count);
S48_EXTERN long		s48_string_utf_8_length(s48_value s);
S48_EXTERN long		s48_string_utf_8_length_n(s48_value s, long start, long count);
S48_EXTERN long		s48_copy_string_to_utf_8(s48_value sch_s, char* s);
S48_EXTERN long		s48_copy_string_to_utf_8_n(s48_value sch_s, long start, long count, char* s);
S48_EXTERN s48_value	s48_enter_string_utf_16be(const uint16_t *);
S48_EXTERN s48_value	s48_enter_string_utf_16be_n(const uint16_t *, long);
S48_EXTERN long		s48_string_utf_16be_length(s48_value);
S48_EXTERN long		s48_string_utf_16be_length_n(s48_value, long, long);
S48_EXTERN long		s48_copy_string_to_utf_16be(s48_value, uint16_t *);
S48_EXTERN long		s48_copy_string_to_utf_16be_n(s48_value, long, long, uint16_t *);
S48_EXTERN s48_value	s48_enter_string_utf_16le(const uint16_t *);
S48_EXTERN s48_value	s48_enter_string_utf_16le_n(const uint16_t *, long);
S48_EXTERN long		s48_string_utf_16le_length(s48_value);
S48_EXTERN long		s48_string_utf_16le_length_n(s48_value, long, long);
S48_EXTERN long		s48_copy_string_to_utf_16le(s48_value, uint16_t *);
S48_EXTERN long		s48_copy_string_to_utf_16le_n(s48_value, long, long, uint16_t *);

S48_EXTERN s48_value	s48_enter_char(long);
S48_EXTERN long 	s48_extract_char(s48_value);
S48_EXTERN s48_value	s48_enter_fixnum(long);
S48_EXTERN long		s48_extract_fixnum(s48_value);
S48_EXTERN s48_value	s48_enter_integer(long);
S48_EXTERN long		s48_extract_integer(s48_value);
S48_EXTERN s48_value	s48_enter_unsigned_integer(unsigned long);
S48_EXTERN unsigned long s48_extract_unsigned_integer(s48_value);
S48_EXTERN s48_value	s48_enter_double(double);
S48_EXTERN double	s48_extract_double(s48_value);

S48_EXTERN s48_value	s48_cons(s48_value, s48_value);
S48_EXTERN s48_value	s48_enter_byte_vector(char *, long);
S48_EXTERN s48_value	s48_enter_unmovable_byte_vector(char *, long);
S48_EXTERN char *	s48_extract_byte_vector(s48_value);
S48_EXTERN s48_value	s48_make_vector(long, s48_value);
S48_EXTERN s48_value	s48_make_byte_vector(long);
S48_EXTERN s48_value	s48_make_unmovable_byte_vector(long);
S48_EXTERN s48_value	s48_enter_byte_string(char *);
S48_EXTERN s48_value	s48_enter_byte_substring(char *, long);
S48_EXTERN s48_value	s48_make_record(s48_value);
S48_EXTERN s48_value	s48_make_weak_pointer(s48_value);
S48_EXTERN void		s48_check_record_type(s48_value, s48_value);
S48_EXTERN s48_value	s48_length(s48_value);
S48_EXTERN void*	s48_extract_pointer(s48_value);
S48_EXTERN s48_value	s48_get_imported_binding(char *);

S48_EXTERN s48_value	s48_set_channel_os_index(s48_value, long);
S48_EXTERN s48_value	s48_add_channel(s48_value, s48_value, long);
S48_EXTERN void		s48_close_channel(long);

S48_EXTERN void		s48_check_enum_set_type(s48_value, s48_value);
S48_EXTERN long		s48_enum_set2integer(s48_value);
S48_EXTERN s48_value	s48_integer2enum_set(s48_value, long);

S48_EXTERN s48_value	s48_call_scheme(s48_value proc, long nargs, ...);

#define S48_MAKE_VALUE(type) (s48_make_byte_vector(sizeof(type)))
#define S48_MAKE_SIZED_VALUE(size) (s48_make_byte_vector(size))
S48_EXTERN void *	s48_value_pointer(s48_value);

#define S48_EXTRACT_VALUE_POINTER(x, type) ((type *) s48_value_pointer(x))
#define S48_EXTRACT_VALUE(x, type) (*(S48_EXTRACT_VALUE_POINTER((x), type)))
#define S48_VALUE_SIZE(x) (S48_BYTE_VECTOR_LENGTH(x))
#define S48_SET_VALUE(x, type, v) (S48_EXTRACT_VALUE((x), type) = (v))

#define S48_UNSAFE_EXTRACT_VALUE_POINTER(x, type)		\
  (S48_ADDRESS_AFTER_HEADER((x), type))
#define S48_UNSAFE_EXTRACT_VALUE(x, type)			\
  (*(S48_UNSAFE_EXTRACT_VALUE_POINTER((x), type)))
#define S48_UNSAFE_SET_VALUE(x, type, v)			\
  (S48_UNSAFE_EXTRACT_VALUE((x), type) = (v))

#define S48_UNSAFE_EXTRACT_DOUBLE(x)				\
  (*(S48_ADDRESS_AFTER_HEADER((x), double)))

#define S48_ARG_REF(argv, index, argc) ((argv)[(argc)-(index)-1])

#define S48_DECLARE_GC_PROTECT(n) long ___gc_buffer[(n)+2]

#define S48_GC_PROTECT_1(v) \
  (___gc_buffer[2]=(long)&(v), \
   s48_push_gc_rootsB((char *) ___gc_buffer, 1))

#define S48_GC_PROTECT_2(v1, v2) \
  (___gc_buffer[2]=(long)&(v1), ___gc_buffer[3]=(long)&(v2), \
   s48_push_gc_rootsB((char *) ___gc_buffer, 2))

#define S48_GC_PROTECT_3(v1, v2, v3) \
  (___gc_buffer[2]=(long)&(v1), \
   ___gc_buffer[3]=(long)&(v2), \
   ___gc_buffer[4]=(long)&(v3), \
   s48_push_gc_rootsB((char *) ___gc_buffer, 3))

#define S48_GC_PROTECT_4(v1, v2, v3, v4) \
  (___gc_buffer[2]=(long)&(v1), \
   ___gc_buffer[3]=(long)&(v2), \
   ___gc_buffer[4]=(long)&(v3), \
   ___gc_buffer[5]=(long)&(v4), \
   s48_push_gc_rootsB((char *) ___gc_buffer, 4))

#define S48_GC_PROTECT_5(v1, v2, v3, v4, v5) \
  (___gc_buffer[2]=(long)&(v1), \
   ___gc_buffer[3]=(long)&(v2), \
   ___gc_buffer[4]=(long)&(v3), \
   ___gc_buffer[5]=(long)&(v4), \
   ___gc_buffer[6]=(long)&(v5), \
   s48_push_gc_rootsB((char *) ___gc_buffer, 5))

#define S48_GC_PROTECT_6(v1, v2, v3, v4, v5, v6) \
  (___gc_buffer[2]=(long)&(v1), \
   ___gc_buffer[3]=(long)&(v2), \
   ___gc_buffer[4]=(long)&(v3), \
   ___gc_buffer[5]=(long)&(v4), \
   ___gc_buffer[6]=(long)&(v5), \
   ___gc_buffer[7]=(long)&(v6), \
   s48_push_gc_rootsB((char *) ___gc_buffer, 6))

#define S48_GC_PROTECT_7(v1, v2, v3, v4, v5, v6, v7) \
  (___gc_buffer[2]=(long)&(v1), \
   ___gc_buffer[3]=(long)&(v2), \
   ___gc_buffer[4]=(long)&(v3), \
   ___gc_buffer[5]=(long)&(v4), \
   ___gc_buffer[6]=(long)&(v5), \
   ___gc_buffer[7]=(long)&(v6), \
   ___gc_buffer[8]=(long)&(v7), \
   s48_push_gc_rootsB((char *) ___gc_buffer, 7))

#define S48_GC_PROTECT_8(v1, v2, v3, v4, v5, v6, v7, v8) \
  (___gc_buffer[2]=(long)&(v1), \
   ___gc_buffer[3]=(long)&(v2), \
   ___gc_buffer[4]=(long)&(v3), \
   ___gc_buffer[5]=(long)&(v4), \
   ___gc_buffer[6]=(long)&(v5), \
   ___gc_buffer[7]=(long)&(v6), \
   ___gc_buffer[8]=(long)&(v7), \
   ___gc_buffer[9]=(long)&(v8), \
   s48_push_gc_rootsB((char *) ___gc_buffer, 8))

#define S48_GC_PROTECT_9(v1, v2, v3, v4, v5, v6, v7, v8, v9) \
  (___gc_buffer[2]=(long)&(v1), \
   ___gc_buffer[3]=(long)&(v2), \
   ___gc_buffer[4]=(long)&(v3), \
   ___gc_buffer[5]=(long)&(v4), \
   ___gc_buffer[6]=(long)&(v5), \
   ___gc_buffer[7]=(long)&(v6), \
   ___gc_buffer[8]=(long)&(v7), \
   ___gc_buffer[9]=(long)&(v8), \
   ___gc_buffer[10]=(long)&(v9), \
   s48_push_gc_rootsB((char *) ___gc_buffer, 9))

#define S48_GC_PROTECT_10(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) \
  (___gc_buffer[2]=(long)&(v1), \
   ___gc_buffer[3]=(long)&(v2), \
   ___gc_buffer[4]=(long)&(v3), \
   ___gc_buffer[5]=(long)&(v4), \
   ___gc_buffer[6]=(long)&(v5), \
   ___gc_buffer[7]=(long)&(v6), \
   ___gc_buffer[8]=(long)&(v7), \
   ___gc_buffer[9]=(long)&(v8), \
   ___gc_buffer[10]=(long)&(v9), \
   ___gc_buffer[11]=(long)&(v10), \
   s48_push_gc_rootsB((char *) ___gc_buffer, 10))

#define S48_GC_UNPROTECT()				\
   do { if (! s48_pop_gc_rootsB())			\
       	  s48_raise_scheme_exception( S48_EXCEPTION_GC_PROTECTION_MISMATCH, 0); \
      } while(0)

#define S48_GC_PROTECT_GLOBAL(v) ((void*)(s48_register_gc_rootB((char *)&(v))))
#define S48_GC_UNPROTECT_GLOBAL(f) (s48_unregister_gc_rootB((char *)(f)))

/* Exceptions */

S48_EXTERN void s48_error(const char* who, const char* message,
			  long irritant_count, ...);
S48_EXTERN void s48_assertion_violation(const char* who, const char* message,
					long irritant_count, ...);
S48_EXTERN void s48_os_error(const char* who, int the_errno,
			     long irritant_count, ...);
S48_EXTERN void s48_out_of_memory_error();

/* The following are deprecated */

S48_EXTERN void s48_raise_argument_type_error(s48_value value);
S48_EXTERN void s48_raise_argument_number_error(s48_value value,
						s48_value min,
						s48_value max);
S48_EXTERN void s48_raise_range_error(s48_value value,
				      s48_value min,
				      s48_value max);
S48_EXTERN void s48_raise_closed_channel_error();
S48_EXTERN void s48_raise_os_error(int the_errno);
S48_EXTERN void s48_raise_string_os_error(char *reason);
S48_EXTERN void s48_raise_out_of_memory_error();

/* Internal use */

S48_EXTERN void s48_raise_scheme_exception(long type, long nargs, ...);

/* Type checking */

#define S48_CHECK_PAIR(v) do { if (!S48_PAIR_P(v)) s48_assertion_violation(NULL, "must be a pair", 1, v); } while (0)
#define S48_CHECK_FIXNUM(v) do { if (!S48_FIXNUM_P(v)) s48_assertion_violation(NULL, "must be a fixnum", 1, v); } while (0)
#define S48_CHECK_STRING(v) do { if (!S48_STRING_P(v)) s48_assertion_violation(NULL, "must be a string", 1, v); } while (0)
#define S48_CHECK_BYTE_VECTOR(v) do { if (!S48_BYTE_VECTOR_P(v)) s48_assertion_violation(NULL, "must be a bytevector", 1, v); } while (0)
#define S48_CHECK_CHANNEL(v) do { if (!S48_CHANNEL_P(v)) s48_assertion_violation(NULL, "must be a channel", 1, v); } while (0)
#define S48_CHECK_RECORD(v) do { if (!S48_RECORD_P(v)) s48_assertion_violation(NULL, "must be a record", 1, v); } while (0)
#define S48_CHECK_VALUE(v) do { if (!S48_BYTE_VECTOR_P(v)) s48_assertion_violation(NULL, "must be an external value", 1, v); } while (0)
#define S48_CHECK_EXPORT_BINDING(v) do { if (!S48_EXPORT_BINDING_P(v)) s48_assertion_violation(NULL, "must be an exported value", 1, v ); } while (0)
#define S48_CHECK_BOOLEAN(v)					\
  do { s48_value s48_temp = (v);				\
       if (s48_temp != S48_TRUE && s48_temp != S48_FALSE)	\
	 s48_assertion_violation(NULL, "must be a boolean", 1, v); } while (0)

#define S48_VALUE_P(v) (S48_BYTE_VECTOR_P(v))

#define S48_TRUE_P(v) ((v) == S48_TRUE)
#define S48_FALSE_P(v) ((v) == S48_FALSE)
#define S48_NULL_P(v) ((v) == S48_NULL)
#define S48_EXTRACT_BOOLEAN(v) ((v) != S48_FALSE)
#define S48_ENTER_BOOLEAN(v) ((v) ? S48_TRUE : S48_FALSE)

#define S48_SHARED_BINDING_CHECK(binding)					\
  do { if (S48_UNSPECIFIC == S48_SHARED_BINDING_REF(binding))			\
         s48_raise_scheme_exception(S48_EXCEPTION_UNBOUND_EXTERNAL_NAME, 1,	\
				    S48_SHARED_BINDING_NAME(binding));		\
  } while(0)

#endif /* !NO_OLD_FFI */



/* both */

S48_EXTERN s48_value	s48_define_exported_binding(char *, s48_value);
S48_EXTERN s48_value	s48_enter_pointer(void *);

#define S48_EXPORT_FUNCTION(p) (s48_define_exported_binding(#p, s48_enter_pointer((void*) p)))
#define s48_export_function(p) S48_EXPORT_FUNCTION(p)

S48_EXTERN void		s48_note_external_event(long);


/* New FFI */

#define s48_fixnum_tag 0
#define s48_fixnum_p_2(c,x) (((long)s48_deref(x) & 3L) == s48_fixnum_tag)
#define s48_immediate_tag 1
#define s48_immediate_p_2(c,x) (((long)s48_deref(x) & 3L) == s48_immediate_tag)
#define s48_header_tag 2
#define s48_header_p_2(c,x) (((long)s48_deref(x) & 3L) == s48_header_tag)
#define s48_stob_tag 3
#define s48_stob_p_2(c,x) (((long)s48_deref(x) & 3L) == s48_stob_tag)
#if defined(S48_GC_BIBOP) && defined(NO_OLD_FFI)
#define S48_STOB_P(x) (((long)(x) & 3L) == s48_stob_tag)
#endif

#define s48_unsafe_enter_long_as_fixnum_2(c, n)   (s48_make_local_ref(c,(s48_value)((n) << 2)))
#define s48_unsafe_extract_long_2(c, x) ((long)s48_deref(x) >> 2)

#define MISC_IMMEDIATE_INTERNAL_2(n) (s48_immediate_tag | ((n) << 2))
#define _s48_value_false    MISC_IMMEDIATE_INTERNAL_2(0)
#define s48_false_2(c)   s48_make_local_ref(c, _s48_value_false)
#define _s48_value_true    MISC_IMMEDIATE_INTERNAL_2(1)
#define s48_true_2(c)   s48_make_local_ref(c, _s48_value_true)
#define _s48_value_char    MISC_IMMEDIATE_INTERNAL_2(2)
#define s48_char_2(c)   s48_make_local_ref(c, _s48_value_char)
#define _s48_value_unspecific    MISC_IMMEDIATE_INTERNAL_2(3)
#define s48_unspecific_2(c)   s48_make_local_ref(c, _s48_value_unspecific)
#define _s48_value_undefined    MISC_IMMEDIATE_INTERNAL_2(4)
#define s48_undefined_2(c)   s48_make_local_ref(c, _s48_value_undefined)
#define _s48_value_eof    MISC_IMMEDIATE_INTERNAL_2(5)
#define s48_eof_2(c)   s48_make_local_ref(c, _s48_value_eof)
#define _s48_value_null    MISC_IMMEDIATE_INTERNAL_2(6)
#define s48_null_2(c)   s48_make_local_ref(c, _s48_value_null)
#define _s48_value_unreleased    MISC_IMMEDIATE_INTERNAL_2(7)
#define s48_unreleased_2(c)   s48_make_local_ref(c, _s48_value_unreleased)

#define s48_unsafe_enter_char_2(call, c) s48_make_local_ref (call, _s48_value_char | ((c) << 8))
#define s48_unsafe_extract_char_2(c,x) ((long)(s48_deref(x) >> 8))
#define s48_char_p_2(c, x) ((((long) s48_deref(x)) & 0xff) == _s48_value_char)

#define ADDRESS_AFTER_HEADER_INTERNAL_2(x, type) ((type *)((x) - s48_stob_tag))
#define STOB_REF_INTERNAL_2(x, i) ADDRESS_AFTER_HEADER_INTERNAL_2(x, s48_value)[i]
#define STOB_BYTE_REF_INTERNAL_2(x, i) (((char *) ADDRESS_AFTER_HEADER_INTERNAL_2(x, s48_value))[i])
#define s48_address_after_header_2(c, x, type) ADDRESS_AFTER_HEADER_INTERNAL_2(s48_deref(x), type)
#define s48_unsafe_stob_ref_2(c, x, i) s48_make_local_ref(c, (STOB_REF_INTERNAL_2(s48_deref(x), i)))
#define s48_unsafe_stob_byte_ref_2(c, x, i) STOB_BYTE_REF_INTERNAL_2(s48_deref(x), i)
#define s48_unsafe_stob_set_2(c, x, i, r) do { s48_ref_t __stob_set_x_ref = (x); s48_ref_t __stob_set_r_ref = (r); long __stob_set_i = (i); s48_value __stob_set_x = s48_deref(__stob_set_x_ref); s48_value __stob_set_v = s48_deref(__stob_set_r_ref); if (s48_stob_immutablep_2(c, (x))) s48_assertion_violation_2(c, NULL, "immutable stob", 1, __stob_set_x); else { S48_WRITE_BARRIER((__stob_set_x), (char *) (&(STOB_REF_INTERNAL_2((__stob_set_x), (__stob_set_i)))),(__stob_set_v)); *(&STOB_REF_INTERNAL_2((__stob_set_x), (__stob_set_i))) = (__stob_set_v); } } while (0)
#define s48_unsafe_stob_byte_set_2(c, x, i, v) do { long __stob_set_i = (i); char __stob_set_v = (v); s48_value __stob_set_x = s48_deref(x); if (s48_stob_immutablep_2(c, (x))) s48_assertion_violation(NULL, "immutable stob", 1, __stob_set_x); else *(&STOB_BYTE_REF_INTERNAL_2((__stob_set_x), (__stob_set_i))) = (__stob_set_v); } while (0)
#define s48_stob_header_2(c, x) (STOB_REF_INTERNAL_2(s48_deref(x), -1))
#define s48_stob_type_2(c, x)   ((s48_stob_header_2(c, x)>>2)&31)
#define s48_stob_address_2(c, x) (&(s48_stob_header_2(c, x)))
#define s48_unsafe_stob_byte_length_2(c, x) (s48_stob_header_2(c, x) >> 8)
#define s48_unsafe_stob_descriptor_length_2(c, x) (s48_unsafe_stob_byte_length_2(c, x) >> S48_LOG_BYTES_PER_CELL)
#define s48_stob_immutablep_2(c, x) ((s48_stob_header_2(c, x)>>7) & 1)
#define s48_stob_make_immutable_2(c, x) ((s48_stob_header_2(c, x)) |= (1<<7))

#define s48_stobtype_pair 0
#define s48_pair_p_2(c, x) (s48_stob_has_type_2(c, x, 0))
#define s48_stobtype_symbol 1
#define s48_symbol_p_2(c, x) (s48_stob_has_type_2(c, x, 1))
#define s48_stobtype_vector 2
#define s48_vector_p_2(c, x) (s48_stob_has_type_2(c, x, 2))
#define s48_stobtype_closure 3
#define s48_closure_p_2(c, x) (s48_stob_has_type_2(c, x, 3))
#define s48_stobtype_location 4
#define s48_location_p_2(c, x) (s48_stob_has_type_2(c, x, 4))
#define s48_stobtype_cell 5
#define s48_cell_p_2(c, x) (s48_stob_has_type_2(c, x, 5))
#define s48_stobtype_channel 6
#define s48_channel_p_2(c, x) (s48_stob_has_type_2(c, x, 6))
#define s48_stobtype_port 7
#define s48_port_p_2(c, x) (s48_stob_has_type_2(c, x, 7))
#define s48_stobtype_ratnum 8
#define s48_ratnum_p_2(c, x) (s48_stob_has_type_2(c, x, 8))
#define s48_stobtype_record 9
#define s48_record_p_2(c, x) (s48_stob_has_type_2(c, x, 9))
#define s48_stobtype_continuation 10
#define s48_continuation_p_2(c, x) (s48_stob_has_type_2(c, x, 10))
#define s48_stobtype_extended_number 11
#define s48_extended_number_p_2(c, x) (s48_stob_has_type_2(c, x, 11))
#define s48_stobtype_template 12
#define s48_template_p_2(c, x) (s48_stob_has_type_2(c, x, 12))
#define s48_stobtype_weak_pointer 13
#define s48_weak_pointer_p_2(c, x) (s48_stob_has_type_2(c, x, 13))
#define s48_stobtype_shared_binding 14
#define s48_shared_binding_p_2(c, x) (s48_stob_has_type_2(c, x, 14))
#define s48_stobtype_transport_link_cell 15
#define s48_transport_link_cell_p_2(c, x) (s48_stob_has_type_2(c, x, 15))
#define s48_stobtype_string 16
#define s48_string_p_2(c, x) (s48_stob_has_type_2(c, x, 16))
#define s48_stobtype_byte_vector 17
#define s48_byte_vector_p_2(c, x) (s48_stob_has_type_2(c, x, 17))
#define s48_stobtype_double 18
#define s48_double_p_2(c, x) (s48_stob_has_type_2(c, x, 18))
#define s48_stobtype_bignum 19
#define s48_bignum_p_2(c, x) (s48_stob_has_type_2(c, x, 19))

#define s48_car_offset 0
#define s48_car_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_pair, 0))
#define s48_unsafe_car_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 0))
#define s48_set_car_2(c, x, r) (s48_stob_set_2(c, (x), s48_stobtype_pair, 0, (r)))
#define s48_unsafe_set_car_2(c, x, r) s48_unsafe_stob_set_2(c, (x), 0, (r))
#define s48_cdr_offset 1
#define s48_cdr_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_pair, 1))
#define s48_unsafe_cdr_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 1))
#define s48_set_cdr_2(c, x, r) (s48_stob_set_2(c, (x), s48_stobtype_pair, 1, (r)))
#define s48_unsafe_set_cdr_2(c, x, r) s48_unsafe_stob_set_2(c, (x), 1, (r))
#define s48_symbol_to_string_offset 0
#define s48_symbol_to_string_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_symbol, 0))
#define s48_unsafe_symbol_to_string_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 0))
#define s48_location_id_offset 0
#define s48_location_id_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_location, 0))
#define s48_unsafe_location_id_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 0))
#define s48_set_location_id_2(c, x, r) (s48_stob_set_2(c, (x), s48_stobtype_location, 0, (r)))
#define s48_unsafe_set_location_id_2(c, x, r) s48_unsafe_stob_set_2(c, (x), 0, (r))
#define s48_contents_offset 1
#define s48_contents_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_location, 1))
#define s48_unsafe_contents_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 1))
#define s48_set_contents_2(c, x, r) (s48_stob_set_2(c, (x), s48_stobtype_location, 1, (r)))
#define s48_unsafe_set_contents_2(c, x, r) s48_unsafe_stob_set_2(c, (x), 1, (r))
#define s48_cell_ref_offset 0
#define s48_cell_ref_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_cell, 0))
#define s48_unsafe_cell_ref_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 0))
#define s48_cell_set_2(c, x, r) (s48_stob_set_2(c, (x), s48_stobtype_cell, 0, (r)))
#define s48_unsafe_cell_set_2(c, x, r) s48_unsafe_stob_set_2(c, (x), 0, (r))
#define s48_closure_template_offset 0
#define s48_closure_template_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_closure, 0))
#define s48_unsafe_closure_template_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 0))
#define s48_set_closure_template_2(c, x, r) (s48_stob_set_2(c, (x), s48_stobtype_closure, 0, (r)))
#define s48_unsafe_set_closure_template_2(c, x, r) s48_unsafe_stob_set_2(c, (x), 0, (r))
#define s48_closure_env_offset 1
#define s48_closure_env_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_closure, 1))
#define s48_unsafe_closure_env_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 1))
#define s48_set_closure_env_2(c, x, r) (s48_stob_set_2(c, (x), s48_stobtype_closure, 1, (r)))
#define s48_unsafe_set_closure_env_2(c, x, r) s48_unsafe_stob_set_2(c, (x), 1, (r))
#define s48_weak_pointer_ref_offset 0
#define s48_weak_pointer_ref_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_weak_pointer, 0))
#define s48_unsafe_weak_pointer_ref_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 0))
#define s48_shared_binding_name_offset 0
#define s48_shared_binding_name_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_shared_binding, 0))
#define s48_unsafe_shared_binding_name_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 0))
#define s48_shared_binding_is_importp_offset 1
#define s48_shared_binding_is_importp_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_shared_binding, 1))
#define s48_unsafe_shared_binding_is_importp_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 1))
#define s48_shared_binding_ref_offset 2
#define s48_shared_binding_ref_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_shared_binding, 2))
#define s48_unsafe_shared_binding_ref_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 2))
#define s48_shared_binding_set_2(c, x, r) (s48_stob_set_2(c, (x), s48_stobtype_shared_binding, 2, (r)))
#define s48_unsafe_shared_binding_set_2(c, x, r) s48_unsafe_stob_set_2(c, (x), 2, (r))
#define s48_port_handler_offset 0
#define s48_port_handler_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_port, 0))
#define s48_unsafe_port_handler_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 0))
#define s48_port_text_codec_spec_offset 1
#define s48_port_text_codec_spec_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_port, 1))
#define s48_unsafe_port_text_codec_spec_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 1))
#define s48_set_port_text_codec_spec_2(c, x, r) (s48_stob_set_2(c, (x), s48_stobtype_port, 1, (r)))
#define s48_unsafe_set_port_text_codec_spec_2(c, x, r) s48_unsafe_stob_set_2(c, (x), 1, (r))
#define s48_port_crlfp_offset 2
#define s48_port_crlfp_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_port, 2))
#define s48_unsafe_port_crlfp_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 2))
#define s48_set_port_crlfp_2(c, x, r) (s48_stob_set_2(c, (x), s48_stobtype_port, 2, (r)))
#define s48_unsafe_set_port_crlfp_2(c, x, r) s48_unsafe_stob_set_2(c, (x), 2, (r))
#define s48_port_status_offset 3
#define s48_port_status_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_port, 3))
#define s48_unsafe_port_status_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 3))
#define s48_set_port_status_2(c, x, r) (s48_stob_set_2(c, (x), s48_stobtype_port, 3, (r)))
#define s48_unsafe_set_port_status_2(c, x, r) s48_unsafe_stob_set_2(c, (x), 3, (r))
#define s48_port_lock_offset 4
#define s48_port_lock_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_port, 4))
#define s48_unsafe_port_lock_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 4))
#define s48_set_port_lock_2(c, x, r) (s48_stob_set_2(c, (x), s48_stobtype_port, 4, (r)))
#define s48_unsafe_set_port_lock_2(c, x, r) s48_unsafe_stob_set_2(c, (x), 4, (r))
#define s48_port_data_offset 5
#define s48_port_data_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_port, 5))
#define s48_unsafe_port_data_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 5))
#define s48_set_port_data_2(c, x, r) (s48_stob_set_2(c, (x), s48_stobtype_port, 5, (r)))
#define s48_unsafe_set_port_data_2(c, x, r) s48_unsafe_stob_set_2(c, (x), 5, (r))
#define s48_port_buffer_offset 6
#define s48_port_buffer_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_port, 6))
#define s48_unsafe_port_buffer_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 6))
#define s48_set_port_buffer_2(c, x, r) (s48_stob_set_2(c, (x), s48_stobtype_port, 6, (r)))
#define s48_unsafe_set_port_buffer_2(c, x, r) s48_unsafe_stob_set_2(c, (x), 6, (r))
#define s48_port_index_offset 7
#define s48_port_index_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_port, 7))
#define s48_unsafe_port_index_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 7))
#define s48_set_port_index_2(c, x, r) (s48_stob_set_2(c, (x), s48_stobtype_port, 7, (r)))
#define s48_unsafe_set_port_index_2(c, x, r) s48_unsafe_stob_set_2(c, (x), 7, (r))
#define s48_port_limit_offset 8
#define s48_port_limit_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_port, 8))
#define s48_unsafe_port_limit_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 8))
#define s48_set_port_limit_2(c, x, r) (s48_stob_set_2(c, (x), s48_stobtype_port, 8, (r)))
#define s48_unsafe_set_port_limit_2(c, x, r) s48_unsafe_stob_set_2(c, (x), 8, (r))
#define s48_port_pending_crp_offset 9
#define s48_port_pending_crp_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_port, 9))
#define s48_unsafe_port_pending_crp_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 9))
#define s48_set_port_pending_crp_2(c, x, r) (s48_stob_set_2(c, (x), s48_stobtype_port, 9, (r)))
#define s48_unsafe_set_port_pending_crp_2(c, x, r) s48_unsafe_stob_set_2(c, (x), 9, (r))
#define s48_port_pending_eofp_offset 10
#define s48_port_pending_eofp_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_port, 10))
#define s48_unsafe_port_pending_eofp_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 10))
#define s48_set_port_pending_eofp_2(c, x, r) (s48_stob_set_2(c, (x), s48_stobtype_port, 10, (r)))
#define s48_unsafe_set_port_pending_eofp_2(c, x, r) s48_unsafe_stob_set_2(c, (x), 10, (r))
#define s48_channel_status_offset 0
#define s48_channel_status_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_channel, 0))
#define s48_unsafe_channel_status_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 0))
#define s48_channel_id_offset 1
#define s48_channel_id_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_channel, 1))
#define s48_unsafe_channel_id_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 1))
#define s48_channel_os_index_offset 2
#define s48_channel_os_index_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_channel, 2))
#define s48_unsafe_channel_os_index_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 2))
#define s48_channel_close_silentlyp_offset 3
#define s48_channel_close_silentlyp_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_channel, 3))
#define s48_unsafe_channel_close_silentlyp_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 3))
#define s48_transport_link_cell_key_offset 0
#define s48_transport_link_cell_key_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_transport_link_cell, 0))
#define s48_unsafe_transport_link_cell_key_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 0))
#define s48_transport_link_cell_value_offset 1
#define s48_transport_link_cell_value_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_transport_link_cell, 1))
#define s48_unsafe_transport_link_cell_value_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 1))
#define s48_set_transport_link_cell_value_2(c, x, r) (s48_stob_set_2(c, (x), s48_stobtype_transport_link_cell, 1, (r)))
#define s48_unsafe_set_transport_link_cell_value_2(c, x, r) s48_unsafe_stob_set_2(c, (x), 1, (r))
#define s48_transport_link_cell_tconc_offset 2
#define s48_transport_link_cell_tconc_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_transport_link_cell, 2))
#define s48_unsafe_transport_link_cell_tconc_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 2))
#define s48_set_transport_link_cell_tconc_2(c, x, r) (s48_stob_set_2(c, (x), s48_stobtype_transport_link_cell, 2, (r)))
#define s48_unsafe_set_transport_link_cell_tconc_2(c, x, r) s48_unsafe_stob_set_2(c, (x), 2, (r))
#define s48_transport_link_cell_next_offset 3
#define s48_transport_link_cell_next_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_transport_link_cell, 3))
#define s48_unsafe_transport_link_cell_next_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 3))
#define s48_set_transport_link_cell_next_2(c, x, r) (s48_stob_set_2(c, (x), s48_stobtype_transport_link_cell, 3, (r)))
#define s48_unsafe_set_transport_link_cell_next_2(c, x, r) s48_unsafe_stob_set_2(c, (x), 3, (r))

#define s48_vector_length_2(c, x) (s48_stob_length_2(c, (x), s48_stobtype_vector))
#define s48_unsafe_vector_length_2(c, x) (s48_unsafe_stob_descriptor_length_2(c, x))
#define s48_unsafe_vector_ref_2(c, x, i) (s48_unsafe_stob_ref_2(c, (x), (i)))
#define s48_unsafe_vector_set_2(c, x, i, r) s48_unsafe_stob_set_2(c, (x), (i), (r))
#define s48_vector_ref_2(c, x, i) (s48_stob_ref_2(c, (x), s48_stobtype_vector, (i)))
#define s48_vector_set_2(c, x, i, r) s48_stob_set_2(c, (x), s48_stobtype_vector, (i), (r))
#define s48_record_length_2(c, x) (s48_stob_length_2(c, (x), s48_stobtype_record))
#define s48_unsafe_record_length_2(c, x) (s48_unsafe_stob_descriptor_length_2(c, x))
#define s48_unsafe_record_ref_2(c, x, i) (s48_unsafe_stob_ref_2(c, (x), (i) + 1))
#define s48_unsafe_record_set_2(c, x, i, r) s48_unsafe_stob_set_2(c, (x), (i) + 1, (r))
#define s48_record_ref_2(c, x, i) (s48_stob_ref_2(c, (x), s48_stobtype_record, (i) + 1))
#define s48_record_set_2(c, x, i, r) s48_stob_set_2(c, (x), s48_stobtype_record, (i) + 1, (r))
#define s48_record_type_2(c, x) (s48_stob_ref_2(c, (x), s48_stobtype_record, 0))
#define s48_unsafe_record_type_2(c, x) (s48_unsafe_stob_ref_2(c, (x), 0))
#define s48_byte_vector_length_2(c, x) (s48_stob_byte_length_2(c, (x), s48_stobtype_byte_vector))
#define s48_byte_vector_ref_2(c, x, i) (s48_stob_byte_ref_2(c, (x), s48_stobtype_byte_vector, (i)))
#define s48_byte_vector_set_2(c, x, i, v) (s48_stob_byte_set_2(c, (x), s48_stobtype_byte_vector, (i), (v)))
#define s48_unsafe_byte_vector_length_2(c, x) (s48_unsafe_stob_byte_length_2(c, (x), s48_stobtype_byte_vector))
#define s48_unsafe_byte_vector_ref_2(c, x, i) (s48_stob_byte_ref_2(c, (x), s48_stobtype_byte_vector, (i)))
#define s48_unsafe_byte_vector_set_2(c, x, i, v) (s48_stob_byte_set_2(c, (x), s48_stobtype_byte_vector, (i), (v)))
#define s48_unsafe_extract_byte_vector_2(c, x) (s48_address_after_header_2(c, (x), char))
#define s48_extract_external_object_2(c, x, type) ((type *)(s48_address_after_header_2(c, x, long)+1))

#define s48_record_type_resumer_2(c, x) s48_unsafe_record_ref_2(c, (x), 0)
#define s48_record_type_uid_2(c, x) s48_unsafe_record_ref_2(c, (x), 1)
#define s48_record_type_name_2(c, x) s48_unsafe_record_ref_2(c, (x), 2)
#define s48_record_type_field_names_2(c, x) s48_unsafe_record_ref_2(c, (x), 3)
#define s48_record_type_number_of_fields_2(c, x) s48_unsafe_record_ref_2(c, (x), 4)
#define s48_record_type_discloser_2(c, x) s48_unsafe_record_ref_2(c, (x), 5)
#define s48_record_type_parent_2(c, x) s48_unsafe_record_ref_2(c, (x), 6)
#define s48_record_type_extension_count_2(c, x) s48_unsafe_record_ref_2(c, (x), 7)
#define s48_record_type_size_2(c, x) s48_unsafe_record_ref_2(c, (x), 8)
#define s48_record_type_data_2(c, x) s48_unsafe_record_ref_2(c, (x), 9)
#define s48_record_type_base_2(c, x) s48_unsafe_record_ref_2(c, (x), 10)

#define s48_exception_unassigned_local 0
#define s48_exception_undefined_global 1
#define s48_exception_unbound_global 2
#define s48_exception_bad_procedure 3
#define s48_exception_wrong_number_of_arguments 4
#define s48_exception_wrong_type_argument 5
#define s48_exception_immutable_argument 6
#define s48_exception_arithmetic_overflow 7
#define s48_exception_index_out_of_range 8
#define s48_exception_heap_overflow 9
#define s48_exception_out_of_memory 10
#define s48_exception_cannot_open_channel 11
#define s48_exception_channel_os_index_already_in_use 12
#define s48_exception_closed_channel 13
#define s48_exception_buffer_fullempty 14
#define s48_exception_unimplemented_instruction 15
#define s48_exception_trap 16
#define s48_exception_proceeding_after_exception 17
#define s48_exception_bad_option 18
#define s48_exception_unbound_external_name 19
#define s48_exception_too_many_arguments_to_external_procedure 20
#define s48_exception_too_many_arguments_in_callback 21
#define s48_exception_callback_return_uncovered 22
#define s48_exception_extension_exception 23
#define s48_exception_extension_return_error 24
#define s48_exception_os_error 25
#define s48_exception_gc_protection_mismatch 26
#define s48_exception_no_current_proposal 27
#define s48_exception_native_code_not_supported 28
#define s48_exception_illegal_exception_return 29
#define s48_exception_external_error 30
#define s48_exception_external_assertion_violation 31
#define s48_exception_external_os_error 32

#define s48_channel_status_closed_2(c) s48_unsafe_enter_long_as_fixnum_2(c, 0)
#define s48_channel_status_input_2(c) s48_unsafe_enter_long_as_fixnum_2(c, 1)
#define s48_channel_status_output_2(c) s48_unsafe_enter_long_as_fixnum_2(c, 2)
#define s48_channel_status_special_input_2(c) s48_unsafe_enter_long_as_fixnum_2(c, 3)
#define s48_channel_status_special_output_2(c) s48_unsafe_enter_long_as_fixnum_2(c, 4)

#ifndef NO_OLD_FFI

#define S48_FIXNUM_TAG 0
#define S48_FIXNUM_P(x) (((long)(x) & 3L) == S48_FIXNUM_TAG)
#define S48_IMMEDIATE_TAG 1
#define S48_IMMEDIATE_P(x) (((long)(x) & 3L) == S48_IMMEDIATE_TAG)
#define S48_HEADER_TAG 2
#define S48_HEADER_P(x) (((long)(x) & 3L) == S48_HEADER_TAG)
#define S48_STOB_TAG 3
#define S48_STOB_P(x) (((long)(x) & 3L) == S48_STOB_TAG)

#define S48_UNSAFE_ENTER_FIXNUM(n)   ((s48_value)((n) << 2))
#define S48_UNSAFE_EXTRACT_FIXNUM(x) ((long)(x) >> 2)

#define S48_MISC_IMMEDIATE(n) (S48_IMMEDIATE_TAG | ((n) << 2))
#define S48_FALSE    (S48_MISC_IMMEDIATE(0))
#define S48_TRUE    (S48_MISC_IMMEDIATE(1))
#define S48_CHAR    (S48_MISC_IMMEDIATE(2))
#define S48_UNSPECIFIC    (S48_MISC_IMMEDIATE(3))
#define S48_UNDEFINED    (S48_MISC_IMMEDIATE(4))
#define S48_EOF    (S48_MISC_IMMEDIATE(5))
#define S48_NULL    (S48_MISC_IMMEDIATE(6))
#define S48_UNRELEASED    (S48_MISC_IMMEDIATE(7))

#define S48_UNSAFE_ENTER_CHAR(c) (S48_CHAR | ((c) << 8))
#define S48_UNSAFE_EXTRACT_CHAR(x) ((long)((x) >> 8))
#define S48_CHAR_P(x) ((((long) (x)) & 0xff) == S48_CHAR)

#define ADDRESS_AFTER_HEADER_INTERNAL(x, type) ((type *)((x) - S48_STOB_TAG))
#define STOB_REF_INTERNAL(x, i) ADDRESS_AFTER_HEADER_INTERNAL(x, s48_value)[i]
#define STOB_BYTE_REF_INTERNAL(x, i) (((char *) ADDRESS_AFTER_HEADER_INTERNAL(x, s48_value))[i])
#define S48_ADDRESS_AFTER_HEADER(x, type) ADDRESS_AFTER_HEADER_INTERNAL(x, type)
#define S48_STOB_REF(x, i) STOB_REF_INTERNAL((x), i)
#define S48_STOB_BYTE_REF(x, i) STOB_BYTE_REF_INTERNAL((x), i)
#define S48_STOB_SET(x, i, v) do { s48_value __stob_set_x = (x); long __stob_set_i = (i); s48_value __stob_set_v = (v); if (S48_STOB_IMMUTABLEP(__stob_set_x)) s48_assertion_violation(NULL, "immutable stob", 1, __stob_set_x); else { S48_WRITE_BARRIER((__stob_set_x), (char *) (&S48_STOB_REF((__stob_set_x), (__stob_set_i))),(__stob_set_v)); *(&S48_STOB_REF((__stob_set_x), (__stob_set_i))) = (__stob_set_v); } } while (0)
#define S48_STOB_BYTE_SET(x, i, v) do { s48_value __stob_set_x = (x); long __stob_set_i = (i); char __stob_set_v = (v); if (S48_STOB_IMMUTABLEP(__stob_set_x)) s48_assertion_violation(NULL, "immutable stob", 1, __stob_set_x); else *(&S48_STOB_BYTE_REF((__stob_set_x), (__stob_set_i))) = (__stob_set_v); } while (0)
#define S48_STOB_HEADER(x) (S48_STOB_REF((x),-1))
#define S48_STOB_TYPE(x)   ((S48_STOB_HEADER(x)>>2)&31)
#define S48_STOB_ADDRESS(x) (&(S48_STOB_HEADER(x)))
#define S48_STOB_BYTE_LENGTH(x) ((unsigned long)S48_STOB_HEADER(x) >> 8)
#define S48_STOB_DESCRIPTOR_LENGTH(x) (S48_STOB_BYTE_LENGTH(x) >> S48_LOG_BYTES_PER_CELL)
#define S48_STOB_IMMUTABLEP(x) ((S48_STOB_HEADER(x)>>7) & 1)
#define S48_STOB_MAKE_IMMUTABLE(x) ((S48_STOB_HEADER(x)) |= (1<<7))

#define S48_STOBTYPE_PAIR 0
#define S48_PAIR_P(x) (s48_stob_has_type(x, 0))
#define S48_STOBTYPE_SYMBOL 1
#define S48_SYMBOL_P(x) (s48_stob_has_type(x, 1))
#define S48_STOBTYPE_VECTOR 2
#define S48_VECTOR_P(x) (s48_stob_has_type(x, 2))
#define S48_STOBTYPE_CLOSURE 3
#define S48_CLOSURE_P(x) (s48_stob_has_type(x, 3))
#define S48_STOBTYPE_LOCATION 4
#define S48_LOCATION_P(x) (s48_stob_has_type(x, 4))
#define S48_STOBTYPE_CELL 5
#define S48_CELL_P(x) (s48_stob_has_type(x, 5))
#define S48_STOBTYPE_CHANNEL 6
#define S48_CHANNEL_P(x) (s48_stob_has_type(x, 6))
#define S48_STOBTYPE_PORT 7
#define S48_PORT_P(x) (s48_stob_has_type(x, 7))
#define S48_STOBTYPE_RATNUM 8
#define S48_RATNUM_P(x) (s48_stob_has_type(x, 8))
#define S48_STOBTYPE_RECORD 9
#define S48_RECORD_P(x) (s48_stob_has_type(x, 9))
#define S48_STOBTYPE_CONTINUATION 10
#define S48_CONTINUATION_P(x) (s48_stob_has_type(x, 10))
#define S48_STOBTYPE_EXTENDED_NUMBER 11
#define S48_EXTENDED_NUMBER_P(x) (s48_stob_has_type(x, 11))
#define S48_STOBTYPE_TEMPLATE 12
#define S48_TEMPLATE_P(x) (s48_stob_has_type(x, 12))
#define S48_STOBTYPE_WEAK_POINTER 13
#define S48_WEAK_POINTER_P(x) (s48_stob_has_type(x, 13))
#define S48_STOBTYPE_SHARED_BINDING 14
#define S48_SHARED_BINDING_P(x) (s48_stob_has_type(x, 14))
#define S48_STOBTYPE_TRANSPORT_LINK_CELL 15
#define S48_TRANSPORT_LINK_CELL_P(x) (s48_stob_has_type(x, 15))
#define S48_STOBTYPE_STRING 16
#define S48_STRING_P(x) (s48_stob_has_type(x, 16))
#define S48_STOBTYPE_BYTE_VECTOR 17
#define S48_BYTE_VECTOR_P(x) (s48_stob_has_type(x, 17))
#define S48_STOBTYPE_DOUBLE 18
#define S48_DOUBLE_P(x) (s48_stob_has_type(x, 18))
#define S48_STOBTYPE_BIGNUM 19
#define S48_BIGNUM_P(x) (s48_stob_has_type(x, 19))

#define S48_CAR_OFFSET 0
#define S48_CAR(x) (s48_stob_ref((x), S48_STOBTYPE_PAIR, 0))
#define S48_UNSAFE_CAR(x) (S48_STOB_REF((x), 0))
#define S48_SET_CAR(x, v) (s48_stob_set((x), S48_STOBTYPE_PAIR, 0, (v)))
#define S48_UNSAFE_SET_CAR(x, v) S48_STOB_SET((x), 0, (v))
#define S48_CDR_OFFSET 1
#define S48_CDR(x) (s48_stob_ref((x), S48_STOBTYPE_PAIR, 1))
#define S48_UNSAFE_CDR(x) (S48_STOB_REF((x), 1))
#define S48_SET_CDR(x, v) (s48_stob_set((x), S48_STOBTYPE_PAIR, 1, (v)))
#define S48_UNSAFE_SET_CDR(x, v) S48_STOB_SET((x), 1, (v))
#define S48_SYMBOL_TO_STRING_OFFSET 0
#define S48_SYMBOL_TO_STRING(x) (s48_stob_ref((x), S48_STOBTYPE_SYMBOL, 0))
#define S48_UNSAFE_SYMBOL_TO_STRING(x) (S48_STOB_REF((x), 0))
#define S48_LOCATION_ID_OFFSET 0
#define S48_LOCATION_ID(x) (s48_stob_ref((x), S48_STOBTYPE_LOCATION, 0))
#define S48_UNSAFE_LOCATION_ID(x) (S48_STOB_REF((x), 0))
#define S48_SET_LOCATION_ID(x, v) (s48_stob_set((x), S48_STOBTYPE_LOCATION, 0, (v)))
#define S48_UNSAFE_SET_LOCATION_ID(x, v) S48_STOB_SET((x), 0, (v))
#define S48_CONTENTS_OFFSET 1
#define S48_CONTENTS(x) (s48_stob_ref((x), S48_STOBTYPE_LOCATION, 1))
#define S48_UNSAFE_CONTENTS(x) (S48_STOB_REF((x), 1))
#define S48_SET_CONTENTS(x, v) (s48_stob_set((x), S48_STOBTYPE_LOCATION, 1, (v)))
#define S48_UNSAFE_SET_CONTENTS(x, v) S48_STOB_SET((x), 1, (v))
#define S48_CELL_REF_OFFSET 0
#define S48_CELL_REF(x) (s48_stob_ref((x), S48_STOBTYPE_CELL, 0))
#define S48_UNSAFE_CELL_REF(x) (S48_STOB_REF((x), 0))
#define S48_CELL_SET(x, v) (s48_stob_set((x), S48_STOBTYPE_CELL, 0, (v)))
#define S48_UNSAFE_CELL_SET(x, v) S48_STOB_SET((x), 0, (v))
#define S48_CLOSURE_TEMPLATE_OFFSET 0
#define S48_CLOSURE_TEMPLATE(x) (s48_stob_ref((x), S48_STOBTYPE_CLOSURE, 0))
#define S48_UNSAFE_CLOSURE_TEMPLATE(x) (S48_STOB_REF((x), 0))
#define S48_SET_CLOSURE_TEMPLATE(x, v) (s48_stob_set((x), S48_STOBTYPE_CLOSURE, 0, (v)))
#define S48_UNSAFE_SET_CLOSURE_TEMPLATE(x, v) S48_STOB_SET((x), 0, (v))
#define S48_CLOSURE_ENV_OFFSET 1
#define S48_CLOSURE_ENV(x) (s48_stob_ref((x), S48_STOBTYPE_CLOSURE, 1))
#define S48_UNSAFE_CLOSURE_ENV(x) (S48_STOB_REF((x), 1))
#define S48_SET_CLOSURE_ENV(x, v) (s48_stob_set((x), S48_STOBTYPE_CLOSURE, 1, (v)))
#define S48_UNSAFE_SET_CLOSURE_ENV(x, v) S48_STOB_SET((x), 1, (v))
#define S48_WEAK_POINTER_REF_OFFSET 0
#define S48_WEAK_POINTER_REF(x) (s48_stob_ref((x), S48_STOBTYPE_WEAK_POINTER, 0))
#define S48_UNSAFE_WEAK_POINTER_REF(x) (S48_STOB_REF((x), 0))
#define S48_SHARED_BINDING_NAME_OFFSET 0
#define S48_SHARED_BINDING_NAME(x) (s48_stob_ref((x), S48_STOBTYPE_SHARED_BINDING, 0))
#define S48_UNSAFE_SHARED_BINDING_NAME(x) (S48_STOB_REF((x), 0))
#define S48_SHARED_BINDING_IS_IMPORTP_OFFSET 1
#define S48_SHARED_BINDING_IS_IMPORTP(x) (s48_stob_ref((x), S48_STOBTYPE_SHARED_BINDING, 1))
#define S48_UNSAFE_SHARED_BINDING_IS_IMPORTP(x) (S48_STOB_REF((x), 1))
#define S48_SHARED_BINDING_REF_OFFSET 2
#define S48_SHARED_BINDING_REF(x) (s48_stob_ref((x), S48_STOBTYPE_SHARED_BINDING, 2))
#define S48_UNSAFE_SHARED_BINDING_REF(x) (S48_STOB_REF((x), 2))
#define S48_SHARED_BINDING_SET(x, v) (s48_stob_set((x), S48_STOBTYPE_SHARED_BINDING, 2, (v)))
#define S48_UNSAFE_SHARED_BINDING_SET(x, v) S48_STOB_SET((x), 2, (v))
#define S48_PORT_HANDLER_OFFSET 0
#define S48_PORT_HANDLER(x) (s48_stob_ref((x), S48_STOBTYPE_PORT, 0))
#define S48_UNSAFE_PORT_HANDLER(x) (S48_STOB_REF((x), 0))
#define S48_PORT_TEXT_CODEC_SPEC_OFFSET 1
#define S48_PORT_TEXT_CODEC_SPEC(x) (s48_stob_ref((x), S48_STOBTYPE_PORT, 1))
#define S48_UNSAFE_PORT_TEXT_CODEC_SPEC(x) (S48_STOB_REF((x), 1))
#define S48_SET_PORT_TEXT_CODEC_SPEC(x, v) (s48_stob_set((x), S48_STOBTYPE_PORT, 1, (v)))
#define S48_UNSAFE_SET_PORT_TEXT_CODEC_SPEC(x, v) S48_STOB_SET((x), 1, (v))
#define S48_PORT_CRLFP_OFFSET 2
#define S48_PORT_CRLFP(x) (s48_stob_ref((x), S48_STOBTYPE_PORT, 2))
#define S48_UNSAFE_PORT_CRLFP(x) (S48_STOB_REF((x), 2))
#define S48_SET_PORT_CRLFP(x, v) (s48_stob_set((x), S48_STOBTYPE_PORT, 2, (v)))
#define S48_UNSAFE_SET_PORT_CRLFP(x, v) S48_STOB_SET((x), 2, (v))
#define S48_PORT_STATUS_OFFSET 3
#define S48_PORT_STATUS(x) (s48_stob_ref((x), S48_STOBTYPE_PORT, 3))
#define S48_UNSAFE_PORT_STATUS(x) (S48_STOB_REF((x), 3))
#define S48_SET_PORT_STATUS(x, v) (s48_stob_set((x), S48_STOBTYPE_PORT, 3, (v)))
#define S48_UNSAFE_SET_PORT_STATUS(x, v) S48_STOB_SET((x), 3, (v))
#define S48_PORT_LOCK_OFFSET 4
#define S48_PORT_LOCK(x) (s48_stob_ref((x), S48_STOBTYPE_PORT, 4))
#define S48_UNSAFE_PORT_LOCK(x) (S48_STOB_REF((x), 4))
#define S48_SET_PORT_LOCK(x, v) (s48_stob_set((x), S48_STOBTYPE_PORT, 4, (v)))
#define S48_UNSAFE_SET_PORT_LOCK(x, v) S48_STOB_SET((x), 4, (v))
#define S48_PORT_DATA_OFFSET 5
#define S48_PORT_DATA(x) (s48_stob_ref((x), S48_STOBTYPE_PORT, 5))
#define S48_UNSAFE_PORT_DATA(x) (S48_STOB_REF((x), 5))
#define S48_SET_PORT_DATA(x, v) (s48_stob_set((x), S48_STOBTYPE_PORT, 5, (v)))
#define S48_UNSAFE_SET_PORT_DATA(x, v) S48_STOB_SET((x), 5, (v))
#define S48_PORT_BUFFER_OFFSET 6
#define S48_PORT_BUFFER(x) (s48_stob_ref((x), S48_STOBTYPE_PORT, 6))
#define S48_UNSAFE_PORT_BUFFER(x) (S48_STOB_REF((x), 6))
#define S48_SET_PORT_BUFFER(x, v) (s48_stob_set((x), S48_STOBTYPE_PORT, 6, (v)))
#define S48_UNSAFE_SET_PORT_BUFFER(x, v) S48_STOB_SET((x), 6, (v))
#define S48_PORT_INDEX_OFFSET 7
#define S48_PORT_INDEX(x) (s48_stob_ref((x), S48_STOBTYPE_PORT, 7))
#define S48_UNSAFE_PORT_INDEX(x) (S48_STOB_REF((x), 7))
#define S48_SET_PORT_INDEX(x, v) (s48_stob_set((x), S48_STOBTYPE_PORT, 7, (v)))
#define S48_UNSAFE_SET_PORT_INDEX(x, v) S48_STOB_SET((x), 7, (v))
#define S48_PORT_LIMIT_OFFSET 8
#define S48_PORT_LIMIT(x) (s48_stob_ref((x), S48_STOBTYPE_PORT, 8))
#define S48_UNSAFE_PORT_LIMIT(x) (S48_STOB_REF((x), 8))
#define S48_SET_PORT_LIMIT(x, v) (s48_stob_set((x), S48_STOBTYPE_PORT, 8, (v)))
#define S48_UNSAFE_SET_PORT_LIMIT(x, v) S48_STOB_SET((x), 8, (v))
#define S48_PORT_PENDING_CRP_OFFSET 9
#define S48_PORT_PENDING_CRP(x) (s48_stob_ref((x), S48_STOBTYPE_PORT, 9))
#define S48_UNSAFE_PORT_PENDING_CRP(x) (S48_STOB_REF((x), 9))
#define S48_SET_PORT_PENDING_CRP(x, v) (s48_stob_set((x), S48_STOBTYPE_PORT, 9, (v)))
#define S48_UNSAFE_SET_PORT_PENDING_CRP(x, v) S48_STOB_SET((x), 9, (v))
#define S48_PORT_PENDING_EOFP_OFFSET 10
#define S48_PORT_PENDING_EOFP(x) (s48_stob_ref((x), S48_STOBTYPE_PORT, 10))
#define S48_UNSAFE_PORT_PENDING_EOFP(x) (S48_STOB_REF((x), 10))
#define S48_SET_PORT_PENDING_EOFP(x, v) (s48_stob_set((x), S48_STOBTYPE_PORT, 10, (v)))
#define S48_UNSAFE_SET_PORT_PENDING_EOFP(x, v) S48_STOB_SET((x), 10, (v))
#define S48_CHANNEL_STATUS_OFFSET 0
#define S48_CHANNEL_STATUS(x) (s48_stob_ref((x), S48_STOBTYPE_CHANNEL, 0))
#define S48_UNSAFE_CHANNEL_STATUS(x) (S48_STOB_REF((x), 0))
#define S48_CHANNEL_ID_OFFSET 1
#define S48_CHANNEL_ID(x) (s48_stob_ref((x), S48_STOBTYPE_CHANNEL, 1))
#define S48_UNSAFE_CHANNEL_ID(x) (S48_STOB_REF((x), 1))
#define S48_CHANNEL_OS_INDEX_OFFSET 2
#define S48_CHANNEL_OS_INDEX(x) (s48_stob_ref((x), S48_STOBTYPE_CHANNEL, 2))
#define S48_UNSAFE_CHANNEL_OS_INDEX(x) (S48_STOB_REF((x), 2))
#define S48_CHANNEL_CLOSE_SILENTLYP_OFFSET 3
#define S48_CHANNEL_CLOSE_SILENTLYP(x) (s48_stob_ref((x), S48_STOBTYPE_CHANNEL, 3))
#define S48_UNSAFE_CHANNEL_CLOSE_SILENTLYP(x) (S48_STOB_REF((x), 3))
#define S48_TRANSPORT_LINK_CELL_KEY_OFFSET 0
#define S48_TRANSPORT_LINK_CELL_KEY(x) (s48_stob_ref((x), S48_STOBTYPE_TRANSPORT_LINK_CELL, 0))
#define S48_UNSAFE_TRANSPORT_LINK_CELL_KEY(x) (S48_STOB_REF((x), 0))
#define S48_TRANSPORT_LINK_CELL_VALUE_OFFSET 1
#define S48_TRANSPORT_LINK_CELL_VALUE(x) (s48_stob_ref((x), S48_STOBTYPE_TRANSPORT_LINK_CELL, 1))
#define S48_UNSAFE_TRANSPORT_LINK_CELL_VALUE(x) (S48_STOB_REF((x), 1))
#define S48_SET_TRANSPORT_LINK_CELL_VALUE(x, v) (s48_stob_set((x), S48_STOBTYPE_TRANSPORT_LINK_CELL, 1, (v)))
#define S48_UNSAFE_SET_TRANSPORT_LINK_CELL_VALUE(x, v) S48_STOB_SET((x), 1, (v))
#define S48_TRANSPORT_LINK_CELL_TCONC_OFFSET 2
#define S48_TRANSPORT_LINK_CELL_TCONC(x) (s48_stob_ref((x), S48_STOBTYPE_TRANSPORT_LINK_CELL, 2))
#define S48_UNSAFE_TRANSPORT_LINK_CELL_TCONC(x) (S48_STOB_REF((x), 2))
#define S48_SET_TRANSPORT_LINK_CELL_TCONC(x, v) (s48_stob_set((x), S48_STOBTYPE_TRANSPORT_LINK_CELL, 2, (v)))
#define S48_UNSAFE_SET_TRANSPORT_LINK_CELL_TCONC(x, v) S48_STOB_SET((x), 2, (v))
#define S48_TRANSPORT_LINK_CELL_NEXT_OFFSET 3
#define S48_TRANSPORT_LINK_CELL_NEXT(x) (s48_stob_ref((x), S48_STOBTYPE_TRANSPORT_LINK_CELL, 3))
#define S48_UNSAFE_TRANSPORT_LINK_CELL_NEXT(x) (S48_STOB_REF((x), 3))
#define S48_SET_TRANSPORT_LINK_CELL_NEXT(x, v) (s48_stob_set((x), S48_STOBTYPE_TRANSPORT_LINK_CELL, 3, (v)))
#define S48_UNSAFE_SET_TRANSPORT_LINK_CELL_NEXT(x, v) S48_STOB_SET((x), 3, (v))

#define S48_VECTOR_LENGTH(x) (s48_stob_length((x), S48_STOBTYPE_VECTOR))
#define S48_UNSAFE_VECTOR_LENGTH(x) (S48_STOB_DESCRIPTOR_LENGTH(x))
#define S48_VECTOR_REF(x, i) (s48_stob_ref((x), S48_STOBTYPE_VECTOR, (i)))
#define S48_VECTOR_SET(x, i, v) (s48_stob_set((x), S48_STOBTYPE_VECTOR, (i), (v)))
#define S48_UNSAFE_VECTOR_REF(x, i) (S48_STOB_REF((x), (i)))
#define S48_UNSAFE_VECTOR_SET(x, i, v) S48_STOB_SET((x), (i), (v))
#define S48_RECORD_LENGTH(x) (s48_stob_length((x), S48_STOBTYPE_RECORD))
#define S48_UNSAFE_RECORD_LENGTH(x) (S48_STOB_DESCRIPTOR_LENGTH(x))
#define S48_RECORD_REF(x, i) (s48_stob_ref((x), S48_STOBTYPE_RECORD, (i) + 1))
#define S48_RECORD_SET(x, i, v) (s48_stob_set((x), S48_STOBTYPE_RECORD, (i) + 1, (v)))
#define S48_UNSAFE_RECORD_REF(x, i) (S48_STOB_REF((x), (i) + 1))
#define S48_UNSAFE_RECORD_SET(x, i, v) S48_STOB_SET((x), (i) + 1, (v))
#define S48_RECORD_TYPE(x) (s48_stob_ref((x), S48_STOBTYPE_RECORD, 0))
#define S48_UNSAFE_RECORD_TYPE(x) (S48_STOB_REF((x), 0))
#define S48_BYTE_VECTOR_LENGTH(x) (s48_stob_byte_length((x), S48_STOBTYPE_BYTE_VECTOR))
#define S48_BYTE_VECTOR_REF(x, i) (s48_stob_byte_ref((x), S48_STOBTYPE_BYTE_VECTOR, (i)))
#define S48_BYTE_VECTOR_SET(x, i, v) (s48_stob_byte_set((x), S48_STOBTYPE_BYTE_VECTOR, (i), (v)))
#define S48_UNSAFE_BYTE_VECTOR_REF(x, i) (S48_STOB_BYTE_REF((x), (i)))
#define S48_UNSAFE_BYTE_VECTOR_SET(x, i, v) S48_BYTE_STOB_SET((x), (i), (v))
#define S48_UNSAFE_BYTE_VECTOR_LENGTH(x) (S48_STOB_BYTE_LENGTH(x))
#define S48_UNSAFE_EXTRACT_BYTE_VECTOR(x) (S48_ADDRESS_AFTER_HEADER((x), char))
#define S48_STRING_LENGTH(s) (s48_string_length(s))
#define S48_STRING_REF(s, i) (s48_string_ref((s), (i)))
#define S48_STRING_SET(s, i, v) (s48_string_set((s), (i), (v)))
#define S48_EXTRACT_EXTERNAL_OBJECT(x, type) ((type *)(S48_ADDRESS_AFTER_HEADER(x, long)+1))

#define S48_RECORD_TYPE_RESUMER(x) S48_RECORD_REF((x), 0)
#define S48_RECORD_TYPE_UID(x) S48_RECORD_REF((x), 1)
#define S48_RECORD_TYPE_NAME(x) S48_RECORD_REF((x), 2)
#define S48_RECORD_TYPE_FIELD_NAMES(x) S48_RECORD_REF((x), 3)
#define S48_RECORD_TYPE_NUMBER_OF_FIELDS(x) S48_RECORD_REF((x), 4)
#define S48_RECORD_TYPE_DISCLOSER(x) S48_RECORD_REF((x), 5)
#define S48_RECORD_TYPE_PARENT(x) S48_RECORD_REF((x), 6)
#define S48_RECORD_TYPE_EXTENSION_COUNT(x) S48_RECORD_REF((x), 7)
#define S48_RECORD_TYPE_SIZE(x) S48_RECORD_REF((x), 8)
#define S48_RECORD_TYPE_DATA(x) S48_RECORD_REF((x), 9)
#define S48_RECORD_TYPE_BASE(x) S48_RECORD_REF((x), 10)

#define S48_CHANNEL_STATUS_CLOSED S48_UNSAFE_ENTER_FIXNUM(0)
#define S48_CHANNEL_STATUS_INPUT S48_UNSAFE_ENTER_FIXNUM(1)
#define S48_CHANNEL_STATUS_OUTPUT S48_UNSAFE_ENTER_FIXNUM(2)
#define S48_CHANNEL_STATUS_SPECIAL_INPUT S48_UNSAFE_ENTER_FIXNUM(3)
#define S48_CHANNEL_STATUS_SPECIAL_OUTPUT S48_UNSAFE_ENTER_FIXNUM(4)

#endif /* !NO_OLD_FFI */

#define S48_EXCEPTION_UNASSIGNED_LOCAL 0
#define S48_EXCEPTION_UNDEFINED_GLOBAL 1
#define S48_EXCEPTION_UNBOUND_GLOBAL 2
#define S48_EXCEPTION_BAD_PROCEDURE 3
#define S48_EXCEPTION_WRONG_NUMBER_OF_ARGUMENTS 4
#define S48_EXCEPTION_WRONG_TYPE_ARGUMENT 5
#define S48_EXCEPTION_IMMUTABLE_ARGUMENT 6
#define S48_EXCEPTION_ARITHMETIC_OVERFLOW 7
#define S48_EXCEPTION_INDEX_OUT_OF_RANGE 8
#define S48_EXCEPTION_HEAP_OVERFLOW 9
#define S48_EXCEPTION_OUT_OF_MEMORY 10
#define S48_EXCEPTION_CANNOT_OPEN_CHANNEL 11
#define S48_EXCEPTION_CHANNEL_OS_INDEX_ALREADY_IN_USE 12
#define S48_EXCEPTION_CLOSED_CHANNEL 13
#define S48_EXCEPTION_BUFFER_FULLEMPTY 14
#define S48_EXCEPTION_UNIMPLEMENTED_INSTRUCTION 15
#define S48_EXCEPTION_TRAP 16
#define S48_EXCEPTION_PROCEEDING_AFTER_EXCEPTION 17
#define S48_EXCEPTION_BAD_OPTION 18
#define S48_EXCEPTION_UNBOUND_EXTERNAL_NAME 19
#define S48_EXCEPTION_TOO_MANY_ARGUMENTS_TO_EXTERNAL_PROCEDURE 20
#define S48_EXCEPTION_TOO_MANY_ARGUMENTS_IN_CALLBACK 21
#define S48_EXCEPTION_CALLBACK_RETURN_UNCOVERED 22
#define S48_EXCEPTION_EXTENSION_EXCEPTION 23
#define S48_EXCEPTION_EXTENSION_RETURN_ERROR 24
#define S48_EXCEPTION_OS_ERROR 25
#define S48_EXCEPTION_GC_PROTECTION_MISMATCH 26
#define S48_EXCEPTION_NO_CURRENT_PROPOSAL 27
#define S48_EXCEPTION_NATIVE_CODE_NOT_SUPPORTED 28
#define S48_EXCEPTION_ILLEGAL_EXCEPTION_RETURN 29
#define S48_EXCEPTION_EXTERNAL_ERROR 30
#define S48_EXCEPTION_EXTERNAL_ASSERTION_VIOLATION 31
#define S48_EXCEPTION_EXTERNAL_OS_ERROR 32

#include <scheme48write-barrier.h>

#ifdef __cplusplus
/* closing brace for extern "C" */
}
#endif

#endif /* _H_SCHEME48 */
