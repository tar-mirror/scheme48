/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Harald Glab-Phlag, Marcus Crestani
 */

#if !defined(FFI_TEST_H)
#define FFI_TEST_H

#include <stdlib.h>
#include <string.h>
#include "scheme48.h"

/* structure definitions for internal use */
typedef struct _strange_thing
{
  int id;
  char* name;
} strange_thing, *strange_thing_ref;   
/* external via .def exported functions */
void s48_on_load(void);

void s48_on_reload(void);

void s48_on_unload(void);

/* functions for testing the new ffi */
s48_ref_t ffi_working_on_lists(s48_call_t call,
			       s48_ref_t sch_a_list);

s48_ref_t ffi_add_integer(s48_call_t call,
			  s48_ref_t sch_integer);

void ffi_priv_initialize(void);

s48_ref_t ffi_pair_p(s48_call_t call, 
		     s48_ref_t sch_maybe_pair);

s48_ref_t ffi_car(s48_call_t call, s48_ref_t sch_a_pair);

s48_ref_t ffi_cdr(s48_call_t call, s48_ref_t sch_a_pair);

s48_ref_t ffi_length(s48_call_t call, s48_ref_t sch_a_list);

s48_ref_t ffi_record_set(s48_call_t call,
			 s48_ref_t  sch_record,
			 s48_ref_t sch_index,
			 s48_ref_t sch_value);

s48_ref_t ffi_record_ref(s48_call_t call,
			 s48_ref_t  sch_record,
			 s48_ref_t sch_index);

s48_ref_t ffi_vector_set(s48_call_t call,
			 s48_ref_t  sch_vector,
			 s48_ref_t sch_index,
			 s48_ref_t sch_value);

s48_ref_t ffi_vector_ref(s48_call_t call,
			 s48_ref_t  sch_vector,
			 s48_ref_t sch_index);

s48_ref_t ffi_make_vector(s48_call_t call,
			  s48_ref_t sch_length,
			  s48_ref_t sch_value);

			   
s48_ref_t ffi_make_byte_vector (s48_call_t call,
				s48_ref_t sch_length);

s48_ref_t ffi_extract_byte_vector (s48_call_t call, s48_ref_t bv);
s48_ref_t ffi_extract_and_modify_byte_vector (s48_call_t call, s48_ref_t bv);
s48_ref_t ffi_extract_twice_and_modify_byte_vector (s48_call_t call, s48_ref_t bv);
s48_ref_t ffi_extract_byte_vector_and_call_scheme (s48_call_t call, s48_ref_t bv, s48_ref_t callback);

s48_ref_t ffi_make_a_record(s48_call_t call,
			    s48_ref_t sch_id_string);

s48_ref_t ffi_get_cons_val(s48_call_t call, 
			   s48_ref_t sch_first_value,
			   s48_ref_t sch_second_value);

s48_ref_t ffi_enums(s48_call_t call,
		    s48_ref_t sch_enum);

s48_ref_t ffi_get_color_enum_set( s48_call_t call,
				  s48_ref_t sch_mask);

s48_ref_t ffi_call_scheme(s48_call_t call, s48_ref_t sch_proc,
			  s48_ref_t sch_nargs, s48_ref_t sch_parm_1,
			  s48_ref_t sch_parm_2, s48_ref_t sch_parm_3);

s48_ref_t ffi_make_strange_value (s48_call_t call, s48_ref_t sch_id,
				  s48_ref_t sch_name);

s48_ref_t ffi_strange_value_to_list (s48_call_t call,
				     s48_ref_t sch_strange_val);

s48_ref_t ffi_strange_value_free (s48_call_t call,
				  s48_ref_t sch_strange_val);

s48_ref_t ffi_propagate_binding(s48_call_t call,
				s48_ref_t sch_binding);

s48_ref_t ffi_propagate_binding_global(s48_call_t call,
				       s48_ref_t sch_binding);

s48_ref_t ffi_a_status_set_and_export(s48_call_t call,
				      s48_ref_t a_status_value);

s48_ref_t ffi_a_status_set_by_binding (s48_call_t call,
				       s48_ref_t sch_a_status_binding,
				       s48_ref_t sch_a_value);

s48_ref_t ffi_a_status_set (s48_call_t call, s48_ref_t sch_a_value);

s48_ref_t ffi_a_status(s48_call_t call, s48_ref_t sch_a_status_binding);

s48_ref_t ffi_export_bindings(s48_call_t call);

s48_ref_t ffi_any_shared_binding_set(s48_call_t call, 
				     s48_ref_t sch_shared_binding_name,
				     s48_ref_t sch_value);

s48_ref_t ffi_any_shared_binding_ref(s48_call_t call,
				     s48_ref_t sch_shared_binding_name);

s48_ref_t ffi_check_a_status_and_get_name(s48_call_t call);

s48_ref_t ffi_make_local_buf(s48_call_t call);
s48_ref_t ffi_free_local_buf(s48_call_t call);
s48_ref_t ffi_free_local_buf1(s48_call_t call);
s48_ref_t ffi_free_local_buf2(s48_call_t call);
s48_ref_t ffi_free_local_buf3(s48_call_t call);

s48_ref_t ffi_make_weak_pointer(s48_call_t call, s48_ref_t value);
s48_ref_t ffi_weak_pointer_p(s48_call_t call, s48_ref_t sch_pointer);
s48_ref_t ffi_weak_pointer_ref(s48_call_t call, s48_ref_t sch_pointer);

s48_ref_t ffi_check_string_latin_1 (s48_call_t call, s48_ref_t sch_string);
s48_ref_t ffi_check_string_utf_8 (s48_call_t call, s48_ref_t sch_string);
s48_ref_t ffi_check_string_utf_16 (s48_call_t call, s48_ref_t sch_string);

char* ffi_string_to_latin_1(s48_call_t call, s48_ref_t sch_in);

#endif
