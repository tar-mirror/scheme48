/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Harald Glab-Phlag, Marcus Crestani
 */

#include "ffi-test.h"

static s48_ref_t a_record_record_type_binding;
static s48_ref_t color_enum_type_binding;

s48_ref_t a_status;
s48_ref_t a_status_the_binding;

/* internal prototype */
uint16_t* ffi_string_to_utf_16(s48_call_t call,s48_ref_t sch_in);
char* ffi_string_to_utf_8(s48_call_t call,s48_ref_t sch_in);
char* ffi_string_to_latin_1(s48_call_t call, s48_ref_t sch_in);

void 
ffi_priv_initialize()
{
  a_record_record_type_binding = s48_get_imported_binding_2("a-record-record-type"); 
  color_enum_type_binding = s48_get_imported_binding_2("color-set-type"); 
}

s48_ref_t
ffi_working_on_lists(s48_call_t call,s48_ref_t sch_a_list)
{
  s48_ref_t sch_length = s48_length_2(call, sch_a_list); 
  int length = s48_extract_long_2(call, sch_length);
  s48_ref_t result = s48_make_vector_2(call,
				       length, 
				       s48_enter_long_2(call, 0));
  int counter = 0;
  while (!s48_null_p_2(call, sch_a_list))
    {
      s48_ref_t car_of_list = s48_car_2(call, sch_a_list);
      sch_a_list = s48_cdr_2(call , sch_a_list);
      s48_vector_set_2(call, result, counter, car_of_list); 
      s48_free_local_ref(call, car_of_list);
      counter++;
    }
  return result;
}

s48_ref_t
ffi_get_cons_val(s48_call_t call, 
		 s48_ref_t sch_first_value,
		 s48_ref_t sch_second_value)
{
  /* this set is to get the constant #\A by using s48_make_local_ref: */
  s48_ref_t new_char = s48_unsafe_enter_char_2(call, 65); 
  s48_ref_t cons_one = s48_cons_2(call , sch_first_value, sch_second_value);
  s48_ref_t result = s48_cons_2(call, new_char, cons_one); 
  return result;
}

s48_ref_t
ffi_pair_p(s48_call_t call, 
	   s48_ref_t sch_maybe_pair)
{
  s48_ref_t result = 
    (s48_pair_p_2(call, sch_maybe_pair)) 
    ? s48_true_2(call) : s48_false_2(call);
  return result;
}

s48_ref_t
ffi_car(s48_call_t call, s48_ref_t sch_a_pair)
{
  s48_check_pair_2(call, sch_a_pair);
  return s48_car_2(call, sch_a_pair);
}

s48_ref_t
ffi_cdr(s48_call_t call, s48_ref_t sch_a_pair)
{
  s48_check_pair_2(call, sch_a_pair);
  return s48_cdr_2(call, sch_a_pair);
}

s48_ref_t
ffi_record_set(s48_call_t call,
	       s48_ref_t sch_record,
	       s48_ref_t sch_index,
	       s48_ref_t sch_value)
{
  long index = s48_extract_long_2(call, sch_index);
  long rec_len = s48_record_length_2(call, sch_record);
  if (index < rec_len)
    {  
      s48_record_set_2(call, sch_record, index, sch_value);
    }
  else 
    {
      s48_assertion_violation_2(call, NULL, "record index wrong", 1, sch_index);
    }
  return sch_record;
}

s48_ref_t
ffi_record_ref(s48_call_t call,
	       s48_ref_t sch_record,
	       s48_ref_t sch_index)
{
  int index = s48_extract_long_2(call, sch_index);
  long rec_len = s48_record_length_2(call, sch_record);
  s48_ref_t result = s48_unspecific_2(call);
  if (index < rec_len)
    {  
      result = s48_record_ref_2(call, sch_record, index);
    }  
  return result;
}

s48_ref_t
ffi_vector_set(s48_call_t call,
	       s48_ref_t sch_vector,
	       s48_ref_t sch_index,
	       s48_ref_t sch_value)
{
  int index = s48_extract_long_2(call, sch_index);
  s48_vector_set_2(call, sch_vector, index, sch_value);
  return sch_vector;
}

s48_ref_t
ffi_vector_ref(s48_call_t call,
	       s48_ref_t sch_vector,
	       s48_ref_t sch_index)
{
  int index = s48_extract_long_2(call, sch_index);
  s48_ref_t result = s48_vector_ref_2(call, sch_vector, index);
  return result;
}

s48_ref_t
ffi_length(s48_call_t call, s48_ref_t sch_a_list)
{
  return s48_length_2(call, sch_a_list);
}
  

s48_ref_t
ffi_add_integer(s48_call_t call, s48_ref_t sch_integer)
{
  long value = s48_extract_long_2(call, sch_integer);
  value *= 10;
  return s48_enter_long_2(call, value);
}

s48_ref_t 
ffi_make_a_record(s48_call_t call, s48_ref_t sch_id_string)
{
  s48_ref_t a_record = s48_make_record_2(call,a_record_record_type_binding);
  s48_ref_t sch_type = s48_enter_string_latin_1_2(call, "type");
  s48_ref_t sch_value  = s48_enter_string_latin_1_2(call, "a-value");
  s48_record_set_2(call, a_record, 0, sch_id_string);
  s48_record_set_2(call, a_record, 1, sch_type);
  s48_record_set_2(call, a_record, 2, sch_value);
  return a_record;
}


s48_ref_t 
ffi_make_byte_vector (s48_call_t call, s48_ref_t sch_length)
{
  int length = s48_extract_long_2(call, sch_length);
  s48_ref_t result = s48_make_byte_vector_2(call, length);
  int count = 0;
  for (count = 0; count < length; count++)
    {
      /* the thing in --- code --- was how I thought the function / macro 
	 works but it deals instead with a direct value 
	 "would it be better to tell this through its name ?? s48_byte_vector_set_direct e.g. H.G.P." 
	 -- s48_ref_t sch_ref = s48_enter_long_2(call, (65 + count));
	 s48_unsafe_byte_vector_set_2(call, result, count, sch_ref); -- */
      s48_unsafe_byte_vector_set_2(call, result, count, (char)(65 + count));
      /*s48_byte_vector_set_2(call, result, count, (long)(65 + count));*/
    }   
  return result;
}  

s48_ref_t
ffi_extract_byte_vector(s48_call_t call, s48_ref_t bv) 
{
  char *buf = s48_extract_byte_vector_2(call, bv);
  int i;
  int res = 1;
  for (i = 0; i < 10; i++)
    res = res && (buf[i] == 'a');
  if (res)
    return s48_true_2(call);
  else
    return s48_false_2(call);
}

s48_ref_t
ffi_extract_byte_vector_readonly(s48_call_t call, s48_ref_t bv) 
{
  char *buf = s48_extract_byte_vector_readonly_2(call, bv);
  int i;
  int res = 1;
  for (i = 0; i < 10; i++)
    res = res && (buf[i] == 'a');

  buf[4] = '4';
  buf[8] = '8';

  if (res)
    return s48_true_2(call);
  else
    return s48_false_2(call);
}

s48_ref_t
ffi_extract_and_modify_byte_vector(s48_call_t call, s48_ref_t bv) 
{
  char *buf = s48_extract_byte_vector_2(call, bv);
  buf[5] = '5';
  return s48_true_2(call);
}

s48_ref_t
ffi_extract_twice_and_modify_byte_vector(s48_call_t call, s48_ref_t bv) 
{
  char *buf = s48_extract_byte_vector_2(call, bv);
  buf[4] = '4';

  buf = s48_extract_byte_vector_2(call, bv);
  buf[8] = '8';

  return s48_true_2(call);
}

s48_ref_t
ffi_extract_byte_vector_and_call_scheme(s48_call_t call, s48_ref_t bv, s48_ref_t sch_proc)
{  
  char *buf = s48_extract_byte_vector_2(call, bv);
  buf[4] = '4';
  buf[8] = '8';

  s48_call_scheme_2(call, sch_proc, 0);

  if (!((buf[4] == 'b') && (buf[8] == 'b')))
    return s48_false_2(call);

  buf[4] = '8';
  buf[8] = '4';

  return s48_true_2(call);
}

s48_ref_t
ffi_extract_byte_vector_assertion(s48_call_t call, s48_ref_t bv)
{  
  char *buf = s48_extract_byte_vector_2(call, bv);
  buf[4] = '4';
  buf[8] = '8';

  s48_assertion_violation_2(call, "ffi_extract_byte_vector_assertion",
			    "throw back to Scheme", 1, bv);

  if (!((buf[4] == 'b') && (buf[8] == 'b')))
    return s48_false_2(call);

  buf[4] = '8';
  buf[8] = '4';

  return s48_true_2(call);
}

s48_ref_t 
ffi_make_vector(s48_call_t call,
		s48_ref_t sch_length,
		s48_ref_t sch_value)
{
  int length = s48_extract_long_2(call, sch_length);
  s48_ref_t result = s48_make_vector_2(call,
				       length, 
				       sch_value);
  return result;
}

s48_ref_t
ffi_enums(s48_call_t call, s48_ref_t sch_enum)
{ 
  long enum_value_int = s48_enum_set2integer_2(call, sch_enum);
  s48_ref_t result = s48_enter_long_2(call, enum_value_int);
  return result;
}

s48_ref_t
ffi_get_color_enum_set( s48_call_t call, s48_ref_t sch_mask)
{  
  long mask = s48_extract_long_2(call, sch_mask);
  s48_ref_t sch_enum = 
    s48_integer2enum_set_2(call, color_enum_type_binding, mask);
  return sch_enum;
}  

s48_ref_t
ffi_call_scheme(s48_call_t call, s48_ref_t sch_proc, 
		s48_ref_t sch_nargs, s48_ref_t sch_parm_1,
		s48_ref_t sch_parm_2, s48_ref_t sch_parm_3)
{  
  long nargs =  s48_extract_long_2(call, sch_nargs);
  s48_ref_t result =
    s48_call_scheme_2(call, sch_proc, nargs, 
		      sch_parm_1, sch_parm_2, sch_parm_3);
  return result;
}

s48_ref_t
ffi_make_strange_value (s48_call_t call, s48_ref_t sch_id,
			s48_ref_t sch_name)
{
  char *temp;
  strange_thing_ref val = (strange_thing_ref)calloc(sizeof(strange_thing),1);
  s48_ref_t result = s48_make_value_2(call, strange_thing_ref);
  val->id = s48_extract_long_2(call, sch_id);
  temp = ffi_string_to_latin_1(call, sch_name); 
  val->name = calloc((s48_string_length_2(call, sch_name) +1),1);
  strcpy(val->name, temp);
  s48_set_value_2(call, result, strange_thing_ref, val);
  return result;
}  

s48_ref_t
ffi_strange_value_to_list (s48_call_t call, s48_ref_t sch_strange_val)
{
  strange_thing_ref val =  s48_extract_value_2(call, sch_strange_val, strange_thing_ref);
  s48_ref_t result = s48_cons_2(call, s48_enter_long_2(call, val->id), 
				s48_enter_string_latin_1_2(call, val->name));
  return result;
}  

s48_ref_t
ffi_strange_value_free (s48_call_t call, s48_ref_t sch_strange_val)
{
  strange_thing_ref val =  s48_extract_value_2(call, sch_strange_val, strange_thing_ref);
  free(val->name);
  free(val);
  s48_free_local_ref(call, sch_strange_val);
  return s48_unspecific_2(call);
}  

s48_ref_t
ffi_export_bindings(s48_call_t call)
{
  s48_value a_status_new = s48_enter_integer(80);
  a_status = s48_make_global_ref (a_status_new);
  /* export the binding "a-status" which is just a simple integer */
  return s48_define_exported_binding_2(call, "a-status", a_status);
}

s48_ref_t
ffi_propagate_binding(s48_call_t call, s48_ref_t sch_binding)
{
  a_status_the_binding = s48_copy_local_ref(call, sch_binding);
  return s48_unspecific_2(call);
}  

s48_ref_t
ffi_propagate_binding_global(s48_call_t call, s48_ref_t sch_binding)
{
  a_status_the_binding = s48_local_to_global_ref(sch_binding);
  return s48_unspecific_2(call);
}

s48_ref_t
ffi_a_status_set_and_export(s48_call_t call, s48_ref_t a_status_value)
{
  long value =  s48_extract_long_2(call, a_status_value);
  a_status = s48_enter_long_2(call, value);
  
  return s48_define_exported_binding_2(call, "a-status", a_status);
  
}

s48_ref_t
ffi_a_status_set(s48_call_t call, s48_ref_t a_status_value)
{
  s48_shared_binding_set_2(call, a_status_the_binding, a_status_value);
  return s48_unspecific_2(call);
}  

s48_ref_t 
ffi_a_status_set_by_binding (s48_call_t call, 
			     s48_ref_t sch_a_status_binding,
			     s48_ref_t sch_a_value)
{
  s48_shared_binding_set_2(call, sch_a_status_binding, sch_a_value);
  return s48_unspecific_2(call);  
}    
				       
s48_ref_t
ffi_a_status(s48_call_t call, s48_ref_t sch_a_status_binding)
{
  return s48_shared_binding_ref_2(call, sch_a_status_binding); 
} 

s48_ref_t
ffi_check_a_status_and_get_name(s48_call_t call)
{
  if (s48_true_p_2(call , s48_shared_binding_is_importp_2(call, a_status_the_binding)))
    return s48_shared_binding_name_2(call, a_status_the_binding);
  else return s48_false_2(call);
}     

s48_ref_t
ffi_any_shared_binding_set(s48_call_t call,
			   s48_ref_t sch_shared_binding_name,
			   s48_ref_t sch_value)
{
  return s48_unspecific_2(call);
  
}  



s48_ref_t ffi_any_shared_binding_ref(s48_call_t call, s48_ref_t sch_shared_binding_name)
{
  return s48_unspecific_2(call);
  
}  
/* end new binding funcs */

/* manged local buffers */
s48_ref_t
ffi_make_local_buf(s48_call_t call)
{
  void *buf = s48_make_local_buf(call, 123);
  memset(buf, '0', 123);
  return s48_true_2(call);
}

s48_ref_t
ffi_free_local_buf(s48_call_t call)
{
  void *buf = s48_make_local_buf(call, 123);
  s48_free_local_buf(call, buf);
  return s48_true_2(call);
}

s48_ref_t
ffi_free_local_buf1(s48_call_t call)
{
  void *buf1 = s48_make_local_buf(call, 123);
  void *buf2 = s48_make_local_buf(call, 123);
  void *buf3 = s48_make_local_buf(call, 123);
  s48_free_local_buf(call, buf1);
  return s48_true_2(call);
}
    
s48_ref_t
ffi_free_local_buf2(s48_call_t call)
{
  void *buf1 = s48_make_local_buf(call, 123);
  void *buf2 = s48_make_local_buf(call, 123);
  void *buf3 = s48_make_local_buf(call, 123);
  s48_free_local_buf(call, buf2);
  return s48_true_2(call);
}

s48_ref_t
ffi_free_local_buf3(s48_call_t call)
{
  void *buf1 = s48_make_local_buf(call, 123);
  void *buf2 = s48_make_local_buf(call, 123);
  void *buf3 = s48_make_local_buf(call, 123);
  s48_free_local_buf(call, buf3);
  return s48_true_2(call);
}

s48_ref_t
ffi_make_weak_pointer(s48_call_t call, s48_ref_t sch_value)
{
  s48_ref_t result = s48_make_weak_pointer_2(call, sch_value); 
  return result;
}

s48_ref_t
ffi_weak_pointer_p(s48_call_t call, s48_ref_t sch_pointer)
{
  return 
    (s48_weak_pointer_p_2(call, sch_pointer)) 
    ? s48_true_2(call) : s48_false_2(call);
}   

s48_ref_t
ffi_weak_pointer_ref(s48_call_t call, s48_ref_t sch_pointer)
{
  return s48_weak_pointer_ref_2(call, sch_pointer);
}

s48_ref_t
ffi_check_string_latin_1 (s48_call_t call, s48_ref_t sch_string)
{
  char* buffer = ffi_string_to_latin_1(call, sch_string);
  long length = s48_string_length_2(call, sch_string);
  s48_ref_t result = s48_enter_byte_vector_2(call, buffer, length);
  return result;
}
s48_ref_t 
ffi_check_string_utf_8 (s48_call_t call, s48_ref_t sch_string)
{
  char* buffer = ffi_string_to_utf_8(call, sch_string);
  long length = s48_string_utf_8_length_2(call, sch_string);
  s48_ref_t result = s48_enter_byte_vector_2(call, buffer, length);
  return result;
}

/* this function is for future use because there may 
   be changes for the be/le problem with utf-16 */
s48_ref_t ffi_check_string_utf_16 (s48_call_t call, s48_ref_t sch_string)
{
  s48_ref_t result;
  int i;
  uint16_t* buffer = ffi_string_to_utf_16(call, sch_string);
  long length = s48_string_utf_16le_length_2(call, sch_string);
  long len = length;
  length *= sizeof(uint16_t); 
  result = s48_make_vector_2(call, len, s48_false_2(call));
  for (i = 0; i < len; i++)
    {
      uint16_t element = *(buffer + i);
      s48_vector_set_2(call, result, i, s48_enter_long_2(call, element));
    }
  return result;
}

/* internally used functions */
char* ffi_string_to_latin_1(s48_call_t call, s48_ref_t sch_in)
{
  char* buffer = NULL;
  long length = s48_string_length_2(call, sch_in) +1;
  buffer = (char*)s48_make_local_buf(call, length);
  s48_copy_string_to_latin_1_2(call, sch_in, buffer);
  return buffer;
}

uint16_t* ffi_string_to_utf_16(s48_call_t call, s48_ref_t sch_in)
{
  uint16_t* buffer = NULL;
  /* s48_string_utf_16le_length_2 is defined to return the length in codepoints */
  long length = s48_string_utf_16le_length_2(call, sch_in);
  length *= sizeof(uint16_t); /* fix code - point length of 2 byte */
  buffer = (uint16_t*)s48_make_local_buf(call, length);
  s48_copy_string_to_utf_16le_2(call, sch_in, buffer);
  return buffer;
}

char* ffi_string_to_utf_8(s48_call_t call, s48_ref_t sch_in)
{
  char* buffer = NULL;
  /* here the real length in byte is returned correct me Mike if it is
     not true if this are codepoints it has to be multiplied with 4
     (max. length of a utf-8 codepoint in byte)*/
  long length = s48_string_utf_8_length_2(call, sch_in);
  buffer = (char*)s48_make_local_buf(call, length);
  s48_copy_string_to_utf_8_2(call, sch_in, buffer);
  return buffer;
}

void s48_on_load(void)
{
  ffi_priv_initialize();
  S48_EXPORT_FUNCTION(ffi_add_integer);
  S48_EXPORT_FUNCTION(ffi_working_on_lists);
  S48_EXPORT_FUNCTION(ffi_make_a_record);
  S48_EXPORT_FUNCTION(ffi_get_cons_val);
  S48_EXPORT_FUNCTION(ffi_pair_p);
  S48_EXPORT_FUNCTION(ffi_car);
  S48_EXPORT_FUNCTION(ffi_cdr);
  S48_EXPORT_FUNCTION(ffi_length);
  S48_EXPORT_FUNCTION(ffi_record_set);
  S48_EXPORT_FUNCTION(ffi_record_ref);
  S48_EXPORT_FUNCTION(ffi_vector_set);
  S48_EXPORT_FUNCTION(ffi_vector_ref);
  S48_EXPORT_FUNCTION(ffi_make_byte_vector );
  S48_EXPORT_FUNCTION(ffi_extract_byte_vector);
  S48_EXPORT_FUNCTION(ffi_extract_byte_vector_readonly);
  S48_EXPORT_FUNCTION(ffi_extract_and_modify_byte_vector);
  S48_EXPORT_FUNCTION(ffi_extract_twice_and_modify_byte_vector);
  S48_EXPORT_FUNCTION(ffi_extract_byte_vector_and_call_scheme);
  S48_EXPORT_FUNCTION(ffi_extract_byte_vector_assertion);
  S48_EXPORT_FUNCTION(ffi_make_vector);
  S48_EXPORT_FUNCTION(ffi_enums);
  S48_EXPORT_FUNCTION(ffi_call_scheme);
  S48_EXPORT_FUNCTION(ffi_a_status_set_and_export);
  S48_EXPORT_FUNCTION(ffi_a_status);
  S48_EXPORT_FUNCTION(ffi_any_shared_binding_ref);
  S48_EXPORT_FUNCTION(ffi_any_shared_binding_set);
  S48_EXPORT_FUNCTION(ffi_a_status_set_by_binding);
  S48_EXPORT_FUNCTION(ffi_a_status_set);
  S48_EXPORT_FUNCTION(ffi_make_strange_value); 
  S48_EXPORT_FUNCTION(ffi_strange_value_to_list );
  S48_EXPORT_FUNCTION(ffi_strange_value_free);
  S48_EXPORT_FUNCTION(ffi_export_bindings);
  S48_EXPORT_FUNCTION(ffi_propagate_binding);
  S48_EXPORT_FUNCTION(ffi_propagate_binding_global);
  S48_EXPORT_FUNCTION(ffi_get_color_enum_set);
  S48_EXPORT_FUNCTION(ffi_check_a_status_and_get_name);
  S48_EXPORT_FUNCTION(ffi_make_local_buf);
  S48_EXPORT_FUNCTION(ffi_free_local_buf);
  S48_EXPORT_FUNCTION(ffi_free_local_buf1);
  S48_EXPORT_FUNCTION(ffi_free_local_buf2);
  S48_EXPORT_FUNCTION(ffi_free_local_buf3);
  S48_EXPORT_FUNCTION(ffi_make_weak_pointer);
  S48_EXPORT_FUNCTION(ffi_weak_pointer_p);
  S48_EXPORT_FUNCTION(ffi_weak_pointer_ref);
  S48_EXPORT_FUNCTION(ffi_check_string_latin_1);
  S48_EXPORT_FUNCTION(ffi_check_string_utf_8);
  S48_EXPORT_FUNCTION(ffi_check_string_utf_16);
}

void s48_on_reload(void)
{
  s48_on_load();
}

void s48_on_unload(void)
{
}
