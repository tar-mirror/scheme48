/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Harald Glab-Phlag, Mike Sperber
 */

/* this file includes procedures to write a double to a byte - vector and read it
   out of the vect */

#include <scheme48.h>

static s48_ref_t
r6rs_float_to_bytevect (s48_call_t call, s48_ref_t sch_float, 
			s48_ref_t sch_bytevect, s48_ref_t sch_index) 
{
  long index = s48_extract_long_2(call, sch_index);
  char values[4];
  long ref = 0;
  *((float*)values) = (float)s48_extract_double_2(call, sch_float);

  while (ref < 4)
    {
      s48_byte_vector_set_2(call, sch_bytevect, index + ref, values[ref]);
      ++ref;
    }
  return s48_unspecific_2(call);
}

static s48_ref_t
r6rs_bytevect_to_float (s48_call_t call, s48_ref_t sch_bytevect, s48_ref_t sch_index) 
{
  long index = s48_extract_long_2(call, sch_index);
  char values[4];
  long ref = 0;
  float resval;
  while (ref < 4)
    {
      values[ref] = s48_byte_vector_ref_2(call, sch_bytevect, index + ref);
      ++ref;
    }
  resval =  *((float*) values);
  return s48_enter_double_2(call, (double)resval);
}

static s48_ref_t
r6rs_double_to_bytevect (s48_call_t call, s48_ref_t sch_double, 
			 s48_ref_t sch_bytevect, s48_ref_t sch_index) 
{
  long index = s48_extract_long_2(call, sch_index);
  char values[8];
  long ref = 0;
  *((double*)values) = s48_extract_double_2(call, sch_double);
  while (ref < 8)
    {
      s48_byte_vector_set_2(call, sch_bytevect, index + ref, values[ref]);
      ++ref;
    }
  return s48_unspecific_2(call);
}

static s48_ref_t
r6rs_bytevect_to_double (s48_call_t call, s48_ref_t sch_bytevect, s48_ref_t sch_index)
{
  long index = s48_extract_long_2(call, sch_index);
  char values[8]; 
  double resval;
  long ref = 0;
  while (ref < 8)
    {
      values[ref] = s48_byte_vector_ref_2(call, sch_bytevect, index + ref);
      ++ref;
    }
  resval = *((double*) values);
  return s48_enter_double_2(call, resval);
}

static s48_ref_t
r6rs_is_big_endian (s48_call_t call) 
{
  union {
    uint32_t i;
    char c[4];
  } bint = {0x01020304};

  return s48_enter_boolean_2(call, bint.c[0] == 1);
}

void s48_init_ieee_bytevect(void)
{
  S48_EXPORT_FUNCTION(r6rs_float_to_bytevect);
  S48_EXPORT_FUNCTION(r6rs_bytevect_to_float);
  S48_EXPORT_FUNCTION(r6rs_double_to_bytevect);
  S48_EXPORT_FUNCTION(r6rs_bytevect_to_double);
  S48_EXPORT_FUNCTION(r6rs_is_big_endian);
}
