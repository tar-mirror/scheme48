/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Mike Sperber, Harald Glab-Phlak
 */

/*
 * Access to various Scheme-side libraries via the FFI
 */

#include <stdlib.h>
#include "scheme48.h"

/*
 * Enum sets
 */

static s48_ref_t enum_set_type_binding = NULL;

/*
 * This needs to be in synch with the layout of :ENUM-SET in enum-set.scm
 */

static void
check_enum_set(s48_value sch_thing)
{
  s48_check_record_type(sch_thing, s48_deref(enum_set_type_binding));
}

static void
check_enum_set_2(s48_call_t call, s48_ref_t sch_thing)
{
  s48_check_record_type_2(call, sch_thing, enum_set_type_binding);
}

void
s48_check_enum_set_type(s48_value sch_thing, s48_value sch_enum_set_type_binding)
{
  check_enum_set(sch_thing);
  {
    s48_value actual_type = S48_UNSAFE_RECORD_REF(sch_thing, 0);
    s48_value binding_val = S48_SHARED_BINDING_REF(sch_enum_set_type_binding);
    s48_value unspecific = S48_UNSPECIFIC;
  
      
  if (!S48_EQ_P(S48_UNSAFE_RECORD_REF(sch_thing, 0),
		S48_SHARED_BINDING_REF(sch_enum_set_type_binding)))
    s48_assertion_violation("s48_check_enum_set_type", "invalid enum-set type", 2,
			    sch_thing, binding_val);
  }
}

void
s48_check_enum_set_type_2(s48_call_t call, s48_ref_t sch_thing, s48_ref_t sch_enum_set_type_binding)
{
  check_enum_set_2(call, sch_thing);
  {
    s48_ref_t actual_type = s48_unsafe_record_ref_2(call, sch_thing, 0);
    s48_ref_t binding_val = s48_shared_binding_ref_2(call, sch_enum_set_type_binding);
      
    if (!s48_eq_p_2(call, actual_type, binding_val))
      s48_assertion_violation_2(call, "s48_check_enum_set_type_2",
				"invalid enum-set type", 2,
				sch_thing, binding_val);
  }
}

long
s48_enum_set2integer(s48_value sch_enum_set)
{
  check_enum_set(sch_enum_set);
  return s48_extract_fixnum(S48_UNSAFE_RECORD_REF(sch_enum_set, 1));
}

long
s48_enum_set2integer_2(s48_call_t call, s48_ref_t sch_enum_set)
{
  check_enum_set_2(call, sch_enum_set);
  return s48_extract_long_2(call, s48_unsafe_record_ref_2(call, sch_enum_set, 1));
}

s48_value
s48_integer2enum_set(s48_value sch_enum_set_type_binding, long mask)
{
  s48_value sch_enum_set = s48_make_record(s48_deref(enum_set_type_binding));
  S48_UNSAFE_RECORD_SET(sch_enum_set, 0, S48_SHARED_BINDING_REF(sch_enum_set_type_binding));
  S48_UNSAFE_RECORD_SET(sch_enum_set, 1, s48_enter_fixnum(mask));
  return sch_enum_set;
}

s48_ref_t
s48_integer2enum_set_2(s48_call_t call, s48_ref_t sch_enum_set_type_binding, long mask)
{
  s48_ref_t sch_enum_set = s48_make_record_2(call, enum_set_type_binding);
  s48_unsafe_record_set_2(call, sch_enum_set, 0, 
			  s48_shared_binding_ref_2(call, sch_enum_set_type_binding));
  s48_unsafe_record_set_2(call, sch_enum_set, 1,
			  s48_enter_long_as_fixnum_2(call, mask));
  return sch_enum_set;
}

void
s48_init_external_libs(void)
{
  enum_set_type_binding = s48_get_imported_binding_2("enum-set-type");
}
