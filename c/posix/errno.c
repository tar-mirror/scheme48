/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Mike Sperber, Will Noble
 */

/*
 * Scheme 48/POSIX errno mapping
 * (largely copied & renamed from the signal mapping in proc.c)
 */

#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>
#include "scheme48.h"

/*
 * Mapping from our `canonical' errno numbers to the local OS's
 * numbers. To avoid having to manually keep the values here in sync
 * with the NAMED-ERRNOS finite record type, we generate the values
 * using a Scheme program.
 */
static int errno_map[] = {
#include "s48_errno.h"
};

extern void s48_init_posix_errno(void);
static s48_ref_t posix_initialize_named_errnos(s48_call_t call);

/*
 * Vector of Scheme errno objects imported from Scheme.
 */

static s48_ref_t posix_errnos_vector_binding;

/*
 * Install all exported functions in Scheme48.
 */
void
s48_init_posix_errno(void)
{
  S48_EXPORT_FUNCTION(posix_initialize_named_errnos);

  posix_errnos_vector_binding =
    s48_get_imported_binding_2("posix-errnos-vector");
}

static s48_ref_t
posix_initialize_named_errnos(s48_call_t call)
{
  int i, length;
  s48_ref_t named_errnos;

  s48_shared_binding_check_2(call, posix_errnos_vector_binding);

  named_errnos = s48_shared_binding_ref_2(call, posix_errnos_vector_binding);

  if(! s48_vector_p_2(call, named_errnos))
    s48_assertion_violation_2(call, 
			      "posix_initialize_named_errnos", "not a vector", 1,
			      named_errnos);
    
  length = s48_unsafe_vector_length_2(call, named_errnos);

  for(i = 0; i < length; i++) {
    s48_ref_t named_errno = s48_unsafe_vector_ref_2(call, named_errnos, i);
    int canonical = s48_extract_long_2(call, s48_unsafe_record_ref_2(call, named_errno, 1));
    int c_errno = errno_map[canonical];
    s48_ref_t scm_errno = (c_errno == -1) ?
                           s48_false_2(call) :
                           s48_enter_long_2(call, c_errno);
    
    s48_unsafe_record_set_2(call, named_errno, 2, scm_errno); }

  return s48_unspecific_2(call);
}
