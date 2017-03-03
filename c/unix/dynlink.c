/* Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Mike Sperber, Marcus Crestani
 */

#define NO_OLD_FFI 1

/*
 * Dynamically load external modules on machines that support it.
 */

#include "sysdep.h"

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include "scheme48.h"

#if defined(HAVE_DLOPEN)
 #include <dlfcn.h>
#else
#include "fake/dlfcn.h"
#endif

#if	defined(RTLD_NOW)
#define	DLOPEN_MODE	RTLD_NOW
#elif	defined(RTLD_LAZY)
#define	DLOPEN_MODE	(RTLD_LAZY)
#else
#define	DLOPEN_MODE	(1)
#endif

static s48_ref_t
shared_object_dlopen(s48_call_t call, s48_ref_t name, s48_ref_t complete_name_p)
{
  void *handle;
  s48_ref_t res;
  char *full_name;

  if (!s48_false_p_2(call, complete_name_p))
    {
      size_t len = strlen(s48_extract_byte_vector_readonly_2(call, name));
      full_name = s48_make_local_buf(call, len + 4);
      memcpy(full_name,
	     s48_extract_byte_vector_readonly_2(call, name),
	     len);
      memcpy(full_name + len,
	     ".so",
	     4);
    }
  else
    full_name = s48_extract_byte_vector_readonly_2(call, name);

  handle = dlopen(full_name, DLOPEN_MODE);
  if (handle == NULL)
    s48_error_2(call, "shared_object_dlopen", (char *)dlerror(), 1, 
		s48_enter_byte_string_2(call, full_name));

  res = s48_make_value_2(call, void *);
  s48_unsafe_extract_value_2(call, res, void *) = handle;

  return res;
}

static s48_ref_t
shared_object_dlsym(s48_call_t call, s48_ref_t handle, s48_ref_t name)
{
  const char *error;
  void *entry;
  void *native_handle;
  s48_ref_t res;
  char *native_name;
  
  native_handle = s48_extract_value_2(call, handle, void *);

  native_name = s48_extract_byte_vector_readonly_2(call, name);

  entry = dlsym(native_handle, native_name);

  if (entry == NULL)
    s48_error_2(call, "shared_object_dlsym", (char*)dlerror(), 2, handle, name);

  res = s48_make_value_2(call, void *);
  s48_unsafe_extract_value_2(call, res, void *) = entry;
  return res;
}

static s48_ref_t
shared_object_dlclose(s48_call_t call, s48_ref_t handle)
{
  void *native_handle = s48_extract_value_2(call, handle, void *);
  
  if (dlclose(native_handle) < 0)
    s48_error_2(call, "shared_object_dlclose", (char*)dlerror(), 1, handle);
  return s48_unspecific_2(call);
}

typedef void (*thunk)();

static s48_ref_t
shared_object_call_thunk(s48_call_t call, s48_ref_t value)
{
  thunk entry;

  entry = s48_extract_value_2(call, value, thunk);
  entry();
  return s48_unspecific_2(call);
}

void
s48_init_dynlink(void)
{
  S48_EXPORT_FUNCTION(shared_object_dlopen);
  S48_EXPORT_FUNCTION(shared_object_dlsym);
  S48_EXPORT_FUNCTION(shared_object_dlclose);
  S48_EXPORT_FUNCTION(shared_object_call_thunk);
}
