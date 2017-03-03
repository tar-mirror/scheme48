/* Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Mike Sperber, Marcus Crestani
 */

#define NO_OLD_FFI 1

/*
 * Load DLLs on Windows.
 */

#include <windows.h>
#include "scheme48.h"
#include "io.h"

extern int s48_utf_8of16_to_utf_16(const unsigned char* utf_8of16,
				   LPWSTR utf_16,
				   int* errorp);

static s48_ref_t
shared_object_dlopen(s48_call_t call, s48_ref_t name, s48_ref_t complete_name_p)
{
  HINSTANCE handle;
  s48_ref_t res;
  char *full_name;
  WCHAR* name_utf16;
  size_t len = strlen(s48_extract_byte_vector_readonly_2(call, name));

  if (!s48_false_p_2(call, complete_name_p))
    {
      full_name = s48_make_local_buf(call, len + 5);
      memcpy(full_name,
	     s48_extract_byte_vector_readonly_2(call, name),
	     len);
      memcpy(full_name + len,
	     ".dll",
	     5);
      len += 4;
    }
  else
    full_name = s48_extract_byte_vector_readonly_2(call, name);

  name_utf16 = malloc(sizeof(WCHAR) * (len + 1));
  if (name_utf16 == NULL)
    s48_out_of_memory_error_2(call);
  s48_utf_8of16_to_utf_16(full_name, name_utf16, NULL);

  handle = LoadLibraryW(name_utf16);

  free(name_utf16);
  if (handle == NULL)
    s48_os_error_2(call, "shared_object_dlopen", GetLastError(), 1, name);

  res = s48_make_value_2(call, HINSTANCE);
  s48_set_value_2(call, res, HINSTANCE, handle);

  return res;
}

static s48_ref_t
shared_object_dlsym(s48_call_t call, s48_ref_t handle, s48_ref_t name)
{
  void *entry;
  HINSTANCE native_handle;
  char *native_name;
  
  native_handle = s48_extract_value_2(call, handle, HINSTANCE);
  native_name = s48_extract_byte_vector_readonly_2(call, name);

  entry = GetProcAddress(native_handle, native_name);

  if (entry == NULL)
    s48_os_error_2(call, "shared_object_dlsym", GetLastError(), 2,
		   handle, name);

  return s48_enter_pointer_2(call, entry);
}

static s48_ref_t
shared_object_dlclose(s48_call_t call, s48_ref_t handle)
{
  HINSTANCE native_handle = s48_extract_value_2(call, handle, HINSTANCE);
  
  if (!FreeLibrary(native_handle) < 0)
    s48_os_error_2(call, "shared_object_dlclose", GetLastError(), 1, handle);
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
