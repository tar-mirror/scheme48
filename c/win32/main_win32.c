/* Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Mike Sperber, Marcus Crestani, Robert Ransom
 */

#include <stdlib.h>
#include <windows.h>

extern long s48_initialize(int *argc, char ***argv);
extern long s48_call_startup_procedure(char **, long);

extern int s48_utf_16_to_utf_8of16(LPWSTR utf_16, 
				   unsigned char* utf_8of16);

int
wmain(int argc, wchar_t *argv_utf16[])
{
  char** argv = malloc(sizeof(char*) * argc);
  int i = 0;
  long return_value;

  if (argv == NULL)
    return -1;

  while (i < argc)
    {
      int size = s48_utf_16_to_utf_8of16(argv_utf16[i], NULL);
      argv[i] = malloc(size + 1);
      if (argv[i] == NULL)
	return -1;
      s48_utf_16_to_utf_8of16(argv_utf16[i], argv[i]);
      ++i;
    }

  return_value = s48_initialize(&argc, &argv);

  if (return_value != 0)
    return return_value;
  else
    return s48_call_startup_procedure(argv, argc);
}
