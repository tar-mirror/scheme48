/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: David Frese, Marcus Crestani, Robert Ransom
 */

#include "utils.h"
#include "gc_config.h"
#include <stdarg.h>
#include <stdlib.h>
#include <assert.h>

void s48_gc_error(const char* message, ...) {
  va_list args;
  va_start(args, message);
  fprintf(stderr, "gc: ");
  vfprintf(stderr, message, args);
  fprintf(stderr, "\n");
  va_end(args);
  abort();
  exit(-1);
}

#if (BIBOP_LOG)
void s48_bibop_log(const char* message, ...) {
  FILE* prot;
  va_list args;

  prot = fopen("BIBOP_LOG", "a");
  va_start(args, message);

  vfprintf(prot, message, args);
  fprintf(prot, "\n");

  va_end(args);

  fclose(prot);
}
#endif
