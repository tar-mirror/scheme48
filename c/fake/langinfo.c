/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Mike Sperber
 */

/*
 * If the system doesn't have an nl_langinfo procedure, we provide our
 * own dummy version.
 */

#include "sysdep.h"

#include <stdio.h>
#include <stdlib.h>

char *
nl_langinfo(nl_item item)
{
  if (item == CODESET)
    return "ASCII";
  else
    {
      fprintf(stderr, "unknown nl_item argument to nl_langinfo: %d", item);
      exit(-1);
    }
}
