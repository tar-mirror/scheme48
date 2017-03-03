/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Mike Sperber, Ivan Shmakov
 */

/*
 * If we don't have sysexits.h, we provide our own EX_... definitions.
 */
#if	defined(HAVE_SYSEXITS_H)
#include <sysexits.h>
#else

/* These are values on most Unices. */

#define EX_USAGE        64      /* command line usage error */
/* SRFI 22 assumes the following value. */
#define EX_SOFTWARE     70      /* internal software error */


#endif
