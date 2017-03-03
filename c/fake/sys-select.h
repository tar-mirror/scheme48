/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Richard Kelsey, Jonathan Rees
 */

/*
 * If we have a sys/select.h, then include it.
 */
#if	defined(HAVE_SYS_SELECT_H)

#include <sys/types.h>
#include <sys/select.h>

#endif
