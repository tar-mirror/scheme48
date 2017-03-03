/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Richard Kelsey, Jonathan Rees
 */

/*
 * If we don't have strerror(), we fake it using sys_nerr and sys_errlist.
 */
#if	! defined(HAVE_STRERROR)

extern char	*strerror(int errnum);

#endif
