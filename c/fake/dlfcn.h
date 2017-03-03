/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Richard Kelsey, Jonathan Rees, Mike Sperber
 */

/*
 * This include file is for systems which do not have dynamic loading.
 */
#if ! defined(HAVE_DLOPEN)

extern void	*dlopen(char *filename, int flags);
extern char	*dlerror(void);
extern void	*dlsym(void *lib, char *name);
extern int	dlclose(void *lib);

#endif
