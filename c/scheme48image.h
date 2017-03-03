/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Richard Kelsey, Jonathan Rees, Mike Sperber
 */

/*
 * Externally visible objects defined in scheme48read-image.c and
 * scheme48write-image.c.
 */

extern char	s48_image_writing_okayP(void);
extern long	s48_write_image(long, long, FILE *);
extern long	s48_read_image(char *, long);

extern long	s48_startup_procedure(void);
extern long	s48_initial_symbols(void);
extern long	s48_initial_imported_bindings(void);
extern long	s48_initial_exported_bindings(void);
extern long	s48_resumer_records(void);

extern void	s48_initialization_completeB(void);
extern void	s48_initializing_gc_root(void);

extern void     s48_set_image_valuesB(long, long, long, long, long);
