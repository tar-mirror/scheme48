/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Richard Kelsey, Jonathan Rees, Mike Sperber
 */

#include <stdio.h>

extern FILE	*ps_open_input_file(char *, long *);
extern FILE	*ps_open_output_file(char *, long *);
extern long	ps_close(FILE *);
extern char	ps_read_char(FILE *, char *, long *, char);
extern long	ps_read_integer(FILE *, char *, long *);
extern long	ps_write_char(char, FILE *);
extern long	ps_write_integer(long, FILE *);
extern long	ps_write_string(char *, FILE *);
extern long	ps_read_block(FILE *, char *, long, char *, long *);
extern long	ps_write_block(FILE *, char *, long);
extern char	*ps_error_string(long);
extern void	ps_error(char *, long count, ...);
