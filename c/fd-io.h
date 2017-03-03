/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Richard Kelsey, Jonathan Rees, Mike Sperber
 */

#define STDIN_FD() 0
#define STDOUT_FD() 1
#define STDERR_FD() 2

S48_EXTERN int ps_open_fd(char *in_filename, psbool is_input, long *status);

S48_EXTERN int ps_close_fd(long fd_as_long);

S48_EXTERN psbool ps_check_fd(long fd_as_long, psbool is_read, long *status);

S48_EXTERN long ps_read_fd(long fd_as_long, char *buf_as_long, long max, psbool waitp,
		       psbool *eofp, psbool *pending, long *status);

S48_EXTERN long ps_write_fd(long fd_as_long, char *buf_as_long, long max,
			psbool *pending, long *status);

S48_EXTERN long ps_abort_fd_op(long fd_as_long);

S48_EXTERN long ps_io_buffer_size(void);

S48_EXTERN psbool ps_io_crlf_p(void);

S48_EXTERN char *ps_console_encoding(long fd_as_long);

