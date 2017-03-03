/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Richard Kelsey, Jonathan Rees, Marcus Crestani, Ivan Shmakov,
 * Mike Sperber
 */

/*
 * Scheme 48/POSIX I/O interface
 */

#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#include "scheme48.h"
#include "scheme48vm.h"		/* ps_close_fd() */
#include "posix.h"
#include "c-mods.h"
#include "unix.h"
#include "fd-io.h"

extern void		s48_init_posix_io(void);
static s48_ref_t	posix_dup(s48_call_t call, s48_ref_t channel, s48_ref_t new_mode),
			posix_dup2(s48_call_t call, s48_ref_t channel, s48_ref_t new_fd),
			posix_pipe(s48_call_t call),
			posix_close_on_exec_p(s48_call_t call, s48_ref_t channel),
			posix_set_close_on_exec(s48_call_t call, s48_ref_t channel,
						s48_ref_t close_p),
     			posix_io_flags(s48_call_t call, s48_ref_t channel, s48_ref_t options);

static s48_ref_t	s48_enter_file_options(s48_call_t call, int options);
  
/*
 * Record types imported from Scheme.
 */
static s48_ref_t	posix_file_options_enum_set_type_binding;

/*
 * Install all exported functions in Scheme48.
 */
void
s48_init_posix_io(void)
{
  S48_EXPORT_FUNCTION(posix_dup);
  S48_EXPORT_FUNCTION(posix_dup2);
  S48_EXPORT_FUNCTION(posix_pipe);
  S48_EXPORT_FUNCTION(posix_close_on_exec_p);
  S48_EXPORT_FUNCTION(posix_set_close_on_exec);
  S48_EXPORT_FUNCTION(posix_io_flags);

  posix_file_options_enum_set_type_binding =
    s48_get_imported_binding_2("posix-file-options-enum-set-type");
}

/*
 * Moves `channel' to a new file descriptor and returns a new channel that uses
 * `channel''s old file descriptor.
 *
 * Without all the error checking, this is:
 *   old_fd = channel_os_index(channel);
 *   new_fd = dup(old_fd);
 *   s48_set_channel_os_index(channel, new_fd);
 *   return s48_add_channel(old_fd);
 *
 */

static s48_ref_t
posix_dup(s48_call_t call, s48_ref_t channel, s48_ref_t new_mode)
{
  int		new_fd, old_fd, flags;
  long          status;
  s48_ref_t	s48_status;
  s48_ref_t	old_mode;
  s48_ref_t 	new_channel;

  if (!s48_channel_p_2(call, channel) ||
      s48_eq_p_2(call, s48_channel_status_2(call, channel), s48_channel_status_closed_2(call)))
    s48_assertion_violation_2(call, "posix_dup", "not an open channel", 1, channel);
  
  old_fd = s48_unsafe_extract_long_2(call, s48_unsafe_channel_os_index_2(call, channel));
  old_mode = s48_unsafe_channel_status_2(call, channel);

  RETRY_OR_RAISE_NEG(new_fd, dup(old_fd));
  
  s48_status = s48_set_channel_os_index_2(call, channel, new_fd);

  if (!s48_true_p_2(call, s48_status)) {
    ps_close_fd(new_fd);		/* retries if interrupted */
    s48_raise_scheme_exception_2(call, s48_extract_long_2(call, s48_status), 1, channel); }

  if (s48_eq_p_2(call, new_mode, s48_channel_status_output_2(call))
      && s48_eq_p_2(call, old_mode, s48_channel_status_input_2(call))) {
    RETRY_OR_RAISE_NEG(flags, fcntl(new_fd, F_GETFL));
    RETRY_OR_RAISE_NEG(status, fcntl(new_fd, F_SETFL, flags | O_NONBLOCK)); }

  new_channel = s48_add_channel_2(call,
				  s48_false_p_2(call, new_mode) ? old_mode : new_mode,
				  s48_unsafe_channel_id_2(call, channel),
				  old_fd);

  if (!s48_channel_p_2(call, new_channel)) {
    ps_close_fd(old_fd);		/* retries if interrupted */
    s48_raise_scheme_exception_2(call, s48_extract_long_2(call, new_channel), 1, channel); }

  return new_channel;
}

/*
 * Same again, except that we get told what the new file descriptor is to be.
 * We close the channel currently using that descriptor, if there be one.
 *
 * Without all the error checking, this is:
 *   old_fd = channel_os_index(channel);
 *   dup2(old_fd, new_fd);
 *   s48_set_channel_os_index(channel, new_fd);
 *   return s48_add_channel(old_fd);
 */

static s48_ref_t
posix_dup2(s48_call_t call, s48_ref_t channel, s48_ref_t new_fd)
{
  s48_ref_t 	new_channel;
  s48_ref_t	s48_status;
  int 		status;
  int 		new_c_fd, old_c_fd;

  if (!s48_channel_p_2(call, channel) ||
      s48_eq_p_2(call, s48_channel_status_2(call, channel), s48_channel_status_closed_2(call)))
    s48_assertion_violation_2(call, "posix_dup2", "not an open channel", 1, channel);

  if (!s48_fixnum_p_2(call, new_fd) || new_fd < 0)
    s48_assertion_violation_2(call, "posix_dup2", "fd not a nonnegative fixnum", 1, new_fd);

  old_c_fd = s48_extract_long_2(call, s48_unsafe_channel_os_index_2(call, channel));
  new_c_fd = s48_extract_long_2(call, new_fd);

  s48_close_channel(new_c_fd);

  RETRY_OR_RAISE_NEG(status, dup2(old_c_fd, new_c_fd));

  s48_status = s48_set_channel_os_index_2(call, channel, new_c_fd);

  if (!s48_true_p_2(call, s48_status)) {
    ps_close_fd(new_c_fd);		/* retries if interrupted */
    s48_raise_scheme_exception_2(call, s48_extract_long_2(call, s48_status), 1, channel); }

  new_channel = s48_add_channel_2(call,
				  s48_unsafe_channel_status_2(call, channel),
				  s48_unsafe_channel_id_2(call, channel),
				  old_c_fd);

  if (!s48_channel_p_2(call, new_channel)) {
    ps_close_fd(old_c_fd);		/* retries if interrupted */
    s48_raise_scheme_exception_2(call, s48_extract_long_2(call, new_channel), 1, channel); }

  return new_channel;
}

/*
 * Opens a pipe and returns a pair (<input-channel> . <output-channel>).
 *
 * Synopsis:
 *    int fds[2];
 *    pipe(fds);
 *    return s48_cons(s48_add_channel(fds[1]), s48_add_channel(fds[2]));
 */

static s48_ref_t
posix_pipe(s48_call_t call)
{  int 		fildes[2],
    		status;
  s48_ref_t	in_channel, out_channel;
  s48_ref_t 	id = s48_enter_string_latin_1_2 (call, "pipe");

  RETRY_OR_RAISE_NEG(status, pipe(fildes));

  in_channel = s48_add_channel_2(call, s48_channel_status_input_2(call), id, fildes[0]);

  if (!s48_channel_p_2(call, in_channel)) {
    ps_close_fd(fildes[0]);		/* retries if interrupted */
    ps_close_fd(fildes[1]);		/* retries if interrupted */
    s48_raise_scheme_exception_2(call, s48_extract_long_2(call, in_channel), 0); }

  RETRY_OR_RAISE_NEG(status, fcntl(fildes[1], F_SETFL, O_NONBLOCK));
  out_channel = s48_add_channel_2(call, s48_channel_status_output_2(call), id, fildes[1]);

  if (!s48_channel_p_2(call, out_channel)) {
    s48_close_channel(fildes[0]);
    ps_close_fd(fildes[1]);		/* retries if interrupted */
    s48_raise_scheme_exception_2(call, s48_extract_long_2(call, in_channel), 0); }

  return s48_cons_2(call, in_channel, out_channel);
}

static s48_ref_t
posix_close_on_exec_p(s48_call_t call, s48_ref_t channel)
{
  int	c_fd,
	status;

  if (!s48_channel_p_2(call, channel) ||
      s48_eq_p_2(call, 
		 s48_channel_status_2(call, channel),
		 s48_channel_status_closed_2(call)))
    s48_assertion_violation_2(call, "posix_close_on_exec_p", "not an open channel", 1, channel);
  
  c_fd = s48_unsafe_extract_long_2(call, s48_unsafe_channel_os_index_2(call, channel));

  RETRY_OR_RAISE_NEG(status, fcntl(c_fd, F_GETFD));

  return s48_enter_boolean_2(call, status & FD_CLOEXEC);
}

static s48_ref_t
posix_set_close_on_exec(s48_call_t call, s48_ref_t channel, s48_ref_t value)
{
  int	status, new_status;
  int	c_fd;

  if (!s48_channel_p_2(call, channel) ||
      s48_eq_p_2(call,
		 s48_channel_status_2(call, channel),
		 s48_channel_status_closed_2(call)))
    s48_assertion_violation_2(call, "posix_set_close_on_exec", "not an open channel", 1, channel);

  c_fd = s48_unsafe_extract_long_2(call, s48_unsafe_channel_os_index_2(call, channel));
  
  RETRY_OR_RAISE_NEG(status, fcntl(c_fd, F_GETFD));

  if (s48_extract_boolean_2(call, value))
    new_status = status | FD_CLOEXEC;
  else
    new_status = status & ! FD_CLOEXEC;
  
  if (new_status != status)
    RETRY_OR_RAISE_NEG(status, fcntl(c_fd, F_SETFD, new_status));
  
  return s48_unspecific_2(call);
}

static s48_ref_t
posix_io_flags(s48_call_t call, s48_ref_t channel, s48_ref_t options)
{
  int	status;
  int	c_fd;

  if (!s48_channel_p_2(call, channel) ||
      s48_eq_p_2(call,
		 s48_channel_status_2(call, channel),
		 s48_channel_status_closed_2(call)))
    s48_assertion_violation_2(call, "posix_io_flags", "not an open channel", 1, channel);

  c_fd = s48_unsafe_extract_long_2(call, s48_unsafe_channel_os_index_2(call, channel));

  if (s48_false_p_2(call, options)) {
  
    RETRY_OR_RAISE_NEG(status, fcntl(c_fd, F_GETFL));

    return s48_enter_file_options(call, status);
  }
  else {
    int c_options = s48_extract_file_options(call, options);

    RETRY_OR_RAISE_NEG(status, fcntl(c_fd, F_SETFL, c_options));
    
    return s48_unspecific_2(call);
  }
}

/* ************************************************************ */
/* File options.
 *
 * We translate the local bits into our own bits and vice versa.
 */

s48_ref_t
s48_enter_file_options(s48_call_t call, int file_options)
{
  s48_ref_t	sch_file_options;
  int		my_file_options;

  my_file_options =
    (O_CREAT    & file_options ? 00001 : 0) |
    (O_EXCL     & file_options ? 00002 : 0) |
    (O_NOCTTY   & file_options ? 00004 : 0) |
    (O_TRUNC    & file_options ? 00010 : 0) |
    (O_APPEND   & file_options ? 00020 : 0) |
    /* POSIX 2nd ed., not in Linux
    (O_DSYNC    & file_options ? 00040 : 0) |
    */
    (O_NONBLOCK & file_options ? 00100 : 0) |
    /* POSIX 2nd ed., not in Linux
    (O_RSYNC    & file_options ? 00200 : 0) |
    */
    /* Not in FreeBSD
    (O_SYNC     & file_options ? 00400 : 0) |
    */
    (O_RDONLY   & file_options ? 01000 : 0) |
    (O_RDWR     & file_options ? 02000 : 0) |
    (O_WRONLY   & file_options ? 04000 : 0);

  sch_file_options
    = s48_integer2enum_set_2(call, posix_file_options_enum_set_type_binding,
			     my_file_options);

  return sch_file_options;
}

int
s48_extract_file_options(s48_call_t call, s48_ref_t sch_file_options)
{
  int	c_file_options;
  long	file_options;

  s48_check_enum_set_type_2(call, sch_file_options,
			    posix_file_options_enum_set_type_binding);

  file_options = s48_enum_set2integer_2(call, sch_file_options);

  c_file_options =
    (00001 & file_options ? O_CREAT    : 0) |
    (00002 & file_options ? O_EXCL     : 0) |
    (00004 & file_options ? O_NOCTTY   : 0) |
    (00010 & file_options ? O_TRUNC    : 0) |
    (00020 & file_options ? O_APPEND   : 0) |
    /* POSIX 2nd ed., not in Linux
    (00040 & file_options ? O_DSYNC    : 0) |
    */
    (00100 & file_options ? O_NONBLOCK : 0) |
    /* POSIX 2nd ed., not in Linux
    (00200 & file_options ? O_RSYNC    : 0) |
    */
    /* Not in FreeBSD
    (00400 & file_options ? O_SYNC     : 0) |
    */
    (01000 & file_options ? O_RDONLY   : 0) |
    (02000 & file_options ? O_RDWR     : 0) |
    (04000 & file_options ? O_WRONLY   : 0);

  return c_file_options;
}

