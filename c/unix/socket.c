/* Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Marcus Crestani
 */

#define NO_OLD_FFI 1

/*
 * Unix-specific sockets stuff.
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#ifdef HAVE_PTHREAD_H
#include <pthread.h>
#endif

#include <scheme48.h>

#include "c-mods.h"
#include "unix.h"
#include "fd-io.h"		/* ps_close_fd() */
#include "event.h"		/* add_pending_fd() */

#include "sysdep.h"

#include "socket.h"
#include "address.h"

static s48_ref_t
s48_socket(s48_call_t call, s48_ref_t sch_af, s48_ref_t sch_type, s48_ref_t sch_protocol)
{
  socket_t fd;
  int mode, status;
  s48_ref_t sch_channel;
  int af = s48_extract_af(call, sch_af);
  int socktype = s48_extract_socket_type(call, sch_type);
  int protocol = s48_extract_long_2(call, sch_protocol);

  RETRY_OR_RAISE_NEG(fd, socket(af, socktype, protocol));
  RETRY_OR_RAISE_NEG(status, fcntl(fd, F_SETFL, O_NONBLOCK));

  sch_channel = s48_add_channel_2(call, s48_channel_status_special_input_2(call),
				  s48_enter_string_latin_1_2(call, "socket"), fd);

  if (!s48_channel_p_2(call, sch_channel))
    {
      ps_close_fd(fd);		/* retries if interrupted */
      s48_raise_scheme_exception_2(call, s48_extract_long_2(call, sch_channel), 0);
    }
  
  return sch_channel;
}

static s48_ref_t
s48_socketpair(s48_call_t call, s48_ref_t sch_af, s48_ref_t sch_type, s48_ref_t sch_protocol)
{
  int status;
  s48_ref_t sch_channel0, sch_channel1;
  s48_ref_t sch_result;
  int af = s48_extract_af(call, sch_af);
  int socktype = s48_extract_socket_type(call, sch_type);
  int protocol = s48_extract_long_2(call, sch_protocol);
  socket_t fds[2];

  RETRY_OR_RAISE_NEG(status, socketpair(af, socktype, protocol, fds));

  RETRY_OR_RAISE_NEG(status, fcntl(fds[0], F_SETFL, O_NONBLOCK));
  RETRY_OR_RAISE_NEG(status, fcntl(fds[1], F_SETFL, O_NONBLOCK));

  sch_channel0 = s48_add_channel_2(call, s48_channel_status_input_2(call),
				   s48_enter_string_latin_1_2(call, "socket"), fds[0]);
  sch_channel1 = s48_add_channel_2(call, s48_channel_status_input_2(call),
				   s48_enter_string_latin_1_2(call, "socket"), fds[1]);

  
  sch_result = s48_cons_2(call, sch_channel0, sch_channel1);
  return sch_result;
}

/*
 * dup() `socket_fd' and return an output channel holding the result.
 *
 * We have to versions, one for calling from C and one for calling from Scheme.
 */

static s48_ref_t
dup_socket_channel(s48_call_t call, socket_t socket_fd)
{
  socket_t output_fd;
  s48_ref_t output_channel;
  int flags;

  RETRY_OR_RAISE_NEG(output_fd, dup(socket_fd));

  RETRY_OR_RAISE_NEG(flags, fcntl(output_fd, F_GETFL));
  flags |= O_NONBLOCK;
  RETRY_OR_RAISE_NEG(flags, fcntl(output_fd, F_SETFL, flags));
  
  output_channel = s48_add_channel_2(call, s48_channel_status_output_2(call),
				     s48_enter_string_latin_1_2(call, "socket connection"),
				     output_fd);
  
  if (!s48_channel_p_2(call, output_channel))
    {
      ps_close_fd(output_fd);		/* retries if interrupted */
      s48_raise_scheme_exception_2(call, s48_extract_long_2(call, output_channel), 0);
    };
  
  return output_channel;
}

socket_t
s48_extract_socket_fd(s48_call_t call, s48_ref_t sch_channel)
{
  s48_check_channel_2(call, sch_channel);
  return s48_extract_long_2(call, s48_unsafe_channel_os_index_2(call, sch_channel));
}

static s48_ref_t
s48_dup_socket_channel(s48_call_t call, s48_ref_t sch_channel)
{
  return dup_socket_channel(call, s48_extract_socket_fd(call, sch_channel));
}

/*
 * Given a bound socket, accept a connection and return a pair of the
 * input channel and the raw socket address.
 *
 * If the accept fails because the client hasn't connected yet, then we
 * return #f.
 *
 * If it fails for any other reason, then an exception is raised.
 */

static s48_ref_t
s48_accept(s48_call_t call, s48_ref_t sch_channel, s48_ref_t sch_retry_p)
{
  socket_t socket_fd = s48_extract_socket_fd(call, sch_channel);
  socket_t connect_fd;
  int status;
  struct sockaddr_storage address;
  socklen_t len;
  s48_ref_t input_channel, output_channel;

  len = sizeof(address);
  
  connect_fd = accept(socket_fd, (struct sockaddr *)&address, &len);
  
  if (connect_fd >= 0) {
    
    RETRY_OR_RAISE_NEG(status, fcntl(connect_fd, F_SETFL, O_NONBLOCK));

    input_channel = s48_add_channel_2(call, s48_channel_status_input_2(call),
				      s48_enter_string_latin_1_2(call, "socket connection"),
				      connect_fd);

    if (!s48_channel_p_2(call, input_channel))
      {
	ps_close_fd(connect_fd);		/* retries if interrupted */
	s48_raise_scheme_exception_2(call, s48_extract_long_2(call, input_channel), 0);
      }
    
    return s48_cons_2(call,
		      input_channel,
		      s48_enter_sockaddr(call, (const struct sockaddr*)&address, len));
  }

  /*
   * Check for errors.  If we need to retry we mark the socket as pending
   * and return #F to tell the Scheme procedure to wait.
   */

  if ((errno != EWOULDBLOCK) && (errno != EINTR) && (errno != EAGAIN))
    s48_os_error_2(call, "s48_accept", errno, 2, sch_channel, sch_retry_p);

  if (! s48_add_pending_fd(socket_fd, PSTRUE))
    s48_out_of_memory_error_2(call);

  return s48_false_2(call);
}

/*
 * Given a socket and an address, connect the socket.
 *
 * If this succeeds, it returns an output channel for the connection.
 * If it fails because the connect would block, add the socket to the
 * pending queue (for output) and return #f.
 * If it fails for any other reason, raise an exception.
 */

static s48_ref_t
s48_connect(s48_call_t call, s48_ref_t sch_channel,
	    s48_ref_t sch_address, s48_ref_t sch_retry_p)
{
  socket_t socket_fd = s48_extract_socket_fd(call, sch_channel);

  /*
   * Try the connection.  If it works we make an output channel and return it.
   * The original socket channel will be used as the input channel.
   *
   * FreeBSD's connect() behaves oddly.  If you get told to wait, wait for
   * select() to signal the all-clear, and then try to connect again, you
   * get an `already connected' (EISCONN) error.  To handle this we pass in
   * a retry_p flag.  If retry_p is  true the `already connected' error is
   * ignored.
   */

  if (connect(socket_fd,
	      s48_extract_value_pointer_2(call, sch_address, struct sockaddr),
	      s48_value_size_2(call, sch_address)) >= 0
      || ((errno == EISCONN) && (s48_true_p_2(call, sch_retry_p))))
    {
      s48_unsafe_stob_set_2(call, sch_channel,
			    s48_channel_status_offset, s48_channel_status_input_2(call));
      return dup_socket_channel(call, socket_fd);
    }
    
  /*
   * Check for errors.  If we need to retry we mark the socket as pending
   * and return #F to tell the Scheme procedure to wait.
   */

  /* already connected, will raise an error from Scheme */
  if (errno == EISCONN)
    return s48_true_2(call);

  if (errno != EWOULDBLOCK && errno != EINTR && errno != EALREADY
      && errno != EINPROGRESS && errno != EAGAIN)
    s48_os_error_2(call, "s48_connect", errno, 3, sch_channel, sch_address, sch_retry_p);

  if (! (s48_add_pending_fd(socket_fd, PSFALSE)))
    s48_out_of_memory_error_2(call);

  return s48_false_2(call);
}

/*
 * Receive a message.  Returns pair (<byte-count> . <sender>) or just
 * <byte-count> if want_sender_p is false.
 */

static s48_ref_t
s48_recvfrom(s48_call_t call, s48_ref_t sch_channel,
	     s48_ref_t sch_buffer, s48_ref_t sch_start, s48_ref_t sch_count,
	     s48_ref_t sch_flags,
	     s48_ref_t sch_want_sender_p,
	     s48_ref_t sch_retry_p)
{
  socket_t socket_fd = s48_extract_socket_fd(call, sch_channel);
  int want_sender_p = !(s48_false_p_2(call, sch_want_sender_p));
  struct sockaddr_storage from;
  socklen_t from_len = (want_sender_p ? sizeof(struct sockaddr_storage) : 0);
  int flags = s48_extract_msg_flags(call, sch_flags);
  size_t buffer_size = s48_byte_vector_length_2(call, sch_buffer);
  size_t start = s48_extract_unsigned_long_2(call, sch_start);
  size_t count = s48_extract_unsigned_long_2(call, sch_count);
  ssize_t status;

  if ((start + count) > buffer_size)
    s48_assertion_violation_2(call, "s48_sendto", "buffer start or count is wrong", 3,
			      sch_buffer, sch_start, sch_count);

  status = recvfrom(socket_fd,
		    s48_extract_byte_vector_2(call, sch_buffer) + start,
		    count,
		    flags,
		    want_sender_p ? (struct sockaddr*)&from : NULL,
		    &from_len);
  
  if (0 <= status)
    {
    if (want_sender_p)
      {
	s48_ref_t sch_count, sch_saddr;
	s48_ref_t sch_result;
	sch_count = s48_enter_unsigned_long_2(call, status);
	sch_saddr = s48_enter_sockaddr(call, (struct sockaddr *)&from, from_len);
	sch_result = s48_cons_2(call, sch_count, sch_saddr);
	return sch_result;
      }
    else
      return s48_enter_unsigned_long_2(call, status);
    }
  
  /*
   * Check for errors.  If we need to retry we mark the socket as pending
   * and return #F to tell the Scheme procedure to wait.
   */
  
  if (errno != EWOULDBLOCK && errno != EINTR && errno != EALREADY
      && errno != EINPROGRESS && errno != EAGAIN)
    s48_os_error_2(call, "s48_recv", errno, 6,
		   sch_channel, sch_buffer, sch_start, sch_count,
		   sch_flags, sch_want_sender_p);

  if (! (s48_add_pending_fd(socket_fd, PSTRUE)))
    s48_out_of_memory_error_2(call);

  return s48_false_2(call);
}

static s48_ref_t
s48_sendto(s48_call_t call, s48_ref_t sch_channel,
	   s48_ref_t sch_buffer, s48_ref_t sch_start, s48_ref_t sch_count,
	   s48_ref_t sch_flags,
	   s48_ref_t sch_saddr,
	   s48_ref_t sch_retry_p) /* ignored on Unix */
{
  socket_t socket_fd = s48_extract_socket_fd(call, sch_channel);
  ssize_t sent;
  const struct sockaddr *sa
    = s48_extract_value_pointer_2(call, sch_saddr, const struct sockaddr);
  socklen_t salen = s48_value_size_2(call, sch_saddr);
  int flags = s48_extract_msg_flags(call, sch_flags);
  size_t buffer_size = s48_byte_vector_length_2(call, sch_buffer);
  size_t start = s48_extract_unsigned_long_2(call, sch_start);
  size_t count = s48_extract_unsigned_long_2(call, sch_count);

  if ((start + count) > buffer_size)
    s48_assertion_violation_2(call, "s48_sendto", "buffer start or count is wrong", 3,
			      sch_buffer, sch_start, sch_count);
  
  sent = sendto(socket_fd,
		s48_extract_byte_vector_readonly_2(call, sch_buffer) + start,
		count,
		flags,
		(struct sockaddr *) sa, salen);
  
  if (0 <= sent)
    return s48_enter_unsigned_long_2(call, sent);

  /*
   * Check for errors.  If we need to retry we mark the socket as pending
   * and return #F to tell the Scheme procedure to wait.
   */
  
  if (errno != EWOULDBLOCK && errno != EINTR && errno != EALREADY
      && errno != EINPROGRESS && errno != EAGAIN)
    s48_os_error_2(call, "s48_sendto", errno, 6,
		 sch_channel, sch_saddr, sch_flags, sch_buffer, sch_start, sch_count);

  if (! (s48_add_pending_fd(socket_fd, PSFALSE)))
    s48_out_of_memory_error_2(call);

  return s48_false_2(call);
}

void
s48_init_os_sockets(void)
{
  S48_EXPORT_FUNCTION(s48_socket);
  S48_EXPORT_FUNCTION(s48_socketpair);
  S48_EXPORT_FUNCTION(s48_dup_socket_channel);
  S48_EXPORT_FUNCTION(s48_accept);
  S48_EXPORT_FUNCTION(s48_connect);
  S48_EXPORT_FUNCTION(s48_recvfrom);
  S48_EXPORT_FUNCTION(s48_sendto);
}
