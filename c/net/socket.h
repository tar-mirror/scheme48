/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Mike Sperber, Marcus Crestani
 */

#ifndef __NET_SOCKET_H__
#define __NET_SOCKET_H__

#ifdef _WIN32
#include <winsock2.h>
#include <mswsock.h>
#else
#include <sys/types.h>
#include <sys/socket.h>
#endif

#include <scheme48.h>

/* OS-unspecific */

int s48_extract_msg_flags(s48_call_t call, s48_ref_t sch_flags);

/* OS-specific */

#ifdef _WIN32
typedef SOCKET socket_t;
#else
typedef int socket_t;
#endif

socket_t s48_extract_socket_fd(s48_call_t call, s48_ref_t sch_channel);

#endif
