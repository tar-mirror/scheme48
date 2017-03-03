/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Mike Sperber, Marcus Crestani
 */

#ifndef __NET_ADDRESS_H__
#define __NET_ADDRESS_H__

#ifdef _WIN32
#include <winsock2.h>
#include <mswsock.h>
typedef int sa_family_t;
typedef unsigned char uint8_t;
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#endif

#include <scheme48.h>

extern sa_family_t s48_extract_af(s48_call_t call, s48_ref_t sch_af_val);
extern s48_ref_t s48_enter_af(s48_call_t call, sa_family_t af);
extern int s48_extract_socket_type(s48_call_t call, s48_ref_t sch_socktype);
extern s48_ref_t s48_enter_socket_type(s48_call_t call, int socktype);
extern s48_ref_t s48_enter_sockaddr(s48_call_t call, const struct sockaddr* sa, socklen_t salen);
extern s48_ref_t s48_enter_in6_addr(s48_call_t call, const struct in6_addr* addr);
extern void s48_extract_in6_addr(s48_call_t call, s48_ref_t sch_addr, struct in6_addr* addr);

#endif
