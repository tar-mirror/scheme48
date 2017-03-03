/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Mike Sperber
 */

extern void s48_init_net_addresses(void);
extern void s48_init_net_sockets(void);

void
s48_init_net(void)
{
  s48_init_net_addresses();
  s48_init_net_sockets();
}
