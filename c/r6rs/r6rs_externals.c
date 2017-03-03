/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Harald Glab-Phlag, Mike Sperber
 */

extern void s48_init_ieee_bytevect(void);

void s48_on_load(void)
{
  s48_init_ieee_bytevect();
}
