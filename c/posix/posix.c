/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Mike Sperber, Will Noble
 */


extern void s48_init_posix_time(void);
extern void s48_init_posix_dir(void);
extern void s48_init_posix_user(void);
extern void s48_init_posix_regexp(void);
extern void s48_init_posix_proc_env(void);
extern void s48_init_posix_io(void);
extern void s48_init_posix_proc(void);
extern void s48_init_posix_errno(void);
extern void s48_init_posix_syslog(void);

void
s48_on_load(void)
{
  s48_init_posix_time();
  s48_init_posix_dir();
  s48_init_posix_dir();
  s48_init_posix_user();
  s48_init_posix_regexp();
  s48_init_posix_proc_env();
  s48_init_posix_io();
  s48_init_posix_proc();
  s48_init_posix_errno();
  s48_init_posix_syslog();
}

extern void s48_uninit_posix_proc(void);
extern void s48_uninit_posix_syslog(void);

void
s48_on_unload(void)
{
  s48_uninit_posix_proc();
  s48_uninit_posix_syslog();
}
