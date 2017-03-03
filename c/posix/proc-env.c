/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Marcus Crestani,
 * Roderic Morris, Will Noble
 */

/*
 * Scheme 48/POSIX process environment interface
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <sys/wait.h>

#include "scheme48.h"
#include "posix.h"
#include "unix.h"
#include "sysdep.h"

extern void		s48_init_posix_proc_env(void);
static s48_ref_t	posix_get_pid(s48_call_t call, s48_ref_t parent_p),
			posix_get_id(s48_call_t call, s48_ref_t user_p, s48_ref_t real_p),
			posix_set_id(s48_call_t call, s48_ref_t user_p,
				     s48_ref_t real_p, s48_ref_t id),
			posix_get_groups(s48_call_t call),
			posix_get_login(s48_call_t call),
			posix_set_sid(s48_call_t call),
			posix_sys_name(s48_call_t call, s48_ref_t which),
			posix_get_env(s48_call_t call, s48_ref_t name),
			posix_set_env(s48_call_t call, s48_ref_t name, s48_ref_t value),
			posix_get_env_alist(s48_call_t call),
  			posix_get_terminal_pathname(s48_call_t call),
			posix_tty_name(s48_call_t call, s48_ref_t channel),
			posix_is_a_tty(s48_call_t call, s48_ref_t channel);

/*
 * Install all exported functions in Scheme48.
 */
void
s48_init_posix_proc_env(void)
{
  S48_EXPORT_FUNCTION(posix_get_pid);
  S48_EXPORT_FUNCTION(posix_get_id);
  S48_EXPORT_FUNCTION(posix_set_id);
  S48_EXPORT_FUNCTION(posix_get_groups);
  S48_EXPORT_FUNCTION(posix_get_login);
  S48_EXPORT_FUNCTION(posix_set_sid);
  S48_EXPORT_FUNCTION(posix_sys_name);
  S48_EXPORT_FUNCTION(posix_get_env);
  S48_EXPORT_FUNCTION(posix_set_env);
  S48_EXPORT_FUNCTION(posix_get_env_alist);
  S48_EXPORT_FUNCTION(posix_get_terminal_pathname);
  S48_EXPORT_FUNCTION(posix_tty_name);
  S48_EXPORT_FUNCTION(posix_is_a_tty);
}

/*
 * Lots of simple little functions.
 */

static s48_ref_t
posix_get_pid(s48_call_t call, s48_ref_t parent_p)
{
  extern char going;
  going = 1 == 0;
  return s48_enter_long_2(call,
			  s48_extract_boolean_2(call, parent_p) ?
			  getppid() :
			  getpid());
}

static s48_ref_t
posix_set_sid(s48_call_t call)
{
  pid_t pid;

  RETRY_OR_RAISE_NEG(pid, setsid());

  return s48_enter_long_2(call, pid);
}

static s48_ref_t
posix_get_id(s48_call_t call, s48_ref_t user_p, s48_ref_t real_p)
{
  if (s48_extract_boolean_2(call, user_p))
    return s48_enter_uid(call, s48_extract_boolean_2(call, real_p) ? getuid() : geteuid());
  else
    return s48_enter_gid(call, s48_extract_boolean_2(call, real_p) ? getgid() : getegid());
}

static s48_ref_t
posix_set_id(s48_call_t call, s48_ref_t user_p, s48_ref_t real_p, s48_ref_t id)
{
  int status;

  if (s48_extract_boolean_2(call, user_p))
    RETRY_OR_RAISE_NEG(status,
                       s48_extract_boolean_2(call, real_p) ?
                       setuid(s48_extract_uid(call, id)) :
                       seteuid(s48_extract_uid(call, id)));
  else
    RETRY_OR_RAISE_NEG(status,
                       s48_extract_boolean_2(call, real_p) ?
                       setgid(s48_extract_gid(call, id)) :
                       setegid(s48_extract_gid(call, id)));

  return s48_unspecific_2(call);
}

static s48_ref_t
posix_get_login(s48_call_t call)
{
  char *login = getlogin();

  return (login == NULL) ? s48_false_2(call) : s48_enter_byte_string_2(call, login);
}

static s48_ref_t
posix_get_env(s48_call_t call, s48_ref_t name)
{
  char *value;

  value = getenv(s48_extract_byte_vector_readonly_2(call, name));

  return (value == NULL) ? s48_false_2(call) : s48_enter_byte_string_2(call, value);
}

static s48_ref_t
posix_set_env(s48_call_t call, s48_ref_t name, s48_ref_t value)
{
  int status;

  RETRY_OR_RAISE_NEG(status,
                     setenv(s48_extract_byte_vector_readonly_2(call, name),
                            s48_extract_byte_vector_readonly_2(call, value), 1));

  return s48_unspecific_2(call);
}


/*
 * Here we turn an array of strings of the form "name=value" into a list
 * of pairs ("name" . "value").
 */

static s48_ref_t
posix_get_env_alist(s48_call_t call)
{
  extern char **ENVIRON_NAME;
  char **c_env = ENVIRON_NAME;
  s48_ref_t sch_env = s48_null_2(call);
  s48_ref_t name;

  for(; *c_env != NULL; c_env++) {
    char *entry = *c_env;
    s48_ref_t value;
    char *name_end = strchr(entry, '=');

    name = s48_enter_byte_substring_2(call, entry, name_end - entry);
    value = s48_enter_byte_substring_2(call, name_end + 1, strlen(name_end + 1));
    sch_env = s48_cons_2(call, s48_cons_2(call, name, value), sch_env); }

  return sch_env;
}

/*
 * Again we turn an array into a list.
 */

static s48_ref_t
posix_get_groups(s48_call_t call)
{
  int status, count, i;
  gid_t *grouplist;
  s48_ref_t groups = s48_null_2(call);
  s48_ref_t temp;

  count = getgroups(0, (gid_t *)NULL);

  grouplist = (gid_t *) malloc(count * sizeof(gid_t));

  if (grouplist == NULL)
    s48_out_of_memory_error_2(call);

  RETRY_NEG(status, getgroups(count, grouplist));

  if (status == -1) {
    free(grouplist);
    s48_os_error_2(call, "posix_get_groups", errno, 0); }

  for(i = count - 1; i > -1; i--) {
    temp = s48_enter_gid(call, grouplist[i]);
    groups = s48_cons_2(call, temp, groups);
  }

  free(grouplist);

  return groups;
}


/*
 * uname() - we could define a record for this, but it seems like overkill.
 */

static s48_ref_t
posix_sys_name(s48_call_t call, s48_ref_t which)
{
  struct utsname names;
  char *value;
  int status;

  RETRY_OR_RAISE_NEG(status, uname(&names));

  switch (s48_extract_long_2(call, which)) {
  case 0: value = names.sysname; break;
  case 1: value = names.nodename; break;
  case 2: value = names.release; break;
  case 3: value = names.version; break;
  default: value = names.machine;
  }

  return s48_enter_string_latin_1_2(call, value);
}

/*
 * Terminals
 */

static s48_ref_t
posix_get_terminal_pathname(s48_call_t call)
{
  char termid[L_ctermid];
  char *status =  ctermid(termid);

  return (*status == '\0') ? s48_false_2(call) : s48_enter_byte_string_2(call, termid);
}

static s48_ref_t
posix_tty_name(s48_call_t call, s48_ref_t channel)
{
  char *name;

  name = ttyname(s48_unsafe_extract_long_2(call, s48_channel_os_index_2(call, channel)));

  return (name == NULL) ? s48_false_2(call) : s48_enter_byte_string_2(call, name);
}

static s48_ref_t
posix_is_a_tty(s48_call_t call, s48_ref_t channel)
{
  return s48_enter_boolean_2(call,
			     isatty(s48_unsafe_extract_long_2(call,
							      s48_channel_os_index_2(call, channel))));
}

