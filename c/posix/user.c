/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Marcus Crestani
 */

/*
 * Users and groups
 *
 * This provides Scheme access to the following: getgrgid(), getgrnam(),
 * getpwuid(), and getpwnam().
 *
 * The only externally visible bindings are s48_posix_init_user() and
 * s48_{enter|extract}_{u|g}id
 */

#include <stdio.h>
#include "scheme48.h"
#include <sys/types.h>
#include <stdio.h>
#include <errno.h>
#include <pwd.h>
#include <grp.h>
#include "posix.h"
#include "unix.h"

extern void		s48_posix_init_user(void);
static s48_ref_t	posix_getpwuid(s48_call_t call, s48_ref_t uid),
			posix_getpwnam(s48_call_t call, s48_ref_t user_name),
			posix_getgrgid(s48_call_t call, s48_ref_t gid),
			posix_getgrnam(s48_call_t call, s48_ref_t group_name);

/*
 * Record types imported from Scheme.
 */
static s48_ref_t	posix_user_id_type_binding,
                        posix_group_id_type_binding;

/*
 * Install all exported functions in Scheme48 and import and protect the
 * required record types.
 */
void
s48_init_posix_user(void)
{
  S48_EXPORT_FUNCTION(posix_getpwuid);
  S48_EXPORT_FUNCTION(posix_getpwnam);
  S48_EXPORT_FUNCTION(posix_getgrgid);
  S48_EXPORT_FUNCTION(posix_getgrnam);

  posix_user_id_type_binding = s48_get_imported_binding_2("posix-user-id-type");
    
  posix_group_id_type_binding = s48_get_imported_binding_2("posix-group-id-type");
}

/* ****************************************************************
 * Converting uids and gids back and forth between C and Scheme.
 */

/*
 * Convert a uid into a Scheme uid record.
 */
s48_ref_t
s48_enter_uid(s48_call_t call, uid_t uid)
{
  s48_ref_t	sch_uid;

  sch_uid = s48_make_record_2(call, posix_user_id_type_binding);
  s48_unsafe_record_set_2(call, sch_uid, 0, s48_enter_long_2(call, uid));

  return sch_uid;
}


/*
 * Convert a Scheme uid record into a uid_t.
 */
uid_t
s48_extract_uid(s48_call_t call, s48_ref_t uid)
{
  s48_check_record_type_2(call, uid, posix_user_id_type_binding);

  return s48_extract_long_2(call, s48_unsafe_record_ref_2(call, uid, 0));
}

/*
 * Convert a gid into a Scheme gid record.
 */
s48_ref_t
s48_enter_gid(s48_call_t call, gid_t gid)
{
  s48_ref_t	sch_gid;

  sch_gid = s48_make_record_2(call, posix_group_id_type_binding);
  s48_unsafe_record_set_2(call, sch_gid, 0, s48_enter_long_2(call, gid));

  return sch_gid;
}

/*
 * Convert a Scheme gid record into a gid_t.
 */
gid_t
s48_extract_gid(s48_call_t call, s48_ref_t gid)
{
  s48_check_record_type_2(call, gid, posix_group_id_type_binding);

  return s48_extract_long_2(call, s48_unsafe_record_ref_2(call, gid, 0));
}

/* ****************************************************************
 * Getting user and group information.
 */

static s48_ref_t enter_user_data(s48_call_t call, struct passwd *data);

static s48_ref_t
posix_getpwuid(s48_call_t call, s48_ref_t uid)
{
  struct passwd *data;
  
  RETRY_OR_RAISE_NULL(data, getpwuid(s48_extract_uid(call, uid)));

  return enter_user_data(call, data);
}

static s48_ref_t
posix_getpwnam(s48_call_t call, s48_ref_t name)
{
  struct passwd *data;
  
  RETRY_OR_RAISE_NULL(data, getpwnam(s48_extract_byte_vector_readonly_2(call, name)));

  return enter_user_data(call, data);
}

/*
 * returns a list of components
 */

static s48_ref_t
enter_user_data(s48_call_t call, struct passwd *data)
{
  s48_ref_t sch_data, temp;

  sch_data = s48_null_2(call);
  temp = s48_enter_byte_string_2(call, data->pw_shell);
  sch_data = s48_cons_2(call, temp, sch_data);
  temp = s48_enter_byte_string_2(call, data->pw_dir);
  sch_data = s48_cons_2(call, temp, sch_data);
  temp = s48_enter_gid(call, data->pw_gid);
  sch_data = s48_cons_2(call, temp, sch_data);
  temp = s48_enter_uid(call, data->pw_uid);
  sch_data = s48_cons_2(call, temp, sch_data);
  temp = s48_enter_byte_string_2(call, data->pw_name);
  sch_data = s48_cons_2(call, temp, sch_data);
  
  return sch_data;
}

static s48_ref_t
enter_group_data(s48_call_t call, struct group *data)
{
  s48_ref_t sch_data, members, temp;
  int length;
  char **names;

  for(length = 0, names = data->gr_mem; *names != NULL; length++, names++);
  members = s48_make_vector_2(call, length, s48_false_2(call));
  for(length = 0, names = data->gr_mem; *names != NULL; length++, names++) {
    temp = s48_enter_byte_string_2(call, *names);
    s48_unsafe_vector_set_2(call, members, length, temp);
  }

  sch_data = s48_null_2(call);
  sch_data = s48_cons_2(call, members, sch_data);
  temp = s48_enter_gid(call, data->gr_gid);
  sch_data = s48_cons_2(call, temp, sch_data);
  temp = s48_enter_byte_string_2(call, data->gr_name);
  sch_data = s48_cons_2(call, temp, sch_data);
  
  return sch_data;
}

static s48_ref_t
posix_getgrgid(s48_call_t call, s48_ref_t gid)
{
  struct group *data;
  
  RETRY_OR_RAISE_NULL(data, getgrgid(s48_extract_gid(call, gid)));

  return enter_group_data(call, data);
}

static s48_ref_t
posix_getgrnam(s48_call_t call, s48_ref_t name)
{
  struct group *data;
  
  RETRY_OR_RAISE_NULL(data, getgrnam(s48_extract_byte_vector_readonly_2(call, name)));

  return enter_group_data(call, data);
}
