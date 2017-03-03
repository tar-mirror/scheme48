/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Richard Kelsey, Jonathan Rees, Marcus Crestani, Mike Sperber,
 * Robert Ransom
 */

/*
 * An interface to Unix opendir(), readdir(), closedir(),
 * stat() and lstat().
 * Note, readdir() returns #F on EOF.
 * Note, calling closedir on an already closed directory has no effect.
 * Note, readdir will never return "." or ".." (POSIX leaves this
 * unspecified).
 * Note, a stat object which is written out to a dump and used on a
 * different OS will cause problems because things have moved around.
 */

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <dirent.h>
#include <time.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "scheme48.h"
#include "scheme48vm.h"		/* ps_close_fd() */
#include "posix.h"
#include "c-mods.h"
#include "unix.h"
#include "fd-io.h"

extern void		s48_init_posix_dir(void);

static s48_ref_t	posix_opendir(s48_call_t call, s48_ref_t svname),
			posix_closedir(s48_call_t call, s48_ref_t svdir),
			posix_readdir(s48_call_t call, s48_ref_t svdir),
			posix_working_directory(s48_call_t call, s48_ref_t new_wd),
			posix_open(s48_call_t call, s48_ref_t path, s48_ref_t id, s48_ref_t options,
				   s48_ref_t mode, s48_ref_t input_p),
			posix_file_stuff(s48_call_t call, s48_ref_t op, s48_ref_t arg1,
					 s48_ref_t arg2),
			posix_file_info(s48_call_t call,
					s48_ref_t os_str_name,
					s48_ref_t svname,
					s48_ref_t follow_link_p,
					s48_ref_t mode_enum),
			posix_create_symbolic_link(s48_call_t call,
						   s48_ref_t svname1, s48_ref_t svname2),
			posix_read_symbolic_link(s48_call_t call, s48_ref_t svname);
/*
 * Record types imported from Scheme.
 */
static s48_ref_t        posix_file_info_type_binding,
			posix_file_mode_type_binding,
			posix_user_id_type_binding;


/*
 * Forward declarations.
 */

static s48_ref_t enter_mode(s48_call_t call, mode_t mode);

/*
 * Install all exported functions in Scheme48.
 */
void
s48_init_posix_dir(void)
{
  S48_EXPORT_FUNCTION(posix_opendir);
  S48_EXPORT_FUNCTION(posix_readdir);
  S48_EXPORT_FUNCTION(posix_closedir);

  S48_EXPORT_FUNCTION(posix_working_directory);

  S48_EXPORT_FUNCTION(posix_open);
  S48_EXPORT_FUNCTION(posix_file_stuff);

  S48_EXPORT_FUNCTION(posix_file_info);

  S48_EXPORT_FUNCTION(posix_create_symbolic_link);
  S48_EXPORT_FUNCTION(posix_read_symbolic_link);

  posix_user_id_type_binding = 
    s48_get_imported_binding_2("posix-user-id-type");
    
  posix_file_info_type_binding = 
    s48_get_imported_binding_2("posix-file-info-type");
    
  posix_file_mode_type_binding = 
    s48_get_imported_binding_2("posix-file-mode-type");
}

/*
 * Interface to opendir.
 */
static s48_ref_t
posix_opendir(s48_call_t call, s48_ref_t svname)
{
  DIR		*dp;
  s48_ref_t	res;
  char		*c_name;

  c_name = s48_extract_byte_vector_readonly_2(call, svname);
  RETRY_OR_RAISE_NULL(dp, opendir(c_name));
  res = s48_make_value_2(call, DIR *);
  s48_unsafe_extract_value_2(call, res, DIR *) = dp;
  return (res);
}

/*
 * Interface to closedir.
 * Note, it is ok to call closedir on an already closed directory.
 */
static s48_ref_t
posix_closedir(s48_call_t call, s48_ref_t svdir)
{
  DIR	**dpp;

  dpp = s48_extract_value_pointer_2(call, svdir, DIR *);
  if (*dpp != (DIR *)NULL) {
    int		status;
    RETRY_OR_RAISE_NEG(status, closedir(*dpp));
    *dpp = (DIR *)NULL;
  }
  return s48_unspecific_2(call);
}

/*
 * Interface to readdir.
 * If we have already read all the files that are in the directory,
 * #F is returned.  Otherwise, a string with the next file name.
 * Note, "." and ".." are never returned.
 */
static s48_ref_t
posix_readdir(s48_call_t call, s48_ref_t svdir)
{
  DIR		**dpp;
  struct dirent	*dep;
  char		*name;

  dpp = s48_extract_value_pointer_2(call, svdir, DIR *);
  if (*dpp == (DIR *)NULL)
    s48_assertion_violation_2(call, "posix_readdir", "invalid NULL value", 1, svdir);
  do {
    errno = 0;
    RETRY_NULL(dep, readdir(*dpp));
    if (dep == (struct dirent *)NULL) {
      if (errno != 0)
	s48_os_error_2(call, "posix_readdir", errno, 1, svdir);
      return s48_false_2(call);
    }
    name = dep->d_name;
  } while ((name[0] == '.')
	   && (name[1] == '\0' || (name[1] == '.' && name[2] == '\0')));
  return s48_enter_byte_string_2(call, name);
}

/* ************************************************************ */
/*
 * Working directory.
 *
 * If the name is a string, we set the working directory to it.  If the name
 * is #f we return the current working directory.  This would be trivial,
 * except that we don't know how big a buffer we need for the path.  So we
 * keep trying until we run out of memory.
 */

int going = 0;
int second = 0;

static s48_ref_t
posix_working_directory(s48_call_t call, s48_ref_t new_wd)
{
  if (second)
    going = 1;
  else
    second = 1;

  if (s48_false_p_2(call, new_wd)) {
    char	*status;
    char	stack_buffer[256];
    char	*buffer = stack_buffer;
    int		buffer_size = 256;
    
    while (0==0) {
      RETRY_NULL(status, getcwd(buffer, buffer_size));

      if (status == buffer) {
	s48_ref_t	result = s48_enter_byte_string_2(call, buffer);
	if (buffer != stack_buffer)
	  free(buffer);
	return result;
      }
      else if (errno == ERANGE) {
	if (buffer != stack_buffer)
	  free(buffer);
	buffer_size *= 2;
	buffer = (char *) malloc(buffer_size * sizeof(char));
	if (buffer == NULL)
	  s48_out_of_memory_error_2(call);
      }
      else
	s48_os_error("posix_working_directory", errno, 1, new_wd);
    }
  }
  else {
    int		status;

    RETRY_OR_RAISE_NEG(status, chdir(s48_extract_byte_vector_readonly_2(call, new_wd)));
    
    return s48_unspecific_2(call);
  }
}

/* ************************************************************ */
/*
 * Open() and friends.
 *
 */

static s48_ref_t
posix_open(s48_call_t call, s48_ref_t path, s48_ref_t id, s48_ref_t options, s48_ref_t mode, s48_ref_t input_p)
{
  int		fd,
    		c_options;
  char		*c_path;
  s48_ref_t	channel;

  c_options = s48_extract_file_options(call, options);
  c_path = s48_extract_byte_vector_readonly_2(call, path);

  if ((O_WRONLY & c_options) || (O_RDWR & c_options))
    c_options |= O_NONBLOCK;

  if (s48_false_p_2(call, mode))
    RETRY_OR_RAISE_NEG(fd, open(c_path, c_options));
  else {
    mode_t	c_mode = s48_extract_mode(call, mode);
    RETRY_OR_RAISE_NEG(fd, open(c_path, c_options, c_mode));
  }

  channel = s48_add_channel_2(call,
			      s48_extract_boolean_2(call, input_p)
			      ? s48_channel_status_input_2(call)
			      : s48_channel_status_output_2(call),
			      id,
			      fd);

  if (!s48_channel_p_2(call, channel)) {
    ps_close_fd(fd);		/* retries if interrupted */
    s48_raise_scheme_exception_2(call, s48_extract_long_2(call, channel), 0); };

  return channel;
}

/*
 * A bunch of simple procedures merged together to save typing.
 */

static s48_ref_t
posix_file_stuff(s48_call_t call, s48_ref_t op, s48_ref_t arg0, s48_ref_t arg1)
{
  int status;

  switch (s48_extract_long_2(call, op)) {

    /* umask(new_mask) */
  case 0:
    return enter_mode(call, umask(s48_extract_mode(call, arg0)));

    /* link(existing, new) */
  case 1:
    RETRY_OR_RAISE_NEG(status, link(s48_extract_byte_vector_readonly_2(call, arg0),
				    s48_extract_byte_vector_readonly_2(call, arg1)));
    break;

    /* mkdir(path, mode) */
  case 2:
    RETRY_OR_RAISE_NEG(status, mkdir(s48_extract_byte_vector_readonly_2(call, arg0),
				     s48_extract_mode(call, arg1)));
    break;

    /* mkfifo(path, mode) */
  case 3:
    RETRY_OR_RAISE_NEG(status, mkfifo(s48_extract_byte_vector_readonly_2(call, arg0),
				      s48_extract_mode(call, arg1)));
    break;

    /* unlink(char *path) */
  case 4:
    RETRY_OR_RAISE_NEG(status, unlink(s48_extract_byte_vector_readonly_2(call, arg0)));
    break;
    
    /* rmdir(char *path) */
  case 5:
    RETRY_OR_RAISE_NEG(status, rmdir(s48_extract_byte_vector_readonly_2(call, arg0)));
    break;
    
    /* rename(char *old, char *new) */
  case 6:
    RETRY_OR_RAISE_NEG(status, rename(s48_extract_byte_vector_readonly_2(call, arg0),
				      s48_extract_byte_vector_readonly_2(call, arg1)));
    break;
    
    /* access(char *path, int modes) */
  case 7: {
    int modes = s48_extract_long_2(call, arg1);
    int local_modes = (001 & modes ? R_OK : 0) |
                      (002 & modes ? W_OK : 0) |
                      (004 & modes ? X_OK : 0) |
                      (010 & modes ? F_OK : 0);
    char *path = s48_extract_byte_vector_readonly_2(call, arg0);

    RETRY_NEG(status, access(path, local_modes));

    if (status == 0)
      return s48_true_2(call);
    else
      switch (errno){
      case EACCES:	/* access would be denied or search permission denied */
      case EROFS:	/* want write access to a read-only filesystem */
      case ENOENT:	/* no entry for a directory component */
      case ENOTDIR:	/* using a non-directory as a directory */
      case ELOOP:	/* too many symbolic links */
	return s48_false_2(call);
      default:		/* all other errors are (supposed to be) real errors */
	s48_os_error_2(call, "posix_file_stuff/access", errno, 2, arg0, arg1); }
  }
  default:
    /* appease gcc -Wall */
    s48_assertion_violation_2(call, "posix_file_stuff", "invalid operation", 1, op);
  }
  return s48_unspecific_2(call);
}

/* ************************************************************ */
/* File modes.
 *
 * We translate the local bits into our own bits and vice versa.
 */

#define S48_ISUID 004000
#define S48_ISGID 002000
#define S48_ISVTX 001000     /* sticky bit, apparently not POSIX */
#define S48_IRUSR  00400
#define S48_IWUSR  00200
#define S48_IXUSR  00100
#define S48_IRGRP  00040
#define S48_IWGRP  00020
#define S48_IXGRP  00010
#define S48_IROTH  00004
#define S48_IWOTH  00002
#define S48_IXOTH  00001

s48_ref_t
enter_mode(s48_call_t call, mode_t mode)
{
  s48_ref_t	sch_mode;
  mode_t        my_mode;

  my_mode =
    (S_ISUID & mode ? S48_ISUID : 0) |
    (S_ISGID & mode ? S48_ISGID : 0) |
    (S_ISVTX & mode ? S48_ISVTX : 0) |
    (S_IRUSR & mode ? S48_IRUSR : 0) |
    (S_IWUSR & mode ? S48_IWUSR : 0) |
    (S_IXUSR & mode ? S48_IXUSR : 0) |
    (S_IRGRP & mode ? S48_IRGRP : 0) |
    (S_IWGRP & mode ? S48_IWGRP : 0) |
    (S_IXGRP & mode ? S48_IXGRP : 0) |
    (S_IROTH & mode ? S48_IROTH : 0) |
    (S_IWOTH & mode ? S48_IWOTH : 0) |
    (S_IXOTH & mode ? S48_IXOTH : 0);

  sch_mode = s48_make_record_2(call, posix_file_mode_type_binding);
  s48_unsafe_record_set_2(call, sch_mode, 0, s48_enter_long_2(call, my_mode));

  return sch_mode;
}

mode_t
s48_extract_mode(s48_call_t call, s48_ref_t sch_mode)
{
  mode_t        c_mode;
  long		mode;

  s48_check_record_type_2(call, sch_mode, posix_file_mode_type_binding);

  mode = s48_extract_long_2(call, s48_unsafe_record_ref_2(call, sch_mode, 0));

  c_mode =
    (S48_ISUID & mode ? S_ISUID : 0) |
    (S48_ISGID & mode ? S_ISGID : 0) |
    (S48_ISVTX & mode ? S_ISVTX : 0) |
    (S48_IRUSR & mode ? S_IRUSR : 0) |
    (S48_IWUSR & mode ? S_IWUSR : 0) |
    (S48_IXUSR & mode ? S_IXUSR : 0) |
    (S48_IRGRP & mode ? S_IRGRP : 0) |
    (S48_IWGRP & mode ? S_IWGRP : 0) |
    (S48_IXGRP & mode ? S_IXGRP : 0) |
    (S48_IROTH & mode ? S_IROTH : 0) |
    (S48_IWOTH & mode ? S_IWOTH : 0) |
    (S48_IXOTH & mode ? S_IXOTH : 0);

  return c_mode;
}

/* ************************************************************ */
/*
 * Interface to stat(), fstat(), and lstat().
 */

/* from time.c */
extern s48_ref_t s48_posix_enter_time(s48_call_t call, time_t time);


static s48_ref_t
posix_file_info(s48_call_t call,
		s48_ref_t os_str_name,
		s48_ref_t svname,
		s48_ref_t follow_link_p,
		s48_ref_t mode_enum)
{
  struct stat	sbuf;
  int		status;
  s48_ref_t	scm_mode;
  s48_ref_t	info;
  s48_ref_t	temp;

  if (s48_channel_p_2(call, svname)) {
    RETRY_OR_RAISE_NEG(status,
		       fstat(s48_unsafe_extract_long_2(call, 
			       s48_unsafe_channel_os_index_2(call, svname)),
			     &sbuf)); }
  else if (s48_false_p_2(call, follow_link_p))
    RETRY_OR_RAISE_NEG(status, stat(s48_extract_byte_vector_readonly_2(call, svname), &sbuf));
  else
    RETRY_OR_RAISE_NEG(status, lstat(s48_extract_byte_vector_readonly_2(call, svname), &sbuf));

  info = s48_make_record_2(call, posix_file_info_type_binding);

  scm_mode = s48_vector_ref_2(call, 
			      mode_enum,
			      S_ISREG(sbuf.st_mode)  ? 0 :
			      S_ISDIR(sbuf.st_mode)  ? 1 :
			      S_ISCHR(sbuf.st_mode)  ? 2 :
			      S_ISBLK(sbuf.st_mode)  ? 3 :
			      S_ISFIFO(sbuf.st_mode) ? 4 :
			      /* next two are not POSIX */
			      S_ISLNK(sbuf.st_mode)  ? 5 :
			      S_ISSOCK(sbuf.st_mode) ? 6 :
			      7);

  /* Stashing the various field values into temp before handing them
     off to S48_UNSAFE_RECORD_SET is necessary because their
     evaluation may cause GC; that GC could have destroyed the
     temporary holding the value of info before this function was
     moved to the new FFI. */

  s48_unsafe_record_set_2(call, info, 0, os_str_name);
  s48_unsafe_record_set_2(call, info, 1, scm_mode);
  temp = s48_enter_long_2(call, sbuf.st_dev);
  s48_unsafe_record_set_2(call, info, 2, temp);
  temp = s48_enter_long_2(call, sbuf.st_ino);
  s48_unsafe_record_set_2(call, info, 3, temp);
  temp = enter_mode(call, sbuf.st_mode);
  s48_unsafe_record_set_2(call, info, 4, temp);
  temp = s48_enter_long_2(call, sbuf.st_nlink);
  s48_unsafe_record_set_2(call, info, 5, temp);
  temp = s48_enter_uid(call, sbuf.st_uid);
  s48_unsafe_record_set_2(call, info, 6, temp);
  temp = s48_enter_gid(call, sbuf.st_gid);
  s48_unsafe_record_set_2(call, info, 7, temp);
  temp = s48_enter_long_2(call, sbuf.st_size);
  s48_unsafe_record_set_2(call, info, 8, temp);
  temp = s48_posix_enter_time(call, sbuf.st_atime);
  s48_unsafe_record_set_2(call, info, 9, temp);
  temp = s48_posix_enter_time(call, sbuf.st_mtime);
  s48_unsafe_record_set_2(call, info, 10, temp);
  temp = s48_posix_enter_time(call, sbuf.st_ctime);
  s48_unsafe_record_set_2(call, info, 11, temp);

  return info;
}

/* ************************************************************ */
/*
 * Symbolic links.
 */

s48_ref_t
posix_create_symbolic_link(s48_call_t call,
			   s48_ref_t svname1, s48_ref_t svname2)
{
  int status;
  RETRY_OR_RAISE_NEG(status, 
		     symlink(s48_extract_byte_vector_readonly_2(call, svname1),
			     s48_extract_byte_vector_readonly_2(call, svname2)));
  return s48_unspecific_2(call);
}

s48_ref_t
posix_read_symbolic_link(s48_call_t call, s48_ref_t svname)
{
  char local_buf[1024];
  char* buf = local_buf;
  ssize_t buf_size = 1024;
  ssize_t status;
  for (;;)
    {
      RETRY_NEG(status, readlink(s48_extract_byte_vector_readonly_2(call, svname), buf, buf_size - 1));
      if (status >= 0)
	{
	  s48_ref_t result;
	  buf[status] = '\0';
	  result = s48_enter_byte_string_2(call, buf);
	  if (buf != local_buf)
	    free(buf);
	  return result;
	}
      else if (status == ENAMETOOLONG)
	{
	  if (buf != local_buf)
	    free(buf);
	  buf_size *= 2;
	  buf = malloc(buf_size * sizeof(char));
	  if (buf == NULL)
	    s48_out_of_memory_error_2(call);
	}
      else
	s48_os_error_2(call, "posix_read_symbolic_link", errno, 1, svname);
    }
}
