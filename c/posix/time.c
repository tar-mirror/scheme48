/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Marcus Crestani,
 * Robert Ransom
 */

/*
 * An interface to the POSIX time functionality.
 */

#include <time.h>
#include <stdlib.h>
#include "scheme48.h"

static s48_ref_t posix_ctime(s48_call_t call, s48_ref_t sch_time);
static s48_ref_t posix_time(s48_call_t call);

static s48_ref_t posix_asctime(s48_call_t call, s48_ref_t sch_t);
static s48_ref_t posix_localtime(s48_call_t call, s48_ref_t sch_time);
static s48_ref_t posix_gmtime(s48_call_t call, s48_ref_t sch_time);
static s48_ref_t posix_mktime(s48_call_t call, s48_ref_t sch_t);
static s48_ref_t posix_strftime(s48_call_t call, s48_ref_t sch_format, s48_ref_t sch_t);

static s48_ref_t posix_time_type_binding;

/*
 * Install all exported functions in Scheme48.
 */
void
s48_init_posix_time(void)
{
  S48_EXPORT_FUNCTION(posix_ctime);
  S48_EXPORT_FUNCTION(posix_time);

  posix_time_type_binding = 
    s48_get_imported_binding_2("posix-time-type");

  S48_EXPORT_FUNCTION(posix_asctime);
  S48_EXPORT_FUNCTION(posix_localtime);
  S48_EXPORT_FUNCTION(posix_gmtime);
  S48_EXPORT_FUNCTION(posix_mktime);
  S48_EXPORT_FUNCTION(posix_strftime);
}

/* ************************************************************ */
/*
 * Convert a time_t into a Scheme time record.
 */
s48_ref_t
s48_posix_enter_time(s48_call_t call, time_t time)
{
  s48_ref_t	sch_time;
  s48_ref_t	temp;

  sch_time = s48_make_record_2(call, posix_time_type_binding);

  /* Stashing the time value into temp before handing it off to
     S48_UNSAFE_RECORD_SET is necessary because its evaluation may
     cause GC; that GC could destroy the temporary holding the value
     of sch_time. */

  temp = s48_enter_long_2(call, time);
  s48_unsafe_record_set_2(call, sch_time, 0, temp);

  return sch_time;
}

/*
 * Convert a Scheme time record into a time_t.
 */
static time_t
extract_time(s48_call_t call, s48_ref_t time)
{
  s48_check_record_type_2(call, time, posix_time_type_binding);

  return s48_extract_long_2(call, s48_unsafe_record_ref_2(call, time, 0));
}

/*
 * The posix ctime() procedure, which converts a time_t into a string, using
 * the local time zone.
 *
 * ENTER_STRING does a copy, which gets us out of ctime()'s static buffer.
 */
static s48_ref_t
posix_ctime(s48_call_t call, s48_ref_t sch_time)
{
  time_t	time;

  s48_check_record_type_2(call, sch_time, posix_time_type_binding);
  time = extract_time(call, sch_time);
  return s48_enter_byte_string_2(call, ctime(&time));
}

static s48_ref_t
posix_time(s48_call_t call)
{
  time_t the_time, status;

  if (time(&the_time) == -1)
    s48_assertion_violation_2(call, "posix_time", "unknown error calling time(3)", 0);
    
  return s48_posix_enter_time(call, the_time);
}

/*
 * Dates.
 * 
 * POSIX timezone handling is f***ed beyond redemption:
 *
 * tzname, timezone and daylight are global variables that can be set
 * off the TZ environment variable via tzset(3). However, environment
 * variables cannot be set in a thread-safe manner ...  Moreover, the
 * BSDs don't implement timezone and daylight.
 *
 * Olin's scsh code does various heroics to make timezone handling
 * work, but, again, that's not thread-safe.  There's some hope in the
 * tm_zone and tm_gmtoff fields of struct tm that the BSDs and glibc
 * (with _BSD_SOURCE set) have, but we'll punt on this for now.
 */

static s48_ref_t
enter_tm(s48_call_t call, struct tm* t)
{
  s48_ref_t vec = s48_make_vector_2(call, 9, s48_unspecific_2(call));
  s48_vector_set_2(call, vec, 0, s48_enter_long_as_fixnum_2(call, t->tm_sec));
  s48_vector_set_2(call, vec, 1, s48_enter_long_as_fixnum_2(call, t->tm_min));
  s48_vector_set_2(call, vec, 2, s48_enter_long_as_fixnum_2(call, t->tm_hour));
  s48_vector_set_2(call, vec, 3, s48_enter_long_as_fixnum_2(call, t->tm_mday));
  s48_vector_set_2(call, vec, 4, s48_enter_long_as_fixnum_2(call, t->tm_mon));
  s48_vector_set_2(call, vec, 5, s48_enter_long_as_fixnum_2(call, t->tm_year));
  s48_vector_set_2(call, vec, 6, s48_enter_long_as_fixnum_2(call, t->tm_wday));
  s48_vector_set_2(call, vec, 7, s48_enter_long_as_fixnum_2(call, t->tm_yday));
  s48_vector_set_2(call, vec, 8, 
		   (t->tm_isdst == 0)
		   ? s48_false_2(call)
		   : ((t->tm_isdst > 0)
		      ? s48_true_2(call)
		      : s48_unspecific_2(call)));
  return vec;
}

static void
extract_tm(s48_call_t call, s48_ref_t sch_t, struct tm* t)
{
  t->tm_sec = s48_extract_long_2(call, s48_vector_ref_2(call, sch_t, 0));
  t->tm_min = s48_extract_long_2(call, s48_vector_ref_2(call, sch_t, 1));
  t->tm_hour = s48_extract_long_2(call, s48_vector_ref_2(call, sch_t, 2));
  t->tm_mday = s48_extract_long_2(call, s48_vector_ref_2(call, sch_t, 3));
  t->tm_mon = s48_extract_long_2(call, s48_vector_ref_2(call, sch_t, 4));
  t->tm_year = s48_extract_long_2(call, s48_vector_ref_2(call, sch_t, 5));
  t->tm_wday = s48_extract_long_2(call, s48_vector_ref_2(call, sch_t, 6));
  t->tm_yday = s48_extract_long_2(call, s48_vector_ref_2(call, sch_t, 7));
  {
    s48_ref_t sch_isdst = s48_vector_ref_2(call, sch_t, 8);;
    if (s48_true_p_2(call, sch_isdst))
      t->tm_isdst = 1;
    else if (s48_false_p_2(call, sch_isdst))
      t->tm_isdst = 0;
    else
      t->tm_isdst = -1;
  }
}

static s48_ref_t
posix_asctime(s48_call_t call, s48_ref_t sch_t)
{
  struct tm t;
  extract_tm(call, sch_t, &t);
  char* text = asctime(&t);
  return s48_enter_byte_string_2(call, text);
}

static s48_ref_t
posix_localtime(s48_call_t call, s48_ref_t sch_time)
{
  time_t time = extract_time(call, sch_time);
  return enter_tm(call, localtime(&time));
}

static s48_ref_t
posix_gmtime(s48_call_t call, s48_ref_t sch_time)
{
  time_t time = extract_time(call, sch_time);
  return enter_tm(call, gmtime(&time));
}

static s48_ref_t
posix_mktime(s48_call_t call, s48_ref_t sch_t)
{
  struct tm t;
  time_t time;
  extract_tm(call, sch_t, &t);
  time = mktime(&t);
  if (time == -1)
    /* we feel your pain */
    s48_assertion_violation_2(call, "posix_mktime", "invalid time object", 1, sch_t);
  else
    return s48_posix_enter_time(call, time);
}

/* This is really ANSI C, but so is all of the above. */

static s48_ref_t
posix_strftime(s48_call_t call, s48_ref_t sch_format, s48_ref_t sch_t)
{
  struct tm t;
  extract_tm(call, sch_t, &t);

  char local_buf[1024];
  char* buf = local_buf;
  size_t buf_size = 1024;
  size_t status;
  for (;;)
    {
      status = strftime(buf, buf_size, s48_extract_byte_vector_readonly_2(call, sch_format), &t);
      if (status > 0)
	{
	  s48_ref_t result = s48_enter_byte_string_2(call, buf);
	  if (buf != local_buf)
	    free(buf);
	  return result;
	}
      else
	{
	  if (buf != local_buf)
	    free(buf);
	  buf_size *= 2;
	  buf = malloc(buf_size * sizeof(char));
	  if (buf == NULL)
	    s48_out_of_memory_error_2(call);
	}
    }
  
}
