/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Mike Sperber, Marcus Crestani
 */

#include <syslog.h>
#include <string.h>
#include <stdlib.h>
#include "scheme48.h"

static s48_ref_t posix_openlog(s48_call_t call,
			       s48_ref_t sch_ident, s48_ref_t sch_option, s48_ref_t sch_facility);
static s48_ref_t posix_setlogmask(s48_call_t call, s48_ref_t sch_logmask);
static s48_ref_t posix_syslog(s48_call_t call,
			      s48_ref_t sch_level, s48_ref_t sch_facility, s48_ref_t sch_message);
static s48_ref_t posix_closelog(s48_call_t call);

/*
 * Install all exported functions in Scheme 48.
 */
void
s48_init_posix_syslog(void)
{
  S48_EXPORT_FUNCTION(posix_openlog);
  S48_EXPORT_FUNCTION(posix_syslog);
  S48_EXPORT_FUNCTION(posix_setlogmask);
  S48_EXPORT_FUNCTION(posix_closelog);
}

/* Syslog options.
 *
 * We translate the our own bits into local bits
 */

/* The order of these is known to the Scheme code. */
static int
extract_syslog_options(s48_call_t call, s48_ref_t sch_syslog_options)
{
  long options = s48_extract_long_2(call, sch_syslog_options);
  return
    (00001 & options ? LOG_CONS   : 0) |
    (00002 & options ? LOG_ODELAY : 0) |
    (00004 & options ? LOG_NDELAY : 0) |
    (00010 & options ? LOG_PID    : 0);
    (00020 & options ? LOG_NOWAIT : 0);
}

/* Syslog facility.
 *
 * We translate the local facility into our own encoding and vice versa.
 */

/* The order of these is known to the Scheme code. */
static int syslog_facilities[] = {
  LOG_AUTH,
  LOG_CRON,
  LOG_DAEMON,
  LOG_KERN,
  LOG_LPR,
  LOG_MAIL,
  LOG_NEWS,
  LOG_USER,
  LOG_UUCP,
  LOG_LOCAL0, LOG_LOCAL1, LOG_LOCAL2, LOG_LOCAL3,
  LOG_LOCAL4, LOG_LOCAL5, LOG_LOCAL6, LOG_LOCAL7
};


static int
extract_syslog_facility(s48_call_t call, s48_ref_t sch_syslog_facility)
{
  return syslog_facilities[s48_extract_long_2(call, sch_syslog_facility)];
}

/* ************************************************************ */
/* Syslog level.
 *
 * We translate the local level into our own encoding and vice versa.
 */

/* The order of these is known to the Scheme code. */
static int syslog_levels[] = {
  LOG_EMERG,
  LOG_ALERT,
  LOG_CRIT,
  LOG_ERR,
  LOG_WARNING,
  LOG_NOTICE,
  LOG_INFO,
  LOG_DEBUG
};


static int
extract_syslog_level(s48_call_t call, s48_ref_t sch_syslog_level)
{
  return syslog_levels[s48_extract_long_2(call, sch_syslog_level)];
}

/* ************************************************************ */
/* Syslog mask.
 *
 * We translate the local bits into our own bits and vice versa.
 */

/* The order of these is known to the Scheme code. */
static s48_ref_t
enter_syslog_mask(s48_call_t call, int syslog_mask)
{
  long my_syslog_mask;

  my_syslog_mask =
    (LOG_MASK(LOG_EMERG)   & syslog_mask ? 00001 : 0) |
    (LOG_MASK(LOG_ALERT)   & syslog_mask ? 00002 : 0) |
    (LOG_MASK(LOG_CRIT)    & syslog_mask ? 00004 : 0) |
    (LOG_MASK(LOG_ERR)     & syslog_mask ? 00010 : 0) |
    (LOG_MASK(LOG_WARNING) & syslog_mask ? 00020 : 0) |
    (LOG_MASK(LOG_NOTICE)  & syslog_mask ? 00040 : 0) |
    (LOG_MASK(LOG_INFO)    & syslog_mask ? 00100 : 0) |
    (LOG_MASK(LOG_DEBUG)   & syslog_mask ? 00200 : 0);

  return s48_enter_long_as_fixnum_2(call, my_syslog_mask);
}

static int
extract_syslog_mask(s48_call_t call, s48_ref_t sch_syslog_mask)
{
  long syslog_mask = s48_extract_long_2(call, sch_syslog_mask);

  return
    (00001 & syslog_mask ? LOG_MASK(LOG_EMERG)   : 0) |
    (00002 & syslog_mask ? LOG_MASK(LOG_ALERT)   : 0) |
    (00004 & syslog_mask ? LOG_MASK(LOG_CRIT)    : 0) |
    (00010 & syslog_mask ? LOG_MASK(LOG_ERR)     : 0) |
    (00010 & syslog_mask ? LOG_MASK(LOG_WARNING) : 0) |
    (00010 & syslog_mask ? LOG_MASK(LOG_NOTICE)  : 0) |
    (00010 & syslog_mask ? LOG_MASK(LOG_INFO)    : 0) |
    (00020 & syslog_mask ? LOG_MASK(LOG_DEBUG)   : 0);
}
#define SYSLOG_IDENT_SIZE 256 /* should be ample */
static int syslog_open = 0;
static char syslog_ident_initial[SYSLOG_IDENT_SIZE];
static char* syslog_ident = syslog_ident_initial;
static size_t syslog_ident_size = SYSLOG_IDENT_SIZE;

static s48_ref_t
posix_openlog(s48_call_t call,
	      s48_ref_t sch_ident, s48_ref_t sch_options, s48_ref_t sch_facility)
{
  if (syslog_open)
    s48_assertion_violation_2(call,
			      "posix_openlog", "syslog is already open", 
			      3, sch_ident, sch_options, sch_facility);
  
  {
    /* 
     * openlog doesn't copy the input string, at least not
     * on every system.  That's just great. 
     */
    char* ident = s48_extract_byte_vector_readonly_2(call, sch_ident);
    size_t ident_size = strlen(ident) + 1;
    if (ident_size > syslog_ident_size)
      {
	if (syslog_ident != syslog_ident_initial)
	  free(syslog_ident);
	syslog_ident = malloc(ident_size);
	if (syslog_ident == NULL)
	  s48_out_of_memory_error_2(call);
	syslog_ident_size = ident_size;
      }
    strcpy(syslog_ident, ident);

  openlog(syslog_ident, 
	  extract_syslog_options(call, sch_options),
	  extract_syslog_facility(call, sch_facility));
  }
  syslog_open = 1;
  return s48_unspecific_2(call);
}

static s48_ref_t
posix_setlogmask(s48_call_t call, s48_ref_t sch_logmask)
{
  return enter_syslog_mask(call, setlogmask(extract_syslog_mask(call, sch_logmask)));
}

static s48_ref_t
posix_syslog(s48_call_t call,
	     s48_ref_t sch_level, s48_ref_t sch_opt_facility, s48_ref_t sch_message)
{
  int facility = s48_false_p_2(call, sch_opt_facility)
    ? 0 : extract_syslog_facility(call, sch_opt_facility);
  int level = extract_syslog_level(call, sch_level);

  if (!syslog_open)
    s48_assertion_violation_2(call, "posix_syslog", "syslog isn't open", 
			      3, sch_level, sch_opt_facility, sch_message);
  syslog(facility | level, "%s", s48_extract_byte_vector_readonly_2(call, sch_message));
  return s48_unspecific_2(call);
}

static s48_ref_t
posix_closelog(s48_call_t call)
{
  if (!syslog_open)
    s48_assertion_violation_2(call, "posix_closelog", "syslog isn't open", 0);
  closelog();
  syslog_open = 0;
  return s48_unspecific_2(call);
}

void
s48_uninit_posix_syslog(void)
{
  if (syslog_ident != syslog_ident_initial)
    free(syslog_ident);
}
