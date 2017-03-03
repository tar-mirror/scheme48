/* Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Michael Zabka, Marcus Crestani
 */

#include <sys/timeb.h>
#include <time.h>
#include "scheme48.h"

static s48_ref_t time_type_binding;

/*
 * Install all exported functions in Scheme48.
 */
void
s48_init_time(void)
{
  S48_EXPORT_FUNCTION(s48_get_current_time);
  S48_EXPORT_FUNCTION(s48_get_timezone);

  time_type_binding =
    s48_get_imported_binding_2("os-time-type");
}

/* ************************************************************ */

/*
 * Convert a _timeb into a Scheme time record.
 */
s48_ref_t
s48_enter_time(s48_call_t call, struct _timeb *now)
{
  s48_ref_t	sch_time;

  sch_time = s48_make_record_2(call, time_type_binding);

  s48_unsafe_record_set_2(call, sch_time, 0, s48_enter_long_2(call, (long)now->time));
  s48_unsafe_record_set_2(call, sch_time, 1, s48_enter_long_2(call, now->millitm*1000));

  return sch_time;
}

/* returns a Scheme time record containing seconds since
 * midnight (00:00:00), January 1, 1970 (UTC) +
 * the fraction of a second in microseconds 
 */
s48_ref_t
s48_get_current_time(s48_call_t call)
{
  struct _timeb time;
  _ftime64_s(&time);

  return s48_enter_time(call, &time);
}

/* returns the difference in seconds between local time and UTC */
s48_ref_t
s48_get_timezone(s48_call_t call)
{
  struct _timeb timebuffer;
  _ftime_s(&timebuffer);

  return s48_enter_long_2(call, -1 * (timebuffer.timezone -
				      timebuffer.dstflag * 60) * 60);
}
