/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Marcus Crestani,
 * Roderic Morris
 */

/*
 * Scheme 48/POSIX regex interface
 */

#include <sys/types.h>
#include <regex.h> /* POSIX.2 */
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "scheme48.h"

extern void		s48_init_posix_regex(void);
static s48_ref_t	posix_compile_regexp(s48_call_t call,
					     s48_ref_t pattern,
					     s48_ref_t extended_p,
					     s48_ref_t ignore_case_p,
					     s48_ref_t submatches_p,
					     s48_ref_t newline_p),
			posix_regexp_match(s48_call_t call,
					   s48_ref_t sch_regex,
					   s48_ref_t string,
					   s48_ref_t start,
					   s48_ref_t submatches_p,
					   s48_ref_t bol_p,
					   s48_ref_t eol_p),
			posix_regexp_error_message(s48_call_t call,
					     s48_ref_t pattern,
					     s48_ref_t extended_p,
					     s48_ref_t ignore_case_p,
					     s48_ref_t submatches_p,
					     s48_ref_t newline_p),
			posix_free_regexp(s48_call_t call, s48_ref_t sch_regex);

/*
 * Record type imported from Scheme.
 */
static s48_ref_t 	posix_regexp_match_type_binding;

/*
 * Install all exported functions in Scheme48.
 */
void
s48_init_posix_regexp(void)
{
  /* Export our stuff. */
  S48_EXPORT_FUNCTION(posix_compile_regexp);
  S48_EXPORT_FUNCTION(posix_regexp_match);
  S48_EXPORT_FUNCTION(posix_regexp_error_message);
  S48_EXPORT_FUNCTION(posix_free_regexp);

  /* Protect and import the regex-match type. */
  posix_regexp_match_type_binding =
    s48_get_imported_binding_2("posix-regexp-match-type");
}

/*
 * Interface to regcomp.  We encode the flags, make the return value, and
 * then call regcomp() to fill it in.
 */
static s48_ref_t
posix_compile_regexp(s48_call_t call, s48_ref_t pattern,
		     s48_ref_t extended_p, s48_ref_t ignore_case_p,
		     s48_ref_t submatches_p, s48_ref_t newline_p)
{
  s48_ref_t sch_regex;
  int status;
  int flags = 
    (s48_extract_boolean_2(call, extended_p)    ? REG_EXTENDED : 0) |
    (s48_extract_boolean_2(call, ignore_case_p) ? REG_ICASE    : 0) |
    (s48_extract_boolean_2(call, submatches_p)  ? 0 : REG_NOSUB) |
    (s48_extract_boolean_2(call, newline_p)     ? REG_NEWLINE  : 0);

  s48_check_byte_vector_2(call, pattern);

  sch_regex = s48_make_value_2(call, regex_t);

  status = regcomp(s48_unsafe_extract_value_pointer_2(call, sch_regex, regex_t),
		   s48_extract_byte_vector_readonly_2(call, pattern),
		   flags);

  if (status == 0)
    return sch_regex;
  else
    return s48_enter_long_2(call, status);   /* not that it can do them much good */
}

/*
 * Interface to regexec.
 *
 * Returns #f if there is no match, #t if there is a match and submatches_p
 * is false, and a list of regex-match records otherwise.
 *
 * Most of this is making the buffer for the match structs and then translating
 * them into Scheme match records.
 */
static s48_ref_t
posix_regexp_match(s48_call_t call, s48_ref_t sch_regex, s48_ref_t string, s48_ref_t sch_start,
		   s48_ref_t submatches_p,
		   s48_ref_t bol_p, s48_ref_t eol_p)
{
  int status;
  s48_ref_t result;

  int start = s48_extract_long_2(call, sch_start);
  int len = strlen(s48_extract_byte_vector_readonly_2(call, string));
  /* re_nsub doesn't include the full pattern */
  size_t nmatch = 1 + s48_extract_value_pointer_2(call, sch_regex, regex_t)->re_nsub;
  regmatch_t *pmatch,
             pmatch_buffer[32];

  int flags =
    (s48_extract_boolean_2(call, bol_p) ? 0 : REG_NOTBOL) |
    (s48_extract_boolean_2(call, eol_p) ? 0 : REG_NOTEOL);

  if ((start < 0) || (start > len))
    s48_assertion_violation_2(call,
			      "posix_regexp_match", "start out of range", 3,
			      sch_start,
			      s48_enter_long_2(call, 0),
			      s48_enter_long_2(call, len));

  if (nmatch <= 32)
    pmatch = pmatch_buffer;
  else {
    pmatch = (regmatch_t *) malloc(nmatch * sizeof(regmatch_t));
    if (pmatch == NULL)
      s48_out_of_memory_error_2(call); }
    
  status = regexec(s48_extract_value_pointer_2(call, sch_regex, regex_t),
		   s48_extract_byte_vector_readonly_2(call, string) + start,
		   nmatch, pmatch, flags);

  if (status == REG_NOMATCH)
    result = s48_false_2(call);
  else if (! s48_extract_boolean_2(call, submatches_p))
    result = s48_true_2(call);
  else {
    s48_ref_t matches = s48_null_2(call);
    s48_ref_t match;
    int i;
    
    for(i = nmatch - 1; i > -1; i--) {
      if (pmatch[i].rm_so == -1)
	match = s48_false_2(call);
      else {
	match = s48_make_record_2(call, posix_regexp_match_type_binding);
	s48_unsafe_record_set_2(call, match,
				0,
				s48_enter_long_2(call, pmatch[i].rm_so + start));
	s48_unsafe_record_set_2(call, match,
				1,
				s48_enter_long_2(call, pmatch[i].rm_eo + start));
	s48_unsafe_record_set_2(call, match, 2, s48_false_2(call)); }  /* submatches */
      matches = s48_cons_2(call, match, matches); }

    result = matches; }

  if (nmatch > 32)
    free(pmatch);
  
  return result;
}

/*
 * Interface to regcomp.
 *
 * This takes the same arguments as `compile_regexp' but returns the error
 * message, if any, that `regcomp()' returns.  For some reason `regerror()'
 * requires both the status code and the compiled pattern buffer returned
 * by `regcomp()'.  `compile_regexp' only returned the status so we have to
 * redo the compilation.
 *
 */
static s48_ref_t
posix_regexp_error_message(s48_call_t call, s48_ref_t pattern,
			   s48_ref_t extended_p, s48_ref_t ignore_case_p,
			   s48_ref_t submatches_p, s48_ref_t newline_p)
{
  regex_t compiled_regex;
  int status;
  int flags = 
    (s48_extract_boolean_2(call, extended_p)    ? REG_EXTENDED : 0) |
    (s48_extract_boolean_2(call, ignore_case_p) ? REG_ICASE    : 0) |
    (s48_extract_boolean_2(call, submatches_p)  ? 0 : REG_NOSUB) |
    (s48_extract_boolean_2(call, newline_p)     ? REG_NEWLINE  : 0);

  s48_check_byte_vector_2(call, pattern);

  status = regcomp(&compiled_regex, s48_extract_byte_vector_readonly_2(call, pattern), flags);

  if (status == 0)
    return s48_false_2(call);
  else {
    size_t buffer_size;
    s48_ref_t buffer;
    
    buffer_size = regerror(status, &compiled_regex, NULL, 0);
    /* For string lengths C counts the nul, Scheme doesn't. */
    buffer = s48_make_byte_vector_2(call, buffer_size);
    regerror(status,
	     &compiled_regex,
	     s48_extract_byte_vector_2(call, buffer),
	     buffer_size);
    
    return buffer; }
}

/*
 * Stub for regfree().
 */

static s48_ref_t
posix_free_regexp(s48_call_t call, s48_ref_t sch_regex)
{
  regfree(s48_extract_value_pointer_2(call, sch_regex, regex_t));

  return s48_unspecific_2(call);
}
