/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Richard Kelsey, Jonathan Rees, Mike Sperber
 */

/*
 * Macros for retrying interrupted system calls.  RETURN_NULL 
 */

#define RETRY_NULL(STATUS, CALL)				\
do {								\
  STATUS = (CALL);						\
} while ((STATUS == NULL) && (errno == EINTR))

#define RETRY_NEG(STATUS, CALL)					\
do {								\
  STATUS = (CALL);						\
} while ((STATUS < 0) && (errno == EINTR))

#define RETRY_OR_RAISE_NULL(STATUS, CALL)			\
do {								\
  while (1) {							\
    STATUS = (CALL);						\
    if (STATUS != NULL)						\
      break;							\
    else if (errno != EINTR)					\
      s48_os_error_2(call, NULL, errno, 0); }			\
 } while (0)

#define RETRY_OR_RAISE_NEG(STATUS, CALL)			\
do {								\
  while (1) {							\
    STATUS = (CALL);						\
    if (STATUS >= 0)						\
      break;							\
    else if (errno != EINTR)					\
      s48_os_error_2(call, NULL, errno, 0); }			\
 } while (0)


