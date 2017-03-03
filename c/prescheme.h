/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Marcus Crestani,
 * David Frese, Timo Harter
 */

#include <errno.h>
#include "io.h"
#include "scheme48arch.h"

#ifdef __GNUC__
  // This requires the "labels as values" extension of GCC
  #define USE_DIRECT_THREADING
#endif

#if SIZEOF_VOID_P == 4
#define BITS_PER_CELL 32
#elif SIZEOF_VOID_P == 8
#define BITS_PER_CELL 64
#else
#error "What size are your pointers, really?"
#endif

#define PS_READ_CHAR(PORT,RESULT,EOFP,STATUS)		\
{							\
  FILE * TTport = PORT;					\
  int TTchar;						\
  if (EOF == (TTchar = getc(TTport)))			\
    RESULT = ps_read_char(TTport, &EOFP, &STATUS, 0==1);\
  else {						\
    RESULT = TTchar; 					\
    EOFP = 0;						\
    STATUS = 0; }					\
}

#define PS_PEEK_CHAR(PORT,RESULT,EOFP,STATUS)		\
{							\
  FILE * TTport = PORT;					\
  int TTchar;						\
  if (EOF == (TTchar = getc(TTport)))			\
    RESULT = ps_read_char(TTport, &EOFP, &STATUS, 0==0);\
  else {						\
    RESULT = TTchar; 					\
    ungetc(RESULT, TTport);				\
    EOFP = 0;						\
    STATUS = 0; }					\
}

#define PS_READ_INTEGER(PORT,RESULT,EOFP,STATUS)	\
RESULT = ps_read_integer(PORT,&EOFP,&STATUS);

#define PS_WRITE_CHAR(CHAR,PORT,STATUS)			\
{							\
  FILE * TTport = PORT;					\
  char TTchar = CHAR;					\
  if (EOF == putc(TTchar,TTport))			\
    STATUS = ps_write_char(TTchar,TTport);		\
  else {						\
    STATUS = 0; }					\
}


/* 
 * C shifts may not work if the amount is greater than the machine word size.
 * Also, undefined for negative values.
 */

#define PS_SHIFT_LEFT_INLINE(X, Y) ((X)*(1L<<(Y)))

static long
PS_SHIFT_RIGHT_INLINE(long x, long y) {
  if (x < 0 && y > 0)
    return x >> y | ~(~0LU >> y);
  else
    return x >> y;
}

#define PS_SHIFT_RIGHT(X,Y,RESULT)   \
{                                    \
  long TTx = X,  TTy = Y;            \
  if ((TTx < 0) && (TTy > 0))        \
    RESULT = (unsigned long)TTx >> TTy | ~(~0LU >> TTy);	\
  else                               \
    RESULT = TTx >> TTy;	     \
}

#define PS_SHIFT_LEFT(X,Y,RESULT)    \
{                                    \
  RESULT = ((X)*(1L<<(Y)));           \
}  

#define PS_SHIFT_RIGHT_LOGICAL(X,Y,RESULT) \
{                                          \
  RESULT = ((unsigned long) X) >> Y;	   \
}

#define PS_SHIFT_RIGHT_LOGICAL_INLINE(X,Y) ((long)(unsigned long)(((unsigned long) (X)) >> (Y)))

extern double ps_pos_infinity(void), ps_neg_infinity(void), ps_not_a_number(void);
#define PS_POS_INF ps_pos_infinity()
#define PS_NEG_INF ps_neg_infinity()
#define PS_NAN ps_not_a_number()

extern long s48_return_value, s48_run_machine();

