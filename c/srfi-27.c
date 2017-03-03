/* 54-BIT (double) IMPLEMENTATION IN C OF THE "MRG32K3A" GENERATOR
   ===============================================================
 
   Sebastian.Egner@philips.com, Mar-2002, in ANSI-C and Scheme 48 0.57

   This code is a C-implementation of Pierre L'Ecuyer's MRG32k3a generator.
   The code uses (double)-arithmetics, assuming that it covers the range
   {-2^53..2^53-1} exactly (!). The code of the generator is based on the 
   L'Ecuyer's own implementation of the generator. Please refer to the
   file 'mrg32k3a.scm' for more information about the method.

   The method provides the following functions via the C/Scheme
   interface of Scheme 48 0.57 to 'mrg32k3a-b.scm':

      s48_ref_t mrg32k3a_pack_state1(s48_ref_t state);
      s48_ref_t mrg32k3a_unpack_state1(s48_ref_t state);
      s48_ref_t mrg32k3a_random_range();
      s48_ref_t mrg32k3a_random_integer(s48_ref_t state, s48_ref_t range);
      s48_ref_t mrg32k3a_random_real(s48_ref_t state);

   As Scheme48 FIXNUMs cannot cover the range {0..m1-1}, we break up
   all values x in the state into x0+x1*w, where w = 2^16 = 65536.
   The procedures in Scheme correct for that.

   compile this file with:
     gcc -c -I $SCHEME48 mrg32k3a-b.c

   history of this file: 
     SE, 18-Mar-2002: initial version
     SE, 22-Mar-2002: interface changed
     SE, 25-Mar-2002: tested with Scheme 48 0.57 in c/srfi-27
     SE, 27-Mar-2002: cleaned
     SE, 13-May-2002: bug found by Shiro Kawai removed
*/

#include "scheme48.h" /* $SCHEME48/c/scheme48.h */

#ifdef _WIN32
#include <windows.h>
#else
#include <sys/time.h>
#endif

#ifndef NULL
  #define NULL 0
#endif
/* maximum value for random_integer: min(S48_MAX_FIXNUM_VALUE, m1) */
#define m_max (((long)1 << 29) - 1)

/* The Generator
   =============
*/

/* moduli of the components */
#define m1 4294967087.0
#define m2 4294944443.0

/* representation of the state in C */
typedef struct {
  double 
    x10, x11, x12,
    x20, x21, x22; 
} state_t;

/* recursion coefficients of the components */
#define a12  1403580.0
#define a13n  810728.0
#define a21   527612.0
#define a23n 1370589.0

/* normalization factor 1/(m1 + 1) */
#define norm 2.328306549295728e-10


/* the actual generator */

static double mrg32k3a(state_t *s) { /* (double), in {0..m1-1} */
  double x10, x20, y;
  long   k10, k20;

/* #define debug 1 */

#if defined(debug)
  printf(
    "state = {%g %g %g %g %g %g};\n",
    s->x10, s->x11, s->x12, 
    s->x20, s->x21, s->x22
  );
#endif

  /* component 1 */
  x10  = a12*(s->x11) - a13n*(s->x12);
  k10  = x10 / m1;
  x10 -= k10 * m1;
  if (x10 < 0.0)
    x10 += m1;
  s->x12 = s->x11;
  s->x11 = s->x10;
  s->x10 = x10;

  /* component 2 */
  x20  = a21*(s->x20) - a23n*(s->x22);
  k20  = x20 / m2;
  x20 -= k20 * m2;
  if (x20 < 0.0)
    x20 += m2;
  s->x22 = s->x21;
  s->x21 = s->x20;
  s->x20 = x20;

  /* combination of component */
  y = x10 - x20;
  if (y < 0.0)
    y += m1;
  return y;
}

/* Exported Interface
   ==================
*/

s48_ref_t mrg32k3a_pack_state1(s48_call_t call, s48_ref_t state) {
  s48_ref_t result;
  state_t   s;

#define REF(i) (double)s48_extract_long_2(call, s48_vector_ref_2(call, state, (long)(i)))

  /* copy the numbers from state into s */
  s.x10 = REF( 0) + 65536.0 * REF( 1);
  s.x11 = REF( 2) + 65536.0 * REF( 3);
  s.x12 = REF( 4) + 65536.0 * REF( 5);
  s.x20 = REF( 6) + 65536.0 * REF( 7);
  s.x21 = REF( 8) + 65536.0 * REF( 9);
  s.x22 = REF(10) + 65536.0 * REF(11);

#undef REF

  /* box s into a Scheme object */
  result = s48_make_value_2(call, state_t);
  s48_set_value_2(call, result, state_t, s);
  return result;
}

s48_ref_t mrg32k3a_unpack_state1(s48_call_t call, s48_ref_t state) {
  s48_ref_t result;
  state_t   s;

  /* unbox s from the Scheme object */
  s = s48_extract_value_2(call, state, state_t);

  /* make and fill a Scheme vector with the numbers */
  result = s48_make_vector_2(call, (long)12, s48_false_2(call));

#define SET(i, x) { \
  long x1 = (long)((x) / 65536.0); \
  long x0 = (long)((x) - 65536.0 * (double)x1); \
  s48_vector_set_2(call, result, (long)(i+0), s48_enter_long_2(call, x0)); \
  s48_vector_set_2(call, result, (long)(i+1), s48_enter_long_2(call, x1)); }

  SET( 0, s.x10);
  SET( 2, s.x11);
  SET( 4, s.x12);
  SET( 6, s.x20);
  SET( 8, s.x21);
  SET(10, s.x22);

#undef SET

  return result;
}

s48_ref_t mrg32k3a_random_range(s48_call_t call) {
  return s48_enter_long_2(call, m_max);  
}

s48_ref_t mrg32k3a_random_integer(s48_call_t call, s48_ref_t state, s48_ref_t range) {
  long    result;
  state_t s;
  long    n;
  double  x, q, qn, xq;

  s = s48_extract_value_2(call, state, state_t);
  n = s48_extract_long_2(call, range);
  if (!( ((long)1 <= n) && (n <= m_max) ))
    s48_assertion_violation_2(call, "mrg32k3a_random_integer", "invalid range", 1, state);

  /* generate result in {0..n-1} using the rejection method */
  q  = (double)( (unsigned long)(m1 / (double)n) );
  qn = q * n;
  do {
    x = mrg32k3a(&s);
  } while (x >= qn);
  xq = x / q;

  /* check the range */
  if (!( (0.0 <= xq) && (xq < (double)m_max) ))
    s48_assertion_violation_2(call, "mrg32k3a_random_integer", "invalid xq", 1, s48_enter_long_2(call, (long)xq));

  /* return result */
  result = (long)xq;
  s48_set_value_2(call, state, state_t, s);
  return s48_enter_long_2(call, result);
}

s48_ref_t mrg32k3a_random_real(s48_call_t call, s48_ref_t state) {
  state_t s;
  double  x;

  s = s48_extract_value_2(call, state, state_t);
  x = (mrg32k3a(&s) + 1.0) * norm;
  s48_set_value_2(call, state, state_t, s);
  return s48_enter_double_2(call, x);
}

#ifdef _WIN32
static s48_ref_t current_time(s48_call_t call){
  SYSTEMTIME systemTime;
  GetSystemTime(&systemTime);
  return s48_enter_unsigned_long_2(call, (unsigned long) systemTime.wSecond);
}
#else
static s48_ref_t current_time(s48_call_t call){
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return s48_enter_long_2(call, tv.tv_sec);
}
#endif


/* Exporting the C values to Scheme
   ================================
*/

void
s48_on_load(void) {
  S48_EXPORT_FUNCTION(mrg32k3a_pack_state1);
  S48_EXPORT_FUNCTION(mrg32k3a_unpack_state1);
  S48_EXPORT_FUNCTION(mrg32k3a_random_range);
  S48_EXPORT_FUNCTION(mrg32k3a_random_integer);
  S48_EXPORT_FUNCTION(mrg32k3a_random_real);
  S48_EXPORT_FUNCTION(current_time);
}

