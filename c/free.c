/* Free-format floating point printer */

/*
 * All software (c) 1996 Robert G. Burger.

 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software, to deal in the software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the software.

 * The software is provided "as is," without warranty of any kind,
 * express or implied, including but not limited to the warranties of
 * merchantability, fitness for a particular purpose and
 * noninfringement. In no event shall the author be liable for any
 * claim, damages or other liability, whether in an action of
 * contract, tort or otherwise, arising from, out of or in connection
 * with the software or the use or other dealings in the software.
 */

/*
 * Modifications for Scheme 48 by Mike Sperber, Eric Knauel,
 * David Frese, Taylor Campbell
 */

#include <stdio.h>
#include <string.h>

#ifndef _WIN32
#include "sysdep.h"
#endif

/* It's a shame ... */
#ifdef _WIN32
typedef unsigned long word32_t;
#define UNSIGNED64 unsigned _int64
#else
#define UNSIGNED64 unsigned long long
#include <inttypes.h>
typedef uint32_t word32_t;
#endif

#define U32 word32_t

/* exponent stored + 1024, hidden bit to left of decimal point */
#define bias 1023
#define bitstoright 52
#define m1mask 0xf
#define hidden_bit 0x100000

#define sign_mask 0x80000000
#define exp_mask  0x7ff00000
#define exp_max 0x7ff
#define exp_shift1 20
#define frac_mask 0xfffff

typedef union { double d; word32_t word[2]; } double_overlay;

#if ((defined(BUILD_UNIVERSAL_BINARY) && defined(__BIG_ENDIAN__)) || IEEE_MOST_FIRST)
#define DOUBLE_WORD0(x) ((double_overlay*)&(x))->word[0]
#define DOUBLE_WORD1(x) ((double_overlay*)&(x))->word[1]
#else
#define DOUBLE_WORD0(x) ((double_overlay*)&(x))->word[1]
#define DOUBLE_WORD1(x) ((double_overlay*)&(x))->word[0]
#endif

#define float_radix 2.147483648e9

typedef UNSIGNED64 Bigit;
#define BIGSIZE 24
#define MIN_E -1074
#define MAX_FIVE 325
#define B_P1 ((Bigit)1 << 52)

typedef struct {
   int l;
   Bigit d[BIGSIZE];
} Bignum;

Bignum R, S, MP, MM, five[MAX_FIVE];
Bignum S2, S3, S4, S5, S6, S7, S8, S9;
int ruf, k, s_n, use_mp, qr_shift, sl, slr;

static void mul10(Bignum *x);
static void big_short_mul(Bignum *x, Bigit y, Bignum *z);
static void print_big (Bignum *x);
static int estimate(int n);
static void one_shift_left(int y, Bignum *z);
static void short_shift_left(Bigit x, int y, Bignum *z);
static void big_shift_left(Bignum *x, int y, Bignum *z);
static int big_comp(Bignum *x, Bignum *y);
static int sub_big(Bignum *x, Bignum *y, Bignum *z);
static void add_big(Bignum *x, Bignum *y, Bignum *z);
static int add_cmp(void);
static int qr(void);

int s48_dragon(char *buf, double v);
void s48_free_init(void);

#define ADD(x, y, z, k) {\
   Bigit x_add, z_add;\
   x_add = (x);\
   if ((k))\
     z_add = x_add + (y) + 1, (k) = (z_add <= x_add);\
   else\
     z_add = x_add + (y), (k) = (z_add < x_add);\
   (z) = z_add;\
}

#define SUB(x, y, z, b) {\
   Bigit x_sub, y_sub;\
   x_sub = (x); y_sub = (y);\
   if ((b))\
     (z) = x_sub - y_sub - 1, b = (y_sub >= x_sub);\
   else\
     (z) = x_sub - y_sub, b = (y_sub > x_sub);\
}

#define MUL(x, y, z, k) {\
   Bigit x_mul, low, high;\
   x_mul = (x);\
   low = (x_mul & 0xffffffff) * (y) + (k);\
   high = (x_mul >> 32) * (y) + (low >> 32);\
   (k) = high >> 32;\
   (z) = (low & 0xffffffff) | (high << 32);\
}

#define SLL(x, y, z, k) {\
   Bigit x_sll = (x);\
   (z) = (x_sll << (y)) | (k);\
   (k) = x_sll >> (64 - (y));\
}

void mul10(x) Bignum *x; {
   int i, l;
   Bigit *p, k;

   l = x->l;
   for (i = l, p = &x->d[0], k = 0; i >= 0; i--)
      MUL(*p, 10, *p++, k);
   if (k != 0)
      *p = k, x->l = l+1;
}

void big_short_mul(x, y, z) Bignum *x, *z; Bigit y; {
   int i, xl, zl;
   Bigit *xp, *zp, k;
   U32 high, low;

   xl = x->l;
   xp = &x->d[0];
   zl = xl;
   zp = &z->d[0];
   high = y >> 32;
   low = y & 0xffffffff;
   for (i = xl, k = 0; i >= 0; i--, xp++, zp++) {
      Bigit xlow, xhigh, z0, t, c, z1;
      xlow = *xp & 0xffffffff;
      xhigh = *xp >> 32;
      z0 = (xlow * low) + k; /* Cout is (z0 < k) */
      t = xhigh * low;
      z1 = (xlow * high) + t;
      c = (z1 < t);
      t = z0 >> 32;
      z1 += t;
      c += (z1 < t);
      *zp = (z1 << 32) | (z0 & 0xffffffff);
      k = (xhigh * high) + (c << 32) + (z1 >> 32) + (z0 < k);
   }
   if (k != 0)
      *zp = k, zl++;
   z->l = zl;
}

void print_big(x) Bignum *x; {
   int i;
   Bigit *p;

   printf("#x");
   i = x->l;
   p = &x->d[i];
   for (p = &x->d[i]; i >= 0; i--) {
      Bigit b = *p--;
      printf("%08x%08x", (int)(b >> 32), (int)(b & 0xffffffff));
   }
}

int estimate(n) int n; {
   if (n < 0)
      return (int)(n*0.3010299956639812);
   else
      return 1+(int)(n*0.3010299956639811);
}

void one_shift_left(y, z) int y; Bignum *z; {
   int n, m, i;
   Bigit *zp;

   n = y / 64;
   m = y % 64;
   zp = &z->d[0];
   for (i = n; i > 0; i--) *zp++ = 0;
   *zp = (Bigit)1 << m;
   z->l = n;
}

void short_shift_left(x, y, z) Bigit x; int y; Bignum *z; {
   int n, m, i, zl;
   Bigit *zp;
   
   n = y / 64;
   m = y % 64;
   zl = n;
   zp = &(z->d[0]);
   for (i = n; i > 0; i--) *zp++ = 0;
   if (m == 0)
      *zp = x;
   else {
      Bigit high = x >> (64 - m);
      *zp = x << m;
      if (high != 0)
	 *++zp = high, zl++;
   }
   z->l = zl;
}

void big_shift_left(x, y, z) Bignum *x, *z; int y; {
   int n, m, i, xl, zl;
   Bigit *xp, *zp, k;
   
   n = y / 64;
   m = y % 64;
   xl = x->l;
   xp = &(x->d[0]);
   zl = xl + n;
   zp = &(z->d[0]);
   for (i = n; i > 0; i--) *zp++ = 0;
   if (m == 0)
      for (i = xl; i >= 0; i--) *zp++ = *xp++;
   else {
      for (i = xl, k = 0; i >= 0; i--)
	 SLL(*xp++, m, *zp++, k);
      if (k != 0)
	 *zp = k, zl++;
   }
   z->l = zl;
}


int big_comp(x, y) Bignum *x, *y; {
   int i, xl, yl;
   Bigit *xp, *yp;

   xl = x->l;
   yl = y->l;
   if (xl > yl) return 1;
   if (xl < yl) return -1;
   xp = &x->d[xl];
   yp = &y->d[xl];
   for (i = xl; i >= 0; i--, xp--, yp--) {
      Bigit a = *xp;
      Bigit b = *yp;

      if (a > b) return 1;
      else if (a < b) return -1;
   }
   return 0;
}

int sub_big(x, y, z) Bignum *x, *y, *z; {
  int xl, yl, zl, b, i;
  Bigit *xp, *yp, *zp;

  xl = x->l;
  yl = y->l;
  if (yl > xl) return 1;
  xp = &x->d[0];
  yp = &y->d[0];
  zp = &z->d[0];
  
  for (i = yl, b = 0; i >= 0; i--)
    SUB(*xp++, *yp++, *zp++, b);
  for (i = xl-yl; b && i > 0; i--) {
    Bigit x_sub;
    x_sub = *xp++;
    *zp++ = x_sub - 1;
    b = (x_sub == 0);
  }
  for (; i > 0; i--) *zp++ = *xp++;
  if (b) return 1;
  zl = xl;
  while ((zl > 0) && (*--zp == 0)) zl--;
  z->l = zl;
  return 0;
}

void add_big(x, y, z) Bignum *x, *y, *z; {
  int xl, yl, k, i;
  Bigit *xp, *yp, *zp;

  xl = x->l;
  yl = y->l;
  if (yl > xl) {
    int tl;
    Bignum *tn;
    tl = xl; xl = yl; yl = tl;
    tn = x; x = y; y = tn;
  }

  xp = &x->d[0];
  yp = &y->d[0];
  zp = &z->d[0];
  
  for (i = yl, k = 0; i >= 0; i--)
    ADD(*xp++, *yp++, *zp++, k);
  for (i = xl-yl; k && i > 0; i--) {
    Bigit z_add;
    z_add = *xp++ + 1;
    k = (z_add == 0);
    *zp++ = z_add;
  }
  for (; i > 0; i--) *zp++ = *xp++;
  if (k)
    *zp = 1, z->l = xl+1;
  else
    z->l = xl;
}

int add_cmp() {
   int rl, ml, sl, suml;
   static Bignum sum;

   rl = R.l;
   ml = (use_mp ? MP.l : MM.l);
   sl = S.l;
   
   suml = rl >= ml ? rl : ml;
   if ((sl > suml+1) || ((sl == suml+1) && (S.d[sl] > 1))) return -1;
   if (sl < suml) return 1;

   add_big(&R, (use_mp ? &MP : &MM), &sum);
   return big_comp(&sum, &S);
}

int qr() {
  if (big_comp(&R, &S5) < 0)
    if (big_comp(&R, &S2) < 0)
      if (big_comp(&R, &S) < 0)
	return 0;
      else {
	sub_big(&R, &S, &R);
	return 1;
      }
    else if (big_comp(&R, &S3) < 0) {
      sub_big(&R, &S2, &R);
      return 2;
    }
    else if (big_comp(&R, &S4) < 0) {
      sub_big(&R, &S3, &R);
      return 3;
    }
    else {
      sub_big(&R, &S4, &R);
      return 4;
    }
  else if (big_comp(&R, &S7) < 0)
    if (big_comp(&R, &S6) < 0) {
      sub_big(&R, &S5, &R);
      return 5;
    }
    else {
      sub_big(&R, &S6, &R);
      return 6;
    }
  else if (big_comp(&R, &S9) < 0)
    if (big_comp(&R, &S8) < 0) {
      sub_big(&R, &S7, &R);
      return 7;
    }
    else {
      sub_big(&R, &S8, &R);
      return 8;
    }
  else {
    sub_big(&R, &S9, &R);
    return 9;
  }
}

#define OUTDIG(d) { *buf++ = (d) + '0'; *buf = 0; return k; }

int s48_dragon(buf, v) char *buf; double v; {
   int sign, e, f_n, m_n, i, d, tc1, tc2;
   word32_t word0 = DOUBLE_WORD0(v);
   word32_t word1 = DOUBLE_WORD1(v);
   Bigit f;

   /* decompose float into sign, mantissa & exponent */
   sign = ((word0 & sign_mask) != 0);
   e = (int)(word0 >> exp_shift1 & (exp_mask>>exp_shift1));

   f = (((Bigit)(word0 & frac_mask)) << 32) | word1;

   if (e == exp_max) {
     /* infinity or NaN */
     if (f == 0)
       strcpy(buf, sign ? "-inf.0" : "+inf.0");
     else
       strcpy(buf, "+nan.0");
     return 0;
   }

   if (e != 0) {
      e = e - bias - bitstoright;
      f |= (Bigit)hidden_bit << 32;
   }
   else if (f != 0)
      /* denormalized */
      e = 1 - bias - bitstoright;

   if (sign) *buf++ = '-';
   if (f == 0) {
     *buf++ = '0';
     *buf = 0;
     return 0;
   }
   
   ruf = !(f & 1); /* ruf = (even? f) */

   /* Compute the scaling factor estimate, k */
   if (e > MIN_E)
      k = estimate(e+52);
   else {
      int n;
      Bigit y;
      
      for (n = e+52, y = (Bigit)1 << 52; f < y; n--) y >>= 1;
      k = estimate(n);
   }

   if (e >= 0)
      if (f != B_P1)
	 use_mp = 0, f_n = e+1, s_n = 1, m_n = e;
      else
	 use_mp = 1, f_n = e+2, s_n = 2, m_n = e;
   else
      if ((e == MIN_E) || (f != B_P1))
	 use_mp = 0, f_n = 1, s_n = 1-e, m_n = 0;
      else
	 use_mp = 1, f_n = 2, s_n = 2-e, m_n = 0;
   
   /* Scale it! */
   if (k == 0) {
      short_shift_left(f, f_n, &R);
      one_shift_left(s_n, &S);
      one_shift_left(m_n, &MM);
      if (use_mp) one_shift_left(m_n+1, &MP);
      qr_shift = 1;
   }
   else if (k > 0) {
      s_n += k;
      if (m_n >= s_n)
	 f_n -= s_n, m_n -= s_n, s_n = 0;
      else 
	 f_n -= m_n, s_n -= m_n, m_n = 0;
      short_shift_left(f, f_n, &R);
      big_shift_left(&five[k-1], s_n, &S);
      one_shift_left(m_n, &MM);
      if (use_mp) one_shift_left(m_n+1, &MP);
      qr_shift = 0;
   }
   else {
      Bignum *power = &five[-k-1];

      s_n += k;
      big_short_mul(power, f, &S);
      big_shift_left(&S, f_n, &R);
      one_shift_left(s_n, &S);
      big_shift_left(power, m_n, &MM);
      if (use_mp) big_shift_left(power, m_n+1, &MP);
      qr_shift = 1;
   }

   /* fixup */
   if (add_cmp() <= -ruf) {
     k--;
     mul10(&R);
     mul10(&MM);
     if (use_mp) mul10(&MP);
   }

   /*
   printf("\nk = %d\n", k);
   printf("R = "); print_big(&R);
   printf("\nS = "); print_big(&S);
   printf("\nM- = "); print_big(&MM);
   if (use_mp) printf("\nM+ = "), print_big(&MP);
   putchar('\n');
   fflush(0);
   */
   
   if (qr_shift) {
     sl = s_n / 64;
     slr = s_n % 64;
   }
   else {
     big_shift_left(&S, 1, &S2);
     add_big(&S2, &S, &S3);
     big_shift_left(&S2, 1, &S4);
     add_big(&S4, &S, &S5);
     add_big(&S4, &S2, &S6);
     add_big(&S4, &S3, &S7);
     big_shift_left(&S4, 1, &S8);
     add_big(&S8, &S, &S9);
   }

again:
   if (qr_shift) { /* Take advantage of the fact that S = (ash 1 s_n) */
      if (R.l < sl)
	 d = 0;
      else if (R.l == sl) {
	    Bigit *p;

	    p = &R.d[sl];
	    d = *p >> slr;
	    *p &= ((Bigit)1 << slr) - 1;
	    for (i = sl; (i > 0) && (*p == 0); i--) p--;
	    R.l = i;
	 }
      else {
	 Bigit *p;
	 
	 p = &R.d[sl+1];
	 d = *p << (64 - slr) | *(p-1) >> slr;
	 p--;
	 *p &= ((Bigit)1 << slr) - 1;
	 for (i = sl; (i > 0) && (*p == 0); i--) p--;
	 R.l = i;
      }
   }
   else /* We need to do quotient-remainder */
     d = qr();

   tc1 = big_comp(&R, &MM) < ruf;
   tc2 = add_cmp() > -ruf;
   if (!tc1)
      if (!tc2) {
	 mul10(&R);
	 mul10(&MM);
	 if (use_mp) mul10(&MP);
	 *buf++ = d + '0';
	 goto again;
      }
      else
	OUTDIG(d+1)
   else
      if (!tc2)
	 OUTDIG(d)
      else {
	 big_shift_left(&R, 1, &MM);
	 if (big_comp(&MM, &S) == -1)
	    OUTDIG(d)
	 else
	    OUTDIG(d+1)
      }
}

void s48_free_init() {
   int n, i, l;
   Bignum *b;
   Bigit *xp, *zp, k;

   five[0].l = l = 0;
   five[0].d[0] = 5;
   for (n = MAX_FIVE-1, b = &five[0]; n > 0; n--) {
      xp = &b->d[0];
      b++;
      zp = &b->d[0];
      for (i = l, k = 0; i >= 0; i--)
	 MUL(*xp++, 5, *zp++, k);
      if (k != 0)
	 *zp = k, l++;
      b->l = l;
   }

   /*
   for (n = 1, b = &five[0]; n <= MAX_FIVE; n++) {
      big_shift_left(b++, n, &R);
      print_big(&R);
      putchar('\n');
   }
   fflush(0);
   */
}
