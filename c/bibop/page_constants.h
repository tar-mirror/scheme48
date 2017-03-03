/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: David Frese, Robert Ransom
 */

#ifndef __S48_PAGE_CONSTANTS_H
#define __S48_PAGE_CONSTANTS_H

#include "memory.h"

#define LOG_BYTES_PER_PAGE 12
#define BYTES_PER_PAGE (1L << LOG_BYTES_PER_PAGE)
#define PAGE_INDEX_MASK (BYTES_PER_PAGE - 1)
#define PAGE_START_ADDRESS(address) ((s48_address)(((long)address) & \
  (~ PAGE_INDEX_MASK)))
#define INDEX_IN_PAGE(address) ((long)((address) & PAGE_INDEX_MASK))

/* This macro evaluates its argument twice to avoid a possible integer
   overflow. */
#define BYTES_TO_PAGES(n) (((n) >> LOG_BYTES_PER_PAGE) + \
			   (((n) & PAGE_INDEX_MASK) ? 1 : 0))

/* This macro can produce an integer overflow, even if applied to a
   number which was returned from BYTES_TO_PAGES. Use with extreme
   care. */
#define PAGES_TO_BYTES_I_KNOW_THIS_CAN_OVERFLOW(n) \
  ((unsigned long)(n) << LOG_BYTES_PER_PAGE)

/* Needed for gc_config.h, where it is used to precompute a constant
   that the preprocessor must be able to compare to another constant.
   (CPP can't handle "unsigned long" in an arithmetic expression.) */
#define PAGES_TO_BYTES_SMALL_CONST_FOR_CPP(n) ((n) << LOG_BYTES_PER_PAGE)

#define PAGES_TO_BYTES_LOSSAGE_MASK ((long)(~((unsigned long)(-1L) >> LOG_BYTES_PER_PAGE)))
#define PAGES_TO_BYTES_LOSES_P(n) ((n) & PAGES_TO_BYTES_LOSSAGE_MASK)

/* This macro can produce an integer overflow, and was unused. */
/* #define PAGES_TO_CELLS(n) (S48_BYTES_TO_CELLS((n) << LOG_BYTES_PER_PAGE)) */

#define ADD_PAGES_I_KNOW_THIS_CAN_OVERFLOW(address, pages) ((s48_address)((address) + \
					       PAGES_TO_BYTES_I_KNOW_THIS_CAN_OVERFLOW(pages)))

#endif
