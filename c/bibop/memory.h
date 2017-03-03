/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: David Frese
 */

#ifndef __S48_GC_MEMORY_H
#define __S48_GC_MEMORY_H

typedef char* s48_address;

/* bytes <--> cells */
/* can't include scheme48.h, because of mutual inclusion: defines S48_LOG_BYTES_PER_CELL */
#define S48_BYTES_PER_CELL (1L << S48_LOG_BYTES_PER_CELL)

#define S48_BYTES_TO_CELLS(b) (((unsigned long)(b + (S48_BYTES_PER_CELL - 1))) \
  >> S48_LOG_BYTES_PER_CELL)

#define S48_CELLS_TO_BYTES(c) ((c) << S48_LOG_BYTES_PER_CELL)

/* addressable units <--> cells */
#define S48_LOG_A_UNITS_PER_CELL S48_LOG_BYTES_PER_CELL /* on byte-addressable platforms (the only ones we support at the moment) */
#define S48_A_UNITS_PER_CELL (1L << S48_LOG_A_UNITS_PER_CELL)

#define S48_A_UNITS_TO_CELLS(a) (((unsigned long)a) >> S48_LOG_A_UNITS_PER_CELL)
#define S48_CELLS_TO_A_UNITS(c) ((c) << S48_LOG_A_UNITS_PER_CELL)

/* addressable units <--> bytes */
#define S48_BYTES_TO_A_UNITS(b) S48_CELLS_TO_A_UNITS(S48_BYTES_TO_CELLS(b))
#define S48_A_UNITS_TO_BYTES(a) S48_CELLS_TO_BYTES(S48_A_UNITS_TO_CELLS(a))

/* address1+ */
#define S48_ADDRESS_INC(address) ((s48_address)(address + S48_A_UNITS_PER_CELL))

#endif
