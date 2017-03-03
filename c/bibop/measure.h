/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: David Frese
 */

#ifndef __S48_GC_MEASURE
#define __S48_GC_MEASURE

#if (MEASURE_GC)

/* called in area_roots.c */
extern void measure_write_barrier(char flag);
extern void measure_areas_roots(unsigned long visited, unsigned long passed);
extern void measure_gc_write_barrier();

/* called in generation_gc.c - so these are only forward declarations */
extern void measure_before_collection(int c);
extern void measure_after_collect(int c);

#endif

#endif
