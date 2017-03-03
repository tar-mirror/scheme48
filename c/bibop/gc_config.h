/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: David Frese, Christos Freris
 */

#ifndef __S48_GC_CONFIG
#define __S48_GC_CONFIG

#include "page_constants.h"
#include "utils.h"

/* Configuration options */

/* Debugging */

#ifndef DISPLAY_MEASURE_GC
#define DISPLAY_MEASURE_GC FALSE
#endif

#ifndef BIBOP_LOG
#define BIBOP_LOG FALSE
#endif


/* Measurement */

#ifndef S48_MEASURE_GC_TIME
#define S48_MEASURE_GC_TIME FALSE
#endif

#ifndef MEASURE_GC
#define MEASURE_GC FALSE
#endif



/* BIBOP */

/* 1. Areas */

/* For small objects */
#ifndef S48_MINIMUM_SMALL_AREA_SIZE
#define S48_MINIMUM_SMALL_AREA_SIZE 32  /* 32 pages * 4KB = 128 KB */
#endif

#ifndef S48_MAXIMUM_SMALL_AREA_SIZE
#define S48_MAXIMUM_SMALL_AREA_SIZE (2*S48_MINIMUM_SMALL_AREA_SIZE)
#endif

/* For weak pointers objects */
#ifndef S48_MINIMUM_WEAK_AREA_SIZE
#define S48_MINIMUM_WEAK_AREA_SIZE 2     /* pages */
#endif

#ifndef S48_MAXIMUM_WEAK_AREA_SIZE
#define S48_MAXIMUM_WEAK_AREA_SIZE 4
#endif


/* 2. Objects */

#ifndef S48_SMALL_OBJECT_LIMIT
#define S48_SMALL_OBJECT_LIMIT PAGES_TO_BYTES_SMALL_CONST_FOR_CPP(16)
#endif

#if BYTES_TO_PAGES(S48_SMALL_OBJECT_LIMIT) > S48_MINIMUM_SMALL_AREA_SIZE
/* I replaced it ...
#error "S48_SMALL_OBJECT_LIMIT has to be smaller than S48_MINIMUM_SMALL_AREA_SIZE"

... with this */
#error "S48_MINIMUM_SMALL_AREA_SIZE has to be equal or greater than S48_SMALL_OBJECT_LIMIT"
#endif


/* 3. Creation Space (Ungar)*/

/* Space for small objects */
/* minimum_allocation_quantum= 128 pages (1p = 4KB) */

#ifndef S48_CREATION_SPACE_SIZE
#define S48_CREATION_SPACE_SIZE (576) /* pages */
#endif

/* Water mark for small objects */

#ifndef S48_ADJUST_WATER_MARK
#define S48_ADJUST_WATER_MARK FALSE
#endif

#ifndef S48_DEFAULT_WATER_MARK
#define S48_DEFAULT_WATER_MARK (S48_CREATION_SPACE_SIZE / 2)
#endif

#if S48_DEFAULT_WATER_MARK >= S48_CREATION_SPACE_SIZE
#error "S48_DEFAULT_WATER_MARK has to be smaller than S48_CREATION_SPACE_SIZE"
#endif

/* If the large objects in the creation space sum up to more than
   this, a collection is triggered. */
#ifndef S48_MAXIMUM_LARGE_CREATION_SPACE_SIZE
#define S48_MAXIMUM_LARGE_CREATION_SPACE_SIZE (2*1024*1024) /* default: 2 MB */
#endif


/* 4. Generations */

/* Number of generations */
#ifndef S48_GENERATIONS_COUNT
#define S48_GENERATIONS_COUNT 4
#endif

#if S48_GENERATIONS_COUNT < 1
#error "S48_GENERATIONS_COUNT has to be a positive number"
#endif


/* 5. Marking cards - Dirty vector */

#ifndef S48_LOG_CARD_SIZE
#define S48_LOG_CARD_SIZE 11
#endif

#define S48_NO_DIRTY_VECTORS 0      /* For tracing everything always */
#define S48_ADDRESS_DIRTY_VECTORS 1

#ifndef S48_DIRTY_VECTOR_METHOD
#define S48_DIRTY_VECTOR_METHOD S48_ADDRESS_DIRTY_VECTORS
#endif

/* Write Barrier Complexity */

/* one can choose the complexity of the write-barrier. Currently there
   are 3 implementations: 0 = every mutated location is traced. 1 =
   only locations that now contain stobs are traced. 2 = only stobs
   pointing into a younger generation are traced. */

#define S48_MUTATED_LOCATION 0
#define S48_STOB_LOCATION 1
#define S48_INTERGEN_STOB_LOCATION 2

#ifndef S48_WRITE_BARRIER_COMPLEXITY
#define S48_WRITE_BARRIER_COMPLEXITY S48_INTERGEN_STOB_LOCATION
#endif


/* 6. Remembered Sets */

#ifndef S48_USE_REMEMBERED_SETS
#define S48_USE_REMEMBERED_SETS TRUE
#endif

#if (S48_USE_REMEMBERED_SETS)
/* one can choose the kind of remembered set. Currently there are 3
   implementations: 0 = dynamically allocated per stob, 1 = static
   allocated space, 2 = static allocated but extensible by-need */

#define S48_DYNAMIC_REMEMBERED_SETS 0
#define S48_STATIC_REMEMBERED_SETS 1
#define S48_EXTENSIBLE_REMEMBERED_SETS 2

#ifndef S48_REMEMBERED_SET_TYPE
#define S48_REMEMBERED_SET_TYPE S48_EXTENSIBLE_REMEMBERED_SETS
#endif

#ifndef S48_REMEMBERED_SET_SIZE
#define S48_REMEMBERED_SET_SIZE 1000
#endif

/* one can choose whether a remembered set may include duplicates or
   not */

#ifndef S48_UNIQUE_REMEMBERED_SET
#define S48_UNIQUE_REMEMBERED_SET TRUE
#endif
#endif /* if (S48_USE_REMEMBERED_SETS) */

/* 7. Policies */

/* Collection policy */
/* Which generation should be collected? */

#ifndef S48_COLLECTION_THRESHOLD
#define S48_COLLECTION_THRESHOLD FALSE
#endif

#ifndef S48_COLLECTION_HEAP_LIMIT 
#define S48_COLLECTION_HEAP_LIMIT  FALSE
#endif

#ifndef S48_COLLECTION_AGE_LIMIT 
#define S48_COLLECTION_AGE_LIMIT  FALSE
#endif


/* If no collection policy is defined ... */
#if (S48_COLLECTION_THRESHOLD || \
     S48_COLLECTION_HEAP_LIMIT || \
     S48_COLLECTION_AGE_LIMIT)
#else
#undef S48_COLLECTION_THRESHOLD
#define S48_COLLECTION_THRESHOLD (1792*1024) /* Default */
#endif

/* If a maximum heap size is defined, and we see that only this
   percent of the maximum could be made free, we force a major
   collection. */
#ifndef S48_EMERGENCY_PERCENTAGE
#define S48_EMERGENCY_PERCENTAGE 10
#endif

/* Promotion policy */
/* Which generation should the live objects be copied to? */

#ifndef S48_PROMOTION_THRESHOLD
#define S48_PROMOTION_THRESHOLD FALSE
#endif

#ifndef S48_PROMOTION_HEAP_LIMIT 
#define S48_PROMOTION_HEAP_LIMIT  FALSE
#endif

#ifndef S48_PROMOTION_AGE_LIMIT 
#define S48_PROMOTION_AGE_LIMIT  FALSE
#endif
     
/* If no promotion policy is defined ... */
#if (S48_PROMOTION_THRESHOLD || \
     S48_PROMOTION_HEAP_LIMIT || \
     S48_PROMOTION_AGE_LIMIT \
     )
#else
#undef S48_PROMOTION_AGE_LIMIT
#define S48_PROMOTION_AGE_LIMIT 2  /* Default */
#endif

/* Wilson's Opportunistic Objects promotion. The Objects become the
   age of their Area according to the allocation. The first part 1/n of
   all Areas are the older ones (i.e. 2 -> 1/2 = half of all Areas) */
#ifndef S48_PART_OF_OLD_AREAS
#define S48_PART_OF_OLD_AREAS 1
#endif

/* From Paper: bigsurvey.ps, page 38 */
#ifndef S48_USE_STATIC_SPACE
#define S48_USE_STATIC_SPACE TRUE
#endif

#if (S48_USE_STATIC_SPACE && S48_GENERATIONS_COUNT < 3)
#error "Static Space option needs at least 3 generations!"
#endif

/* Transport Link Cells - Ghuloum, Dybvig 2007 */
#define S48_HAVE_TRANSPORT_LINK_CELLS TRUE

#endif /* #ifndef __S48_GC_CONFIG */
