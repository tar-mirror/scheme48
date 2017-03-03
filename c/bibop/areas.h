/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: David Frese, Robert Ransom
 */

#ifndef __S48_AREAS_H
#define __S48_AREAS_H

#include "memory.h"
#include "gc_config.h"
#include "remset.h"

/* Areas

 Memory is divided into areas, each of which has:
  start address
  end pointer
  allocation pointer (points to next unused space)
  next area in this category

 Each generation is a set of areas, grouped into lists by metatype and
 fixed-ness
*/

typedef struct {
#if S48_DIRTY_VECTOR_METHOD==S48_ADDRESS_DIRTY_VECTORS
  int length;
  s48_address* items;
#endif
} Dirty_vector;

/* must be synchronized with the Scheme-side definition in ALLOCATION */
typedef enum { AREA_TYPE_SIZE_SMALL, AREA_TYPE_SIZE_LARGE, AREA_TYPE_SIZE_WEAKS,
               AREA_TYPE_SIZE_ILLEGAL }
  area_type_size_t;

typedef enum {GC_ACTION_IGNORE = 0, GC_ACTION_ERROR, GC_ACTION_COPY_MIXED,
	      GC_ACTION_COPY_SMALL, GC_ACTION_MARK_LARGE,
	      GC_ACTION_COPY_WEAK}
  gc_action_t;

typedef struct Area {
  s48_address start;
  s48_address end;
  s48_address frontier;
    
  struct Area* next;
  int generation_index;
  area_type_size_t area_type_size;

  /* only used during collection: */
  gc_action_t action;
  s48_address trace;
  Dirty_vector dirty_vector;
  struct Space* target_space;
#if S48_USE_REMEMBERED_SETS==TRUE
  RemSet* remset;
#endif
} Area;

typedef struct Space {
  int generation_index;
  Area* small_area;
  Area* large_area;
  Area* weaks_area;
} Space;

#define AREA_REMAINING(area) ((area) == NULL ? 0 :\
                     (area)->end - (area)->frontier)


/* Allocate an area of between MINIMUM and MAXIMUM pages, inclusive. */
extern Area* s48_allocate_area_without_crashing(unsigned long minimum,
						unsigned long maximum,
						unsigned char generation_index,
						area_type_size_t area_type_size);
extern Area* s48_allocate_area(unsigned long minimum, unsigned long maximum,
			       unsigned char generation_index, area_type_size_t area_type_size);

/* Remove AREA from the list starting with START */
extern Area* s48_delete_area(Area* start, Area* area);

/* Free all pages covered by AREA, update the memory_map and free Area
   struct itself too. */
extern void s48_free_area(Area* area);

/* Call s48_free_area on all areas in the list starting with START */
extern void s48_free_areas(Area* start);

/* Allocate a block for the whole image */
void s48_allocate_image_area(long bytes, s48_address* start, s48_address* end);

/* Wrap the static make_area */
Area* s48_make_area(s48_address start, s48_address end,
		    s48_address frontier, 
		    unsigned char generation_index,
		    area_type_size_t area_type_size);

#endif
