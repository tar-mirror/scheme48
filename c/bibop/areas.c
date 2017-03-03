/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: David Frese, Robert Ransom
 */

#include <stdlib.h>
#include <string.h>
#include "utils.h"
#include "areas.h"
#include "page_constants.h"
#include "memory_map.h"
#include "area_roots.h"
#include "page_alloc.h"
#include "gc_config.h"
#include "remset.h"

static Area* make_area(s48_address start, s48_address end,
		       s48_address frontier, 
		       unsigned char generation_index,
		       area_type_size_t area_type_size) {
  Area* area = (Area*)malloc(sizeof(Area));
  if (area == NULL) s48_gc_error("make_area: out of memory");
  area->start = start;
  area->end = end;
  area->frontier = frontier;
  area->trace = start;
  area->next = NULL;
  area->generation_index = generation_index;
  area->area_type_size = area_type_size;
  s48_init_dirty_vector(area);
#if S48_USE_REMEMBERED_SETS==TRUE
  area->remset = s48_make_remset();
#endif
  return area;
}

inline static void free_area(Area* area) {
  s48_deinit_dirty_vector(area);
#if S48_USE_REMEMBERED_SETS==TRUE
  s48_free_remset(area->remset);
#endif
  free(area);
}

/* delete_area deletes AREA from the linked list starting with START,
   and returns the (eventually different) new start of the list. Does
   NOT free the memory allocated for the area structure (nor the area
   itself)! */

Area* s48_delete_area(Area* start, Area* area) {
  if (start == NULL)
    /* no areas in the list? -> Done */
    return start;
  else if (start == area) {
    /* list starts with AREA? -> next is new start */
    Area* next = area->next;
    area->next = NULL;
    return next;
  } else {
    /* search for the area before AREA */
    Area* prev = start;
    /* if AREA is not in the list, prev->next can be NULL. But of
       corse this should never happen !? */
    while (prev->next != NULL) {
      if (prev->next == area) {
	prev->next = area->next;
	break;
      }
      prev = prev->next;
    }
    area->next = NULL;
    return start;
  }
}

/* Allocate an area of between MINIMUM and MAXIMUM pages, inclusive. */
    
Area* s48_allocate_area_without_crashing(unsigned long minimum,
					 unsigned long maximum,
					 unsigned char generation_index,
					 area_type_size_t area_type_size) {
  s48_address start;
  Area* area;
  unsigned long size = s48_allocate_pages(minimum, maximum, &start);

  if (size == 0) {
    return NULL;
  };

#if (BIBOP_LOG)
  s48_bibop_log("s48_allocate_pages: size = %i",
	    size);
#endif

  /* Safe because S48_ALLOCATE_PAGES has already checked MINIMUM and
     MAXIMUM with PAGES_TO_BYTES_LOSES_P, and SIZE is less than
     MAXIMUM.

     This call does crash if S48_MAKE_AREA cannot allocate an Area
     struct, but avoiding an out-of-memory crash here is too hard to
     be worthwhile. */
  area = s48_make_area(start, ADD_PAGES_I_KNOW_THIS_CAN_OVERFLOW(start, size),
		       start,
		       generation_index, area_type_size);

  return area;
}

Area* s48_allocate_area(unsigned long minimum, unsigned long maximum,
			unsigned char generation_index,
			area_type_size_t area_type_size) {
  Area* area = s48_allocate_area_without_crashing(minimum, maximum,
						  generation_index,
						  area_type_size);

  if (area == NULL) {
    s48_gc_error("s48_allocate_area: out of memory");
  }

  return area;
}

/* Free the pages covered by AREA, and free the struct itself too. */

void s48_free_area(Area* area) {
  unsigned long size = BYTES_TO_PAGES(area->end - area->start);
  s48_address start = area->start;
  unsigned long i;
  
  s48_free_pagesB(start, size);

  /* This is not really needed, I think. It's only a waste of time */
  for (i = 0; i < size; i++) {
    /* Safe because I is less than SIZE, which cannot cause an
       overflow here. */
    s48_memory_map_setB(ADD_PAGES_I_KNOW_THIS_CAN_OVERFLOW(start, i), NULL);
  }

#ifndef NDEBUG
  /* Blank it out, to find errors more easily */
  memset(area->start, 0, area->end - area->start);
#endif

  free_area(area);
}

/* Call s48_free_area on all areas in the list starting with START */
void s48_free_areas(Area* start) {
  while (start != NULL) {
    Area* next = start->next;
    s48_free_area(start);
    start = next;
  }
}

/* Get the type size of this stob's area: small, large, weaks. Called
   from the BIBOP dumper */
area_type_size_t s48_area_type_size(s48_value stob) {
  Area* area;
  area = s48_memory_map_ref(S48_ADDRESS_AT_HEADER(stob));

  if (area == NULL) {
    return AREA_TYPE_SIZE_ILLEGAL;
  }

  return area->area_type_size;
}


/* Allocate a block for the whole image */
void s48_allocate_image_area(long bytes, s48_address* start, s48_address* end) {
  
  s48_address memory;
  
  memory = (s48_address)malloc(bytes + BYTES_PER_PAGE);
  
  if (memory == NULL) s48_gc_error("s48_allocate_image_area: out of memory\n");
  
  *start = PAGE_START_ADDRESS(memory + BYTES_PER_PAGE - 1);
  *end = PAGE_START_ADDRESS(*start + bytes);
  
  return;
}

/* Wrap the static make_area */
Area* s48_make_area(s48_address start, s48_address end,
		    s48_address frontier, 
		    unsigned char generation_index,
		    area_type_size_t area_type_size) {
  Area* area = make_area(start, end, frontier, generation_index, area_type_size);
  /* The area is put into all memory-map cells that are covered by
     it. */
  int size = BYTES_TO_PAGES(end-start);
  int i;
  for (i = 0; i < size; i++)
    /* Safe because I is less than SIZE, which cannot cause an
       overflow here. */
    s48_memory_map_setB(ADD_PAGES_I_KNOW_THIS_CAN_OVERFLOW(start, i), area);
  return area;
}
