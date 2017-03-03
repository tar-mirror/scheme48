/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: David Frese
 */

/* Implements area_roots.h */

#include <stdlib.h>

#include "scheme48.h"

#include "area_roots.h"
#include "areas.h"
#include "memory.h"
#include "memory_map.h"
#include "utils.h"
#include "data.h"
#include "measure.h"
#include "generation_gc.h"
#include "gc_config.h"

#if (S48_USE_REMEMBERED_SETS)
#include "remset.h"
#endif

/* initializes the dirty vector of AREA */
void s48_init_dirty_vector(Area* area) {

#if S48_DIRTY_VECTOR_METHOD==S48_ADDRESS_DIRTY_VECTORS
  Dirty_vector* dv = &area->dirty_vector;
  unsigned long area_size = area->end - area->start;
  unsigned long number_of_cards = area_size >> S48_LOG_CARD_SIZE ;
  dv->length = number_of_cards;
  /* A vector of pointers */
  dv->items = (s48_address*)calloc(sizeof(s48_address), number_of_cards);
#endif

}

/* deinitializes the dirty vector of AREA. (should free the memory
   allocated in init_dirty_vector */
void s48_deinit_dirty_vector(Area* area) {
#if S48_DIRTY_VECTOR_METHOD==S48_ADDRESS_DIRTY_VECTORS
  free(area->dirty_vector.items);
#endif
}

#if (MEASURE_GC)
static unsigned long areas_visited = 0;
static unsigned long areas_passed = 0;
#endif

inline static void call_trace_locationsB(Area* area,
					 s48_address start_address,
					 s48_address end_address) {
#if (MEASURE_GC)
  areas_passed += (end_address - start_address);
#endif
  
  s48_internal_trace_locationsB(area, TRUE, start_address, end_address,
				"s48_trace_areas_roots");
}

/* trace (all possible) pointers to collected generations, in the
   old/uncollected area AREA */
void trace_area_roots(Area* area)
{
  /* the tracing can always be stopped at the trace pointer, because
     the part behind that (objects added the the area during the
     collection) will be traced later anyway. And the cards behind
     that will not be marked either. */

#if (MEASURE_GC)
  areas_visited += areas->frontier - areas->start;
#endif

#if S48_DIRTY_VECTOR_METHOD==S48_NO_DIRTY_VECTORS
  /* Without a dirty vector, we trace everything to be sure to catch
     all intergenerational pointers. */
  call_trace_locationsB(area, area->start, area->trace);
#endif

#if S48_DIRTY_VECTOR_METHOD==S48_ADDRESS_DIRTY_VECTORS
  /* This method stores the first location of an intergenerational
     pointer within a card (a fixed-sized part of the area). */
  Dirty_vector* dirty_vector = &area->dirty_vector;
  s48_address card_start_address = area->start;
  int i;

  for (i = 0;
       (i < dirty_vector->length) && (card_start_address < area->trace);
       i++, card_start_address += S48_CARD_SIZE) {
    s48_address start_address = dirty_vector->items[i];
    if (start_address != NULL) {
      s48_address end_address = card_start_address + S48_CARD_SIZE;

      /* no need to trace behind trace pointer */
      if (end_address > area->trace) end_address = area->trace;
      
      /* checks */
      if (start_address < card_start_address)
	s48_gc_error("s48_trace_areas_roots: dirty address too small.");
      if (start_address >= end_address)
	s48_gc_error("s48_trace_areas_roots: dirty address too big.");

      /* reset */
      dirty_vector->items[i] = NULL;

      /* trace */
      call_trace_locationsB(area, start_address, end_address);
    }
  } // for loop over dirty vector
#endif

}

/* FPage 9 */

/* passes all dirty regions in all areas in the linked list starting
   with AREAS, to trace_locationsB */
void s48_trace_areas_roots(Area* areas) {
  while(areas != NULL) {
    trace_area_roots(areas);
    areas = areas->next;
  }
      
#if (MEASURE_GC)
  measure_areas_roots(areas_visited, areas_passed);
  areas_visited = 0;
  areas_passed = 0;
#endif
  
}
    
void s48_set_dirty_vector(Area* area, s48_address addr, long stob,
			  Area* maybe_to_area) {
  s48_set_dirty_vector_inline(area, addr, stob, maybe_to_area);
}
 
void s48_write_barrier(long stob, s48_address address, long value) {
  s48_write_barrier_inline(stob, address, value);
}
