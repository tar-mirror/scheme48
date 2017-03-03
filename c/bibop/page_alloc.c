/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: David Frese
 */

#include <stdlib.h>
#include <assert.h>
#include "utils.h"
#include "page_constants.h"
#include "page_alloc.h"
#include "memory.h"

/* A doubly linked list of free areas. (Not to be confused with Areas!) */

typedef struct FreeArea {
  s48_address start;       /* first page */
  unsigned long size; /* number of pages */
  struct FreeArea* next;
  struct FreeArea* prev;
} FreeArea;

static FreeArea* freelist;

static unsigned long free_area_count = 0;
static unsigned long free_page_count = 0;

static FreeArea* make_free_area(s48_address start, unsigned long size) {
  FreeArea* new = (FreeArea*)malloc(sizeof(FreeArea));
  if (new == NULL) s48_gc_error("make_free_area: out of memory");
  new->start = start;
  new->size = size;
  new->next = NULL;
  new->prev = NULL;
  free_area_count = free_area_count + 1;
  return new;
}

inline static void deallocate_free_area(FreeArea* area) {
  free(area);
  free_area_count = free_area_count - 1;
}

inline static void connect(FreeArea* first, FreeArea* second) {
  second->prev = first;
  first->next = second;
}

inline static s48_address address_after_free_area(FreeArea* area) {
  /* Presumed safe because AREA->SIZE is the size in pages of an
     allocated block of memory. */
  return area->start + PAGES_TO_BYTES_I_KNOW_THIS_CAN_OVERFLOW(area->size);
}

inline static void adjust_free_area(FreeArea* area, unsigned long pages) {
  area->size = area->size + pages;
}

void s48_initialize_page_allocation() {
  freelist = make_free_area(0, 0);
  connect(freelist, freelist);
  free_area_count = 0;
  free_page_count = 0;
}

static void check_freelist() {
  FreeArea* area = freelist->next;
  unsigned int areas = 0, pages = 0;
  while (1) {
    s48_address end = address_after_free_area(area);
    if (area == freelist) {
      /* now we've seen the whole list */
      if ((areas != free_area_count) || (pages != free_page_count))
	s48_gc_error("bad page freelist (1)");
      else return; /* the list is OK */	 
    } else if ((end < area->start) ||
	       ((freelist != area->prev) &&
		(area->start < address_after_free_area(area->prev)))) {
      s48_gc_error("bad page freelist (2)");
    } else {
      char dummy; unsigned long addr;
      /* check if area has correct addresses */
      addr = (unsigned long)(area->start);
      dummy = *(area->start);
      
      if ((addr % 4) != 0) {
	s48_gc_error("bad page start address");
      }
      
      areas = areas + 1;
      pages = pages + area->size;
      area = area->next;
      /* LOOP */
    }
  }
  /* Never reached */
}

/* Add SIZE pages starting from START to the set of free pages.  We
   walk down the list of free areas to find where START goes and then
   either merge with an existing area or create a new one. */

void s48_free_pagesB(s48_address start, unsigned long size) {
  s48_address end = start + PAGES_TO_BYTES_I_KNOW_THIS_CAN_OVERFLOW(size);
  FreeArea* before = freelist;
  int done;

  if (PAGES_TO_BYTES_LOSES_P(size)) {
    s48_gc_error("s48_free_pagesB: integer overflow detected too late to avoid"
		 " crash (%li pages requested)", size);
  };

  free_page_count = free_page_count + size;
  
  do {
    FreeArea* after = before->next;
    done = 1; /* true */
    if (after == freelist) {
      /* we're last */
      if ( (start == address_after_free_area(before)) &&
	   (before != freelist))
	adjust_free_area(before, size);
      else {
	FreeArea* new = make_free_area(start, size);
	connect(before, new);
	connect(new, after);
      }
    } else {
      s48_address end_of_previous = address_after_free_area(before);
      assert(end_of_previous <= start);
      if (after->start < start) {
	/* we're after AFTER */
	before = after;
	done = 0; /* LOOP */
      } else {
	assert(end <= after->start);
	if ((start == end_of_previous) &&
	    (before != freelist)) {
	  /* merge us with BEFORE */
	  adjust_free_area(before, size);
	  if (end == after->start) {
	    /* and with AFTER, deleting AFTER */
	    adjust_free_area(before, after->size);
	    connect(before, after->next);
	    deallocate_free_area(after);
	  }
	} else if (end == after->start) {
	  /* merge us with AFTER */
	  after->start = start;
	  adjust_free_area(after, size);
	} else {
	  /* nothing doing, we're on our own */
	  FreeArea* new = make_free_area(start, size);
	  connect(before, new);
	  connect(new, after);
	}
      }
    }
  } while (!done);

  check_freelist();
}

/* Getting more memory from the OS */

/* Grab at least a quarter-megabyte (2**18) at a time. */
/* minimum_allocation_quantum = 64 Pages (= 64 * 4KB = 256 KB)  */

static unsigned long minimum_allocation_quantum = BYTES_TO_PAGES(2 << 18);


#define generic_max(a, b) ((a < b) ? b : a)

/* We grab the memory and then cut it down to even page boundaries. */
/* Returns 0 on failure, non-zero on success. */

static int get_more_memory(unsigned long minimum) {
  unsigned long ask_for = generic_max(minimum, minimum_allocation_quantum) + 1;
  /* may lose up to one full page on the ends */
  unsigned long size = PAGES_TO_BYTES_I_KNOW_THIS_CAN_OVERFLOW(ask_for);
  s48_address memory;
  if (PAGES_TO_BYTES_LOSES_P(ask_for)) {
    s48_gc_error("get_more_memory: integer overflow detected too late to avoid"
		 " crash (%li pages requested)", minimum);
  };
  memory = (s48_address)malloc(size);
  if (memory == NULL)
    return 0;
  else {
    s48_address start = PAGE_START_ADDRESS(memory);
    if (start == memory)
      s48_free_pagesB(start, ask_for);
    else
      s48_free_pagesB(ADD_PAGES_I_KNOW_THIS_CAN_OVERFLOW(start, 1),
		      ask_for - 1);
    return 1;
  }
}

/* Do a first-fit search of the free list to find a free section of
   between MINIMUM and MAXIMUM pages, inclusive. */

/* Returns the number of pages allocated (between MINIMUM and MAXIMUM,
   or 0 on failure). */

unsigned long s48_allocate_pages(unsigned long minimum,
				 unsigned long maximum,
				 s48_address* start) {
  FreeArea* area = freelist->next;

  if (PAGES_TO_BYTES_LOSES_P(minimum) || PAGES_TO_BYTES_LOSES_P(maximum)) {
    s48_gc_error("s48_allocate_pages: integer overflow detected too late to"
		 " avoid crash (%li..%li pages requested)", minimum, maximum);
  };

  while (1) {
    if (area == freelist) {
      if (!get_more_memory(minimum)) {
	return 0;
      };
      area = freelist->next; /* LOOP */
    } else if (area->size < minimum) {
      area = area->next; /* LOOP */
    } else if (maximum < area->size) {
      *start = area->start;
      area->start = ADD_PAGES_I_KNOW_THIS_CAN_OVERFLOW(area->start, minimum);

      /* This integer overflow seems to be harmless. */
      adjust_free_area(area, - ((long) minimum));

      free_page_count = free_page_count - minimum;
      check_freelist();
      return minimum;
    } else {
      unsigned long size = area->size;
      *start = area->start;
      connect(area->prev, area->next);
      deallocate_free_area(area);
      free_page_count = free_page_count - size;
      check_freelist();
#if (BIBOP_LOG)
  s48_bibop_log("s48_allocate_pages: minimum %li < area->size %li < maximum %li",
	    minimum, size, maximum);
#endif
      return size;
    }
  }
  /* Never reached */
}

