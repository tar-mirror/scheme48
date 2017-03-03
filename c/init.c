/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Richard Kelsey, Jonathan Rees, Martin Gasbichler, Mike Sperber,
 * David Frese, Marcus Crestani
 */

#include <stdlib.h>
#include <stdio.h>
#include "scheme48vm.h"
#include "scheme48heap.h"
#include "scheme48image.h"
#include "ffi.h"

extern long s48_get_file_size(unsigned char *);

#if !defined(DEFAULT_HEAP_SIZE)
/* 4 megacells = 16 megabytes on 32-bit systems */
#define DEFAULT_HEAP_SIZE 4000000L
#endif

#if !defined(DEFAULT_STACK_SIZE)
/* 2500 cells = 10000 bytes */
#define DEFAULT_STACK_SIZE 2500L
#endif

#if defined(STATIC_AREAS) && defined(S48_GC_BIBOP)
#error "The BIBOP GC doesn't support the STATIC_AREAS feature yet."
#endif

#if defined(STATIC_AREAS)
#define DEFAULT_IMAGE_NAME NULL
#else

/* DEFAULT_IMAGE_NAME should be defined using the -D switch to cc. */
#if !defined(DEFAULT_IMAGE_NAME)
#define DEFAULT_IMAGE_NAME "scheme48.image"
#endif

#endif /* STATIC_AREAS */

extern void	s48_sysdep_init(void);
extern void	s48_initialize_external_modules(void);

long
s48_initialize(int *argcp, char ***argv)
{
  char *image_name = DEFAULT_IMAGE_NAME;
  long heap_size = DEFAULT_HEAP_SIZE;    /* in numbers of cells */
  long stack_size = DEFAULT_STACK_SIZE;  /* in numbers of cells */
  int errors = 0;
  char *stack;

#if defined(STATIC_AREAS)
  extern long static_entry;
  extern long static_symbol_table;
  extern long static_imported_binding_table, static_exported_binding_table;
  extern long p_count, *p_areas[], p_sizes[];
  extern long i_count, *i_areas[], i_sizes[];
#endif

  int argc = *argcp;
  int vm_argc = 0;
  char *me = *(*argv);		/* Save program name. */

  {
    /* initialize floating-point printer */
    extern void s48_free_init(void);

    s48_free_init();
  }

  (*argv)++; argc--;		/* Skip program name. */

  for (; argc > 0; argc--, (*argv)++)
    if ((*argv)[0][0] == '-')
      switch ((*argv)[0][1]) {
      case 'h':
	argc--; (*argv)++;
	if (argc == 0) { errors++; break; }
	heap_size = atol(*(*argv));
	if (heap_size < 0) errors++;  /* 0 means now no limit */
	break;
      case 's':
	argc--; (*argv)++;
	if (argc == 0) { errors++; break; }
	stack_size = atoi(*(*argv));
	if (stack_size <= 0) errors++;
	break;
      case 'i':
	argc--; (*argv)++;
	if (argc == 0) { errors++; break; }
	image_name = *(*argv);
	break;
      case 'a':
	argc--;
	vm_argc = argc;    /* remaining args are passed to the VM */
	argc = 0;
	break;
      case 'I':
#ifdef S48_GC_BIBOP
	heap_size = 0;  /* unlimited heap size (BIBOP GC only) */
#endif
	/* code for -i */
	argc--; (*argv)++;
	if (argc == 0) { errors++; break; }
	image_name = *(*argv);
	/* code for -a */
	argc--;
	vm_argc = argc;    /* remaining args are passed to the VM */
	argc = 0;
	break;
      default:
	fprintf(stderr, "Invalid argument: %s\n", *(*argv));
	errors++;
      }
    else
      if ((*argv)[0][0] != '\0') {
	fprintf(stderr, "Invalid argument: %s\n", *(*argv));
	errors++; }
  if (errors != 0) {
    fprintf(stderr,
"Usage: %s [options] [-a arguments]\n\
Options: -h <heap-size>    %s heap size in words (default %ld).%s\n\
	 -s <stack-size>   Stack buffer size in words.\n\
         -i <file>         Load image from file (default \"%s\")\n",
	    me,
#if S48_GC_BIBOP
	    "Maximum",
	    (long)DEFAULT_HEAP_SIZE,
"\n                           A heap size of 0 means the heap can grow\n\
                           unboundedly. This is dangerous because it can\n\
                           cause your system to run out of memory.",
#else
	    "Total",
	    (long)DEFAULT_HEAP_SIZE,
	    "",
#endif
	    DEFAULT_IMAGE_NAME
	    );
    return 1;
  }


  /* Disable GC to read the image and initialize the VM-stack */ 
  s48_forbid_gcB();

  s48_sysdep_init();
  s48_heap_init();
  s48_init();

  if (image_name == NULL) {
#if defined(STATIC_AREAS)
    s48_register_static_areas(p_count, p_areas, p_sizes,
			      i_count, i_areas, i_sizes);
    s48_set_image_valuesB(static_entry,
			  static_symbol_table,
			  static_imported_binding_table,
			  static_exported_binding_table);
    if (s48_initialize_heap(heap_size) == NULL) {
      fprintf(stderr, "system is out of memory\n");
      return 1; }
#else
    fprintf(stderr, "No image file.\n");
    return 1;
#endif
  } else if (s48_read_image(image_name, heap_size) == -1) {
    fprintf(stderr, "Image file \"%s\" is unusable.\n", image_name);
    return 1; }
  
  stack = (char *) malloc(stack_size * sizeof(long));
    
  if (!stack) {
    fprintf(stderr, "system is out of memory\n");
    return 1; }

  s48_initialize_ffi();

  s48_initialize_vm(stack, stack_size);

  s48_initialize_external_modules();

  /* Heap und stack are ok. Enable the GC. */
  s48_allow_gcB();

  *argcp = vm_argc;

  return 0;
}
