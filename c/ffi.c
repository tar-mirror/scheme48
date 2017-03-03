/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Marcus Crestani, Harald Glab-Phlak
 */

/* Modelled on Jim Blandy's foreign function interface that he put in
   his Scheme implementation called Minor. */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "scheme48.h"
#include "scheme48vm.h"
#include "scheme48heap.h"
#include "ffi.h"

/* structs */

struct ref_group;

struct s48_ref_s
{
  s48_value obj;
  struct ref_group *group;
};

struct ref;

struct ref
{
  struct s48_ref_s x;
  struct ref *next, *prev;
};

#define NUM_REFS_PER_CLUMP 85

struct ref_clump;

struct ref_clump
{
  struct ref_clump *next;
  struct ref refs[NUM_REFS_PER_CLUMP];
};

struct ref_group
{
  struct ref_clump *clumps;
  struct ref *free;
  struct ref *last_free;
  short first_never_used;
  struct ref allocated;
};

struct buf_group;

struct buf_group
{
  void *buffer;
  struct buf_group *next, *prev;
};

enum BV_MODE { READWRITE, READONLY };

struct bv_group;

struct bv_group
{
  char *buffer;
  s48_ref_t byte_vector;
  enum BV_MODE mode;
  struct bv_group *next, *prev;
};

struct s48_call_s
{
  s48_call_t older_call;
  s48_call_t subcall_parent;
  s48_call_t child;
  s48_call_t next_subcall, prev_subcall;
  struct ref_group *local_refs;
  struct buf_group *local_bufs;
  struct bv_group *local_bvs;
};


/* global states */
static s48_call_t current_call = NULL;
static struct ref_group *global_ref_group = NULL;

#define GLOBAL_REF_P(ref) (ref->group == global_ref_group)

/* REFS */

static struct ref_group *
make_ref_group (void)
{
  struct ref_group *g = (struct ref_group *) malloc (sizeof (struct ref_group));
  if (g == NULL)
    s48_out_of_memory_error();
  memset (g, 0, sizeof (*g));

  g->clumps = 0;
  g->free = 0;
  g->allocated.next = &g->allocated;
  g->allocated.prev = &g->allocated;
  return g;
}

static void
free_ref_group (struct ref_group *g)
{
  struct ref_clump *c, *next;
  for (c = g->clumps; c; c = next) {
    next = c->next;
    free (c);
  }
  free (g);
}

static s48_ref_t
make_ref (struct ref_group *g, s48_value obj)
{
  struct ref *r;

  if (g->clumps && (g->first_never_used < NUM_REFS_PER_CLUMP))
    r = &g->clumps->refs[g->first_never_used++];
  else if (g->free) {
    r = g->free;
    g->free = r->next;
  } else {
    struct ref_clump *new =
      (struct ref_clump *) malloc (sizeof (struct ref_clump));
    if (new == NULL)
      s48_out_of_memory_error();

    new->next = g->clumps;
    g->clumps = new;
    r = &new->refs[0];
    g->first_never_used = 1;
  }

  r->next = g->allocated.next;
  r->prev = &g->allocated;
  r->next->prev = r;
  r->prev->next = r;
  r->x.group = g;
  r->x.obj = obj;

  return &r->x;
}

static void
free_ref (s48_ref_t x)
{
#ifdef DEBUG_FFI
  fprintf (stderr, "free ref with scheme value %x\n", s48_deref(x));
#endif
  struct ref *r = (struct ref *) x;
  struct ref_group *g = r->x.group;

  r->next->prev = r->prev;
  r->prev->next = r->next;
  r->next = 0;
  if (g->free) {
    g->last_free->next = r;
    g->last_free = r;
  } else
    g->free = g->last_free = r;
  r->x.obj = S48_FALSE;
}

static void
walk_ref_group (struct ref_group *g,
		void (*func) (s48_ref_t ref, void *closure),
		void *closure)
{
  struct ref *r;
  struct ref *head = &g->allocated;
  for (r = head->next; r != head; r = r->next)
    func (&r->x, closure);
}


/* LOCAL REFS */

s48_ref_t
s48_make_local_ref (s48_call_t call, s48_value obj)
{
#ifdef DEBUG_FFI
  fprintf (stderr, "make local ref from scheme value %x\n", obj);
#endif
  return make_ref (call->local_refs, obj);
}

s48_ref_t
s48_copy_local_ref (s48_call_t call, s48_ref_t ref)
{
  s48_ref_t r = s48_make_local_ref (call, s48_deref(ref));
  return r;
}

void
s48_free_local_ref (s48_call_t call, s48_ref_t ref)
{
#ifdef DEBUG_FFI
  fprintf (stderr, "free local ref with scheme value %x\n", s48_deref(ref));
#endif
  if (!GLOBAL_REF_P (ref))
    free_ref (ref);
  else 
    s48_assertion_violation ("s48_free_localref", "ref is not local", 0);
}

void
s48_free_local_ref_array (s48_call_t call, s48_ref_t *refs, size_t len)
{
  size_t i;
  for (i = 0; i < len; i++)
    s48_free_local_ref (call, refs[i]);
}


/* GLOBAL REFS */

s48_ref_t
s48_make_global_ref (s48_value obj)
{
#ifdef DEBUG_FFI
  fprintf (stderr, "make global ref from scheme value %x\n", obj);
#endif
  return make_ref (global_ref_group, obj);
}

void
s48_free_global_ref (s48_ref_t ref)
{
#ifdef DEBUG_FFI
  fprintf (stderr, "free global ref from scheme value %x\n", s48_deref(ref));
#endif
  if (GLOBAL_REF_P (ref))
    free_ref (ref);
  else
    s48_assertion_violation ("s48_free_global_ref", "ref is not global", 0);
}

s48_ref_t
s48_local_to_global_ref(s48_ref_t ref)
{
  s48_value temp = s48_deref(ref);
#ifdef DEBUG_FFI
  fprintf (stderr, "local to global ref from scheme value %x\n", s48_deref(ref));
#endif
  free_ref (ref);
  return s48_make_global_ref(temp);
}

static void
walk_global_refs (void (*func) (s48_ref_t ref, void *closure),
		  void *closure)
{
  walk_ref_group (global_ref_group, func, closure);
}


/* BUFS */

struct buf_group *
make_buf_group (void)
{
  struct buf_group *g = (struct buf_group *) malloc (sizeof (struct buf_group));
  if (g == NULL)
    s48_out_of_memory_error();
#ifdef DEBUG_FFI
  fprintf (stderr, "make buf group %x\n", g);
#endif
  return g;
}

void
free_buf (struct buf_group *b)
{
#ifdef DEBUG_FFI
  fprintf (stderr, "free buf %x\n", b);
#endif
  free (b->buffer);
  free (b);
}

void
free_buf_group (struct buf_group *g)
{
  struct buf_group *b, *next;
#ifdef DEBUG_FFI
  fprintf (stderr, "free buf group %x\n", g);
#endif
  for (b = g; b; b = next) {
    next = b->next;
    free_buf (b);
  }
}

void *
s48_make_local_buf (s48_call_t call, size_t s)
{
  struct buf_group *g = make_buf_group ();
#ifdef DEBUG_FFI
  fprintf (stderr, "make buf with size %x\n", s);
#endif
  g->buffer = (void *) calloc (1, s);
  if (g->buffer == NULL)
    s48_out_of_memory_error();
  g->prev = NULL;
  g->next = call->local_bufs;
  if (g->next)
    g->next->prev = g;
  call->local_bufs = g;
  return g->buffer;
}

void
s48_free_local_buf (s48_call_t call, void *buffer)
{
  struct buf_group *prev, *b, *next;

  if (! call->local_bufs)
    return;

#ifdef DEBUG_FFI
  fprintf (stderr, "free buf %x\n", buffer);
#endif

  if (buffer == call->local_bufs->buffer) {
    b = call->local_bufs;
    call->local_bufs = call->local_bufs->next;
    if (call->local_bufs)
      call->local_bufs->prev = NULL;
    free_buf (b);
    return;
  }
  
  prev = call->local_bufs;
  b = call->local_bufs->next;
  while (b) {
    if (buffer == b->buffer) {
      next = b->next;
      prev = b->prev;
      prev->next = next;
      if (next)
	next->prev = prev;
      free_buf (b);
      b = NULL;
    } else {
      b = b->next;
    }
  }
}


/* BYTE VECTORS */

struct bv_group *
make_bv_group (void)
{
  struct bv_group *g = (struct bv_group *) malloc (sizeof (struct bv_group));
  if (g == NULL)
    s48_out_of_memory_error();
#ifdef DEBUG_FFI
  fprintf (stderr, "make bv group %x\n", g);
#endif
  return g;
}


static void
copy_to_bv (s48_call_t call, struct bv_group *bv, void *closure)
{
  if (bv->mode != READONLY)
    s48_copy_to_byte_vector_2(call, bv->byte_vector, bv->buffer);
}

static void
copy_from_bv (s48_call_t call, struct bv_group *bv, void *closure)
{
  s48_copy_from_byte_vector_2(call, bv->byte_vector, bv->buffer);
}

void
free_bv (s48_call_t call, struct bv_group *b)
{
#ifdef DEBUG_FFI
  fprintf (stderr, "free bv %x\n", b);
#endif
  copy_to_bv (call, b, NULL);
  free (b->buffer);
  free (b);
}

void
free_bv_group (s48_call_t call, struct bv_group *g)
{
  struct bv_group *b, *next;
#ifdef DEBUG_FFI
  fprintf (stderr, "free bv group %x\n", g);
#endif
  for (b = g; b; b = next) {
    next = b->next;
    free_bv (call, b);
  }
}

struct bv_group *
s48_find_local_bv (s48_call_t call, s48_ref_t byte_vector, long s)
{
  struct bv_group *b;

  if (! call->local_bvs)
    return NULL;

  if (s48_eq_p_2 (call, byte_vector, call->local_bvs->byte_vector)) {
    return call->local_bvs;
  }
  
  b = call->local_bvs->next;
  while (b) {
    if (s48_eq_p_2 (call, byte_vector, b->byte_vector)) {
      return b;
    } else {
      b = b->next;
    }
  }

  return NULL;
}

char *
s48_really_make_local_bv (s48_call_t call, s48_ref_t byte_vector, long s, enum BV_MODE mode)
{
  struct bv_group *g = make_bv_group ();
#ifdef DEBUG_FFI
  fprintf (stderr, "make bv with size %x\n", s);
#endif
  g->buffer = (char *) calloc (1, s);
  if (g->buffer == NULL)
    s48_out_of_memory_error();
  g->byte_vector = byte_vector;
  g->mode = mode;
  g->prev = NULL;
  g->next = call->local_bvs;
  if (g->next)
    g->next->prev = g;
  call->local_bvs = g;
  return g->buffer;
}

psbool     s48_unmovable_p (s48_call_t, s48_ref_t);

char *
s48_maybe_make_local_bv (s48_call_t call, s48_ref_t byte_vector, long s, enum BV_MODE mode)
{
  char *buf;
  struct bv_group *b;

  if (s48_unmovable_p(call, byte_vector))
    {
      return s48_extract_unmovable_byte_vector_2(call, byte_vector);
    }

  b = s48_find_local_bv (call, byte_vector, s);
  if (b)
    {
      b->mode = mode;
      return b->buffer;
    }
  else
    {
      buf = s48_really_make_local_bv (call, byte_vector, s, mode);
      s48_extract_byte_vector_region_2(call, byte_vector, 0, s, buf);
      return buf;
    }
}

char *
s48_make_local_bv (s48_call_t call, s48_ref_t byte_vector, long s)
{
  return s48_maybe_make_local_bv(call, byte_vector, s, READWRITE);
}

char *
s48_make_local_bv_readonly (s48_call_t call, s48_ref_t byte_vector, long s)
{
  return s48_maybe_make_local_bv(call, byte_vector, s, READONLY);
}

void
s48_free_local_bv (s48_call_t call, char *buffer)
{
  struct bv_group *prev, *b, *next;

  if (! call->local_bvs)
    return;

#ifdef DEBUG_FFI
  fprintf (stderr, "free bv %x\n", buffer);
#endif

  if (buffer == call->local_bvs->buffer) {
    b = call->local_bvs;
    call->local_bvs = call->local_bvs->next;
    if (call->local_bvs)
      call->local_bvs->prev = NULL;
    free_bv (call, b);
    return;
  }
  
  prev = call->local_bvs;
  b = call->local_bvs->next;
  while (b) {
    if (buffer == b->buffer) {
      next = b->next;
      prev = b->prev;
      prev->next = next;
      if (next)
	next->prev = prev;
      free_bv (call, b);
      b = NULL;
    } else {
      b = b->next;
    }
  }
}

static void
walk_local_bvs (s48_call_t call,
		void (*func) (s48_call_t call, struct bv_group *bv, void *closure),
		void *closure)
{
  struct bv_group *b;

  for (b = call->local_bvs; b; b = b->next)
    func (call, b, closure);
}

void
s48_copy_local_bvs_to_scheme (s48_call_t call)
{
  walk_local_bvs (call, copy_to_bv, NULL);
}

void
s48_copy_local_bvs_from_scheme (s48_call_t call)
{
  walk_local_bvs (call, copy_from_bv, NULL);
}


/* CALLS */

static s48_call_t
really_make_call (s48_call_t older_call)
{
  s48_call_t new = (s48_call_t ) malloc (sizeof (struct s48_call_s));
  if (new == NULL)
    s48_out_of_memory_error();
  memset (new, 0, sizeof (*new));
  new->local_refs = make_ref_group ();
  new->older_call = older_call;
  new->subcall_parent = NULL;
  new->child = NULL;
  new->local_bufs = NULL;
  new->local_bvs = NULL;
  return new;
}

s48_call_t
s48_push_call (s48_call_t call)
{
#ifdef DEBUG_FFI
    fprintf (stderr, "push\n");
#endif
  current_call = really_make_call (call);
  return current_call;
}

static void
free_call (s48_call_t call)
{
  if (call->child) {
    s48_call_t c = call->child;

    do {
      s48_call_t temp = c;
      c = c->next_subcall;
      free_call (temp);
    } while (c != call->child);
  }
  free_bv_group (call, call->local_bvs);
  free_ref_group (call->local_refs);
  free_buf_group (call->local_bufs);
#ifdef DEBUG_FFI
  fprintf (stderr, "free_call\n");
  fprintf(stderr, "  count calls: %d, localrefs: %d, globalrefs: %d\n",
	  count_calls(), count_local_refs (), count_global_refs());
#endif
  free (call);
}

void
s48_pop_to (s48_call_t call)
{
  while (current_call != call) {
    s48_call_t here = current_call;
    if (!here)
      s48_assertion_violation ("s48_pop_to", "current_call is null", 0);
    current_call = here->older_call;
    free_call (here);
#ifdef DEBUG_FFI
    fprintf (stderr, "pop\n");
#endif
  }
}


/* SUBCALLS */

s48_call_t
s48_make_subcall (s48_call_t call)
{
  s48_call_t new = (s48_call_t ) malloc (sizeof (struct s48_call_s));
  if (new == NULL)
    s48_out_of_memory_error();
  memset (new, 0, sizeof (*new));
  new->local_refs = make_ref_group ();
  new->older_call = NULL;
  new->subcall_parent = call;
  new->child = NULL;

  if (call->child) {
    new->next_subcall = call->child->next_subcall;
    new->prev_subcall = call->child;
    new->next_subcall->prev_subcall = new;
    new->prev_subcall->next_subcall = new;
  } else {
    new->next_subcall = new->prev_subcall = new;
    call->child = new;
  }

  return new;
}

void
s48_free_subcall (s48_call_t subcall)
{
  s48_call_t parent = subcall->subcall_parent;
  if (subcall->next_subcall == subcall) {
    parent->child = NULL;
  } else {
    parent->child = subcall->next_subcall;
    subcall->prev_subcall->next_subcall = subcall->next_subcall;
    subcall->next_subcall->prev_subcall = subcall->prev_subcall;
  }
  free_call (subcall);
}

s48_ref_t
s48_finish_subcall (s48_call_t call, s48_call_t subcall, s48_ref_t ref)
{
  s48_ref_t result = ref ? s48_copy_local_ref (call, ref) : NULL;
  s48_free_subcall (subcall);
  return result;
}

static void
walk_call (s48_call_t call,
	   void (*func) (s48_ref_t, void *closure),
	   void *closure)
{
  s48_call_t c = NULL;
  walk_ref_group (call->local_refs, func, closure);
  c = call->child;
  if (c)
    do
      walk_call (c, func, closure);
    while ((c = c->next_subcall) != call->child);
}

static void
walk_local_refs (void (*func) (s48_ref_t, void *closure), void *closure)
{
  s48_call_t c;
  for (c = current_call; c; c = c->older_call)
    walk_call (c, func, closure);
}

#ifdef DEBUG_FFI /* for debugging */
static void
count_a_ref (s48_ref_t ref, void *closure)
{
  size_t *count_p = closure;
  (*count_p)++;
}

static size_t
count_global_refs ()
{
  size_t count = 0;
  walk_global_refs (count_a_ref, &count);
  return count;
}

static size_t
count_local_refs ()
{
  size_t count = 0;
  walk_local_refs (count_a_ref, &count);
  return count;
}

static size_t
count_calls ()
{
  size_t count;
  s48_call_t c;
  for (c = current_call, count = 0; c; c = c->older_call, count++);
  return count;
}
#endif

void
s48_setref (s48_ref_t ref, s48_value obj)
{
  ref->obj = obj;
}

s48_value 
s48_deref (s48_ref_t ref)
{
  return ref->obj;
}

s48_call_t
s48_first_call (void)
{
  return really_make_call (NULL);
}

s48_call_t
s48_get_current_call (void)
{
  return current_call;
}

void
s48_initialize_ffi (void)
{
  if (current_call)
    s48_assertion_violation ("s48_init_ffi", "current_call is already set", 0);
  current_call = s48_first_call ();

  if (global_ref_group)
    s48_assertion_violation ("s48_init_ffi", "global_ref_group is already set", 0);
  global_ref_group = make_ref_group ();
}

static void
trace_a_ref (s48_ref_t ref, void *closure)
{
  (*(size_t *) closure)++;
  s48_setref(ref, s48_trace_value (s48_deref(ref)));
}

void
s48_trace_external_calls (void)
{
  size_t cnt_locals = 0;
  size_t cnt_globals = 0;
  walk_local_refs (trace_a_ref, &cnt_locals);
  walk_global_refs (trace_a_ref, &cnt_globals);
#ifdef DEBUG_FFI
  fprintf(stderr, "### TRACED locals %d    globals %d ###\n", cnt_locals, cnt_globals);
#endif
}


#ifdef DEBUG_FFI
/* TESTS */

static s48_ref_t
test_0 (s48_call_t call)
{
  fprintf(stderr, "test_0\n");
  fprintf(stderr, "  count calls: %d, localrefs: %d, globalrefs: %d\n",
	  count_calls(), count_local_refs (), count_global_refs());
  return s48_make_local_ref (call, _s48_value_true);
}

static s48_ref_t
test_1 (s48_call_t call, s48_ref_t ref_1)
{
  s48_ref_t result;

  fprintf(stderr, ">>> %d <<<\n", s48_extract_fixnum (s48_deref(ref_1)));
  /*
  s48_ref_t proc =
    s48_make_local_ref (call,
			S48_SHARED_BINDING_REF(s48_get_imported_binding ("display")));
  fprintf(stderr, "> test_1\n");
  fprintf(stderr, "  count calls: %d, localrefs: %d, globalrefs: %d\n",
	  count_calls(), count_local_refs (), count_global_refs());
  result = s48_call_scheme_2 (call, proc, 1, ref_1);
  fprintf(stderr, "  count calls: %d, localrefs: %d, globalrefs: %d\n",
	  count_calls(), count_local_refs (), count_global_refs());
  fprintf(stderr, "< test_1\n");
  */
  return result;
}

static s48_ref_t
call_thunk (s48_call_t call, s48_ref_t thunk)
{
  s48_ref_t result;
  fprintf(stderr, "> call_thunk\n");
  fprintf(stderr, "  count calls: %d, localrefs: %d, globalrefs: %d\n",
	  count_calls(), count_local_refs (), count_global_refs());
  result = s48_call_scheme_2 (call, thunk, 0);
  fprintf(stderr, "  count calls: %d, localrefs: %d, globalrefs: %d\n",
	  count_calls(), count_local_refs (), count_global_refs());
  fprintf(stderr, "< call_thunk\n");
  return result;
}

static s48_ref_t
call_unary (s48_call_t call, s48_ref_t unary, s48_ref_t arg)
{
  s48_ref_t result;
  fprintf(stderr, "> call_unary\n");
  fprintf(stderr, "  count calls: %d, localrefs: %d, globalrefs: %d\n",
	  count_calls(), count_local_refs (), count_global_refs());
  result = s48_call_scheme_2 (call, unary, 1, arg);
  fprintf(stderr, "  count calls: %d, localrefs: %d, globalrefs: %d\n",
	  count_calls(), count_local_refs (), count_global_refs());
  fprintf(stderr, "< call_unary\n");
  return result;
}

void
init_debug_ffi (void)
{
  S48_EXPORT_FUNCTION(test_0);
  S48_EXPORT_FUNCTION(test_1);
  S48_EXPORT_FUNCTION(call_thunk);
  S48_EXPORT_FUNCTION(call_unary);
  S48_EXPORT_FUNCTION(s48_length_2);
}

/*
; ,open external-calls primitives

(import-lambda-definition-2 call-thunk (thunk))
(import-lambda-definition-2 call-unary (proc arg))

(call-thunk
 (lambda ()
   (call-with-current-continuation
    (lambda (cont)
      (call-thunk
       (lambda ()
         (call-thunk
          (lambda ()
	    (collect)
            (call-unary cont 23)))))))))

*/

#endif
