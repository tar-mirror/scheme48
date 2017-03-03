/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Richard Kelsey, Jonathan Rees, Marcus Crestani, Mike Sperber,
 * Robert Ransom, Harald Glab-Phlak, Marcel Turino
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include <stdarg.h>

#include "c-mods.h"
#include "scheme48.h"
#include "scheme48vm.h"
#include "bignum.h"
#include "ffi.h"

/*
 * The Joy of C
 * I don't understand why we need this, but we do.
 */

struct s_jmp_buf {
  jmp_buf buf;
};

/*
 * Longjump target set up by the most recent call into C.
 */
static struct s_jmp_buf	current_return_point;

/*
 * The name of the procedure we are currently executing; used for error messages.
 */
static s48_ref_t current_procedure = NULL;

/*
 * Stack of Scheme stack-block records which represent portions of the process
 * stack.
 */
static s48_ref_t current_stack_block = NULL;

/*
 * These need to agree with the record definition in callback.scm.
 */
#define STACK_BLOCK_FREE(stack_block)	S48_UNSAFE_RECORD_REF(stack_block, 0)
#define STACK_BLOCK_UNWIND(stack_block)	S48_UNSAFE_RECORD_REF(stack_block, 1)
#define STACK_BLOCK_PROC(stack_block)	S48_UNSAFE_RECORD_REF(stack_block, 2)
#define STACK_BLOCK_THREAD(stack_block)	S48_UNSAFE_RECORD_REF(stack_block, 3)
#define STACK_BLOCK_NEXT(stack_block)	S48_UNSAFE_RECORD_REF(stack_block, 4)

#define STACK_BLOCK_FREE_2(c, stack_block)	\
  s48_unsafe_record_ref_2(c, stack_block, 0)
#define STACK_BLOCK_UNWIND_2(c, stack_block)	\
  s48_unsafe_record_ref_2(c, stack_block, 1)
#define STACK_BLOCK_PROC_2(c, stack_block)	\
  s48_unsafe_record_ref_2(c, stack_block, 2)
#define STACK_BLOCK_THREAD_2(c, stack_block)	\
  s48_unsafe_record_ref_2(c, stack_block, 3)
#define STACK_BLOCK_NEXT_2(c, stack_block)	\
  s48_unsafe_record_ref_2(c, stack_block, 4)

#define s48_push_2(c, x) s48_push(s48_deref(x))

#ifdef DEBUG_FFI
/*
 * For debugging.
 */

static int callback_depth()
{
  int depth = 0;
  s48_value stack = s48_deref(current_stack_block);

  for(; stack != S48_FALSE; depth++, stack = STACK_BLOCK_NEXT(stack));

  return depth;
}
#endif

/*
 * The value being returned from an external call.  The returns may be preceded
 * by a longjmp(), so we stash the value here.
 */
static s48_value external_return_value;

/* Exports to Scheme */
static s48_value	s48_clear_stack_top(void);
static s48_ref_t	s48_system_2(s48_call_t call, s48_ref_t string);

/* Imports from Scheme */
static s48_ref_t 	the_record_type_binding       = NULL;
static s48_ref_t 	stack_block_type_binding      = NULL;
static s48_ref_t 	callback_binding              = NULL;
static s48_ref_t 	delay_callback_return_binding = NULL;

#ifdef DEBUG_FFI
static s48_value	s48_trampoline(s48_value proc, s48_value nargs);
#endif
static s48_ref_t        s48_trampoline_2(s48_call_t call, s48_ref_t proc, s48_ref_t nargs);

void
s48_initialize_external()
{
  the_record_type_binding = 
    s48_get_imported_binding_2("s48-the-record-type");

  stack_block_type_binding = 
    s48_get_imported_binding_2("s48-stack-block-type");

  callback_binding = 
    s48_get_imported_binding_2("s48-callback");

  delay_callback_return_binding =
    s48_get_imported_binding_2("s48-delay-callback-return");

  current_stack_block = s48_make_global_ref(_s48_value_false);
  current_procedure = s48_make_global_ref(_s48_value_false);

  S48_EXPORT_FUNCTION(s48_clear_stack_top);
  S48_EXPORT_FUNCTION(s48_system_2);

#ifdef DEBUG_FFI
  S48_EXPORT_FUNCTION(s48_trampoline);
#endif
  S48_EXPORT_FUNCTION(s48_trampoline_2);

#ifdef DEBUG_FFI
  init_debug_ffi ();
#endif
}

/* The three reasons for an extern-call longjump. */

#define NO_THROW        0
#define EXCEPTION_THROW 1
#define CLEANUP_THROW   2

/*
 * Used to call `proc' from Scheme code. `nargs' the number of arguments in
 * vector `argv'.  If `spread_p' is true the procedure is applied to the
 * arguments, otherwise `proc' is just called on `nargs' and `argv'.
 *
 * We do a setjmp() to get a return point for clearing off this portion of
 * the process stack.  This is used when `proc' calls back to Scheme and
 * then a throw transfers control up past the call to `proc'.
 */

typedef s48_value (*proc_0_t)(void);
typedef s48_value (*proc_1_t)(s48_value);
typedef s48_value (*proc_2_t)(s48_value, s48_value);
typedef s48_value (*proc_3_t)(s48_value, s48_value, s48_value);
typedef s48_value (*proc_4_t)(s48_value, s48_value, s48_value, s48_value);
typedef s48_value (*proc_5_t)(s48_value, s48_value, s48_value, s48_value,
			      s48_value);
typedef s48_value (*proc_6_t)(s48_value, s48_value, s48_value, s48_value,
			      s48_value, s48_value);
typedef s48_value (*proc_7_t)(s48_value, s48_value, s48_value, s48_value,
			      s48_value, s48_value, s48_value);
typedef s48_value (*proc_8_t)(s48_value, s48_value, s48_value, s48_value,
			      s48_value, s48_value, s48_value, s48_value);
typedef s48_value (*proc_9_t)(s48_value, s48_value, s48_value, s48_value,
			      s48_value, s48_value, s48_value, s48_value,
			      s48_value);
typedef s48_value (*proc_10_t)(s48_value, s48_value, s48_value, s48_value,
			       s48_value, s48_value, s48_value, s48_value,
			       s48_value, s48_value);
typedef s48_value (*proc_11_t)(s48_value, s48_value, s48_value, s48_value,
			       s48_value, s48_value, s48_value, s48_value,
			       s48_value, s48_value, s48_value);
typedef s48_value (*proc_12_t)(s48_value, s48_value, s48_value, s48_value,
			       s48_value, s48_value, s48_value, s48_value,
			       s48_value, s48_value, s48_value, s48_value);
typedef s48_value (*proc_n_t)(int, s48_value []);

s48_value
s48_external_call(s48_value sch_proc, s48_value proc_name,
		  long nargs, char *char_argv)
{
  volatile char *gc_roots_marker;	/* volatile to survive longjumps */
  volatile s48_value name = proc_name;	/* volatile to survive longjumps */
  
#ifdef DEBUG_FFI
  int depth; 	/* debugging */
#endif

  long *argv = (long *) char_argv;

  proc_0_t proc = S48_EXTRACT_VALUE(sch_proc, proc_0_t);

  int throw_reason;

  s48_setref(current_procedure, name);

  S48_CHECK_VALUE(sch_proc);
  S48_CHECK_STRING(name);

  gc_roots_marker = s48_set_gc_roots_baseB();

#ifdef DEBUG_FFI
  depth = callback_depth();
  fprintf(stderr, "[external_call at depth %d]\n", depth);
#endif

  throw_reason = setjmp(current_return_point.buf);

  if (throw_reason == NO_THROW) {	/* initial entry */
    switch (nargs) {
    case 0:
      external_return_value = proc();
      break;
    case 1:
      external_return_value = ((proc_1_t)proc)(argv[0]);
      break;
    case 2:
      external_return_value = ((proc_2_t)proc)(argv[1], argv[0]);
      break;
    case 3:
      external_return_value = ((proc_3_t)proc)(argv[2], argv[1], argv[0]);
      break;
    case 4:
      external_return_value = ((proc_4_t)proc)(argv[3], argv[2], argv[1], argv[0]);
      break;
    case 5:
      external_return_value = ((proc_5_t)proc)(argv[4],
					       argv[3], argv[2], argv[1], argv[0]);
      break;
    case 6:
      external_return_value = ((proc_6_t)proc)(argv[5], argv[4],
					       argv[3], argv[2], argv[1], argv[0]);
      break;
    case 7:
      external_return_value = ((proc_7_t)proc)(argv[6], argv[5], argv[4],
					       argv[3], argv[2], argv[1], argv[0]);
      break;
    case 8:
      external_return_value = ((proc_8_t)proc)(argv[7], argv[6], argv[5], argv[4],
					       argv[3], argv[2], argv[1], argv[0]);
      break;
    case 9:
      external_return_value = ((proc_9_t)proc)(argv[8],
					       argv[7], argv[6], argv[5], argv[4],
					       argv[3], argv[2], argv[1], argv[0]);
      break;
    case 10:
      external_return_value = ((proc_10_t)proc)(argv[9], argv[8],
						argv[7], argv[6], argv[5], argv[4],
						argv[3], argv[2], argv[1], argv[0]);
      break;
    case 11:
      external_return_value = ((proc_11_t)proc)(argv[10], argv[9], argv[8],
						argv[7], argv[6], argv[5], argv[4],
						argv[3], argv[2], argv[1], argv[0]);
      break;
    case 12:
      external_return_value = ((proc_12_t)proc)(argv[11], argv[10], argv[9], argv[8],
						argv[7], argv[6], argv[5], argv[4],
						argv[3], argv[2], argv[1], argv[0]);
      break;
    default:
      external_return_value = ((proc_n_t)proc)((int)nargs, (s48_value *)argv);
    }

    /* Raise an exception if the user neglected to pop off some gc roots. */
    
    if (! s48_release_gc_roots_baseB((char *)gc_roots_marker)) {
      s48_raise_scheme_exception(S48_EXCEPTION_GC_PROTECTION_MISMATCH, 0);
    }
    
    /* Clear any free stack-blocks off of the top of the stack-block stack and
       then longjmp past the corresponding portions of the process stack. */
    
    if (s48_deref(current_stack_block) != S48_FALSE &&
	STACK_BLOCK_FREE(s48_deref(current_stack_block)) == S48_TRUE) {

      s48_value bottom_free_block;
      
      do {
	bottom_free_block = s48_deref(current_stack_block);
	s48_setref(current_stack_block, STACK_BLOCK_NEXT(s48_deref(current_stack_block)));
      }
      while (s48_deref(current_stack_block) != S48_FALSE &&
	     STACK_BLOCK_FREE(s48_deref(current_stack_block)) == S48_TRUE);
      
#ifdef DEBUG_FFI
      fprintf(stderr, "[Freeing stack blocks from %d to %d]\n",
	      depth,
	      callback_depth());
#endif

      longjmp(S48_EXTRACT_VALUE_POINTER(STACK_BLOCK_UNWIND(bottom_free_block),
					struct s_jmp_buf)->buf,
	      CLEANUP_THROW);
    }
  }
  else {	/* throwing an exception or unwinding the stack */
#ifdef DEBUG_FFI
    fprintf(stderr, "[external_call throw; was %d and now %d]\n",
	    depth,
	    callback_depth());
    fprintf(stderr, "[throw unrolling to %ld]\n", gc_roots_marker);
#endif
    s48_release_gc_roots_baseB((char *)gc_roots_marker);
  }

  /* Check to see if a thread is waiting to return to the next block down. */
     
  if (s48_deref(current_stack_block) != S48_FALSE &&
      STACK_BLOCK_THREAD(s48_deref(current_stack_block)) != S48_FALSE) {
#ifdef DEBUG_FFI
    fprintf(stderr, "[releasing return at %d]\n", callback_depth());
#endif

    if (throw_reason == EXCEPTION_THROW) {
      /* We are in the midst of raising an exception, so we need to piggyback
	 our exception on that one. */
      s48_value old_exception
	= s48_resetup_external_exception(S48_EXCEPTION_CALLBACK_RETURN_UNCOVERED,
					 2);
      s48_push(old_exception);
      s48_push(s48_deref(current_stack_block));
      external_return_value = S48_UNSPECIFIC;
    }
    else {
      s48_setup_external_exception(S48_EXCEPTION_CALLBACK_RETURN_UNCOVERED, 2);
      s48_push(s48_deref(current_stack_block));
      s48_push(external_return_value);
      external_return_value = S48_UNSPECIFIC;
    }
  }

  return external_return_value;
}

/*
 * The value being returned from an external call.  The returns may be preceded
 * by a longjmp(), so we stash the value here.
 */
static s48_ref_t cexternal_return_value;

typedef s48_ref_t (*cproc_0_t)(s48_call_t);
typedef s48_ref_t (*cproc_1_t)(s48_call_t,
			       s48_ref_t);
typedef s48_ref_t (*cproc_2_t)(s48_call_t,
			       s48_ref_t, s48_ref_t);
typedef s48_ref_t (*cproc_3_t)(s48_call_t,
			       s48_ref_t, s48_ref_t, s48_ref_t);
typedef s48_ref_t (*cproc_4_t)(s48_call_t,
			       s48_ref_t, s48_ref_t, s48_ref_t, s48_ref_t);
typedef s48_ref_t (*cproc_5_t)(s48_call_t,
			       s48_ref_t, s48_ref_t, s48_ref_t, s48_ref_t,
			       s48_ref_t);
typedef s48_ref_t (*cproc_6_t)(s48_call_t,
			       s48_ref_t, s48_ref_t, s48_ref_t, s48_ref_t,
			       s48_ref_t, s48_ref_t);
typedef s48_ref_t (*cproc_7_t)(s48_call_t,
			       s48_ref_t, s48_ref_t, s48_ref_t, s48_ref_t,
			       s48_ref_t, s48_ref_t, s48_ref_t);
typedef s48_ref_t (*cproc_8_t)(s48_call_t,
			       s48_ref_t, s48_ref_t, s48_ref_t, s48_ref_t,
			       s48_ref_t, s48_ref_t, s48_ref_t, s48_ref_t);
typedef s48_ref_t (*cproc_9_t)(s48_call_t,
			       s48_ref_t, s48_ref_t, s48_ref_t, s48_ref_t,
			       s48_ref_t, s48_ref_t, s48_ref_t, s48_ref_t,
			       s48_ref_t);
typedef s48_ref_t (*cproc_10_t)(s48_call_t,
				s48_ref_t, s48_ref_t, s48_ref_t, s48_ref_t,
				s48_ref_t, s48_ref_t, s48_ref_t, s48_ref_t,
				s48_ref_t, s48_ref_t);
typedef s48_ref_t (*cproc_11_t)(s48_call_t,
				s48_ref_t, s48_ref_t, s48_ref_t, s48_ref_t,
				s48_ref_t, s48_ref_t, s48_ref_t, s48_ref_t,
				s48_ref_t, s48_ref_t, s48_ref_t);
typedef s48_ref_t (*cproc_12_t)(s48_call_t,
				s48_ref_t, s48_ref_t, s48_ref_t, s48_ref_t,
				s48_ref_t, s48_ref_t, s48_ref_t, s48_ref_t,
				s48_ref_t, s48_ref_t, s48_ref_t, s48_ref_t);
typedef s48_ref_t (*cproc_n_t)(s48_call_t, int, s48_ref_t  []);

s48_value
s48_external_ecall(s48_call_t call,
		   s48_value sch_proc, s48_value proc_name,
		   long nargs, char *char_argv)
{
  volatile char *gc_roots_marker;	/* volatile to survive longjumps */
  volatile s48_value name = proc_name;	/* volatile to survive longjumps */
  s48_call_t new_call;
  s48_ref_t argv_ref[12];
  s48_ref_t sch_proc_ref, proc_name_ref;
  s48_value result;

#ifdef DEBUG_FFI
  int depth = callback_depth(); 	/* debugging */
#endif

  long *argv = (long *) char_argv;

  cproc_0_t cproc = S48_EXTRACT_VALUE(sch_proc, cproc_0_t);

  int throw_reason;

  s48_ref_t sbt = NULL;

  s48_setref(current_procedure, name);

  S48_CHECK_VALUE(sch_proc);
  S48_CHECK_STRING(name);

  gc_roots_marker = s48_set_gc_roots_baseB();

#ifdef DEBUG_FFI
  fprintf(stderr, "[external_call_2 at depth %d]\n", depth);
#endif

  throw_reason = setjmp(current_return_point.buf);

  if (throw_reason == NO_THROW) {	/* initial entry */
    long i;
    new_call = s48_push_call (call);
    for (i = 0; i < nargs; i++) 
      argv_ref[i] = s48_make_local_ref (new_call, argv[i]);
    sch_proc_ref = s48_make_local_ref (new_call, sch_proc);
    proc_name_ref = s48_make_local_ref (new_call, proc_name);

    switch (nargs) {
    case 0:
      cexternal_return_value = ((cproc_0_t)cproc)(new_call);
      break;
    case 1:
      cexternal_return_value = ((cproc_1_t)cproc)(new_call, argv_ref[0]);
      break;
    case 2:
      cexternal_return_value = ((cproc_2_t)cproc)(new_call, argv_ref[1], argv_ref[0]);
      break;
    case 3:
      cexternal_return_value = ((cproc_3_t)cproc)(new_call, argv_ref[2], argv_ref[1], argv_ref[0]);
      break;
    case 4:
      cexternal_return_value = ((cproc_4_t)cproc)(new_call,
						  argv_ref[3], argv_ref[2], argv_ref[1], argv_ref[0]);
      break;
    case 5:
      cexternal_return_value = ((cproc_5_t)cproc)(new_call, argv_ref[4],
						  argv_ref[3], argv_ref[2], argv_ref[1], argv_ref[0]);
      break;
    case 6:
      cexternal_return_value = ((cproc_6_t)cproc)(new_call, argv_ref[5], argv_ref[4],
						  argv_ref[3], argv_ref[2], argv_ref[1], argv_ref[0]);
      break;
    case 7:
      cexternal_return_value = ((cproc_7_t)cproc)(new_call, argv_ref[6], argv_ref[5], argv_ref[4],
						  argv_ref[3], argv_ref[2], argv_ref[1], argv_ref[0]);
      break;
    case 8:
      cexternal_return_value = ((cproc_8_t)cproc)(new_call, 
						  argv_ref[7], argv_ref[6], argv_ref[5], argv_ref[4],
						  argv_ref[3], argv_ref[2], argv_ref[1], argv_ref[0]);
      break;
    case 9:
      cexternal_return_value = ((cproc_9_t)cproc)(new_call, argv_ref[8],
						  argv_ref[7], argv_ref[6], argv_ref[5], argv_ref[4],
						  argv_ref[3], argv_ref[2], argv_ref[1], argv_ref[0]);
      break;
    case 10:
      cexternal_return_value = ((cproc_10_t)cproc)(new_call, argv_ref[9], argv_ref[8],
						   argv_ref[7], argv_ref[6], argv_ref[5], argv_ref[4],
						   argv_ref[3], argv_ref[2], argv_ref[1], argv_ref[0]);
      break;
    case 11:
      cexternal_return_value = ((cproc_11_t)cproc)(new_call, argv_ref[10], argv_ref[9], argv_ref[8],
						   argv_ref[7], argv_ref[6], argv_ref[5], argv_ref[4],
						   argv_ref[3], argv_ref[2], argv_ref[1], argv_ref[0]);
      break;
    case 12:
      cexternal_return_value = ((cproc_12_t)cproc)(new_call,
						   argv_ref[11], argv_ref[10], argv_ref[9], argv_ref[8],
						   argv_ref[7], argv_ref[6], argv_ref[5], argv_ref[4],
						   argv_ref[3], argv_ref[2], argv_ref[1], argv_ref[0]);
      break;
    default:
      cexternal_return_value = ((cproc_n_t)cproc)(new_call, (int) nargs, argv_ref);
    }

    /* Raise an exception if the user neglected to pop off some gc roots. */
    
    if (! s48_release_gc_roots_baseB((char *)gc_roots_marker)) {
      s48_raise_scheme_exception(S48_EXCEPTION_GC_PROTECTION_MISMATCH, 0);
    }
    
    /* Clear any free stack-blocks off of the top of the stack-block stack and
       then longjmp past the corresponding portions of the process stack. */
    
    if (!s48_false_p_2(new_call, current_stack_block) &&
	s48_true_p_2(new_call, STACK_BLOCK_FREE_2(new_call, current_stack_block))) {

      s48_ref_t bottom_free_block;
      
      do {
	s48_setref(bottom_free_block, s48_deref(current_stack_block));
	s48_setref(current_stack_block, s48_deref(STACK_BLOCK_NEXT_2(new_call, current_stack_block)));
      }
      while (!s48_false_p_2(new_call, current_stack_block) &&
	     s48_false_p_2(new_call, STACK_BLOCK_FREE_2(new_call, current_stack_block)));
      
#ifdef DEBUG_FFI
      fprintf(stderr, "[Freeing stack blocks from %d to %d]\n",
	      depth,
	      callback_depth());
#endif

      longjmp(s48_extract_value_pointer_2(new_call, 
					  STACK_BLOCK_UNWIND_2(new_call, bottom_free_block),
					  struct s_jmp_buf)->buf,
	      CLEANUP_THROW);
    }
  }
  else {	/* throwing an exception or unwinding the stack */
#ifdef DEBUG_FFI
    fprintf(stderr, "[external_call_2 throw; was %d and now %d]\n",
	    depth,
	    callback_depth());
    fprintf(stderr, "[throw unrolling to %ld]\n", gc_roots_marker);
#endif
    s48_release_gc_roots_baseB((char *)gc_roots_marker);
  }

  /* otherwise the pop_to will kill us */
  if (cexternal_return_value)
	cexternal_return_value = s48_copy_local_ref(call, cexternal_return_value);

  s48_pop_to (call);

  if (cexternal_return_value)
    result = s48_deref(cexternal_return_value);
  else
    result = S48_UNSPECIFIC;

  /* Check to see if a thread is waiting to return to the next block down. */
  if (!s48_false_p_2(call, current_stack_block) &&
      !s48_false_p_2(call, sbt = STACK_BLOCK_THREAD_2(call, current_stack_block))) {
#ifdef DEBUG_FFI
    fprintf(stderr, "[releasing return at %d]\n", callback_depth());
#endif

    if (throw_reason == EXCEPTION_THROW) {
      /* We are in the midst of raising an exception, so we need to piggyback
	 our exception on that one. */
      s48_value old_exception
	= s48_resetup_external_exception(S48_EXCEPTION_CALLBACK_RETURN_UNCOVERED,
					 2);
      s48_push(old_exception);
      s48_push_2(call, current_stack_block);

      if (cexternal_return_value)
	s48_free_local_ref(call, cexternal_return_value);

      result = S48_UNSPECIFIC;
    } else {
      if (cexternal_return_value) {
      s48_setup_external_exception(S48_EXCEPTION_CALLBACK_RETURN_UNCOVERED, 2);
      s48_push_2(call, current_stack_block);
      s48_push_2(call, cexternal_return_value);
      } else {
	s48_setup_external_exception(S48_EXCEPTION_CALLBACK_RETURN_UNCOVERED, 1);
	s48_push_2(call, current_stack_block);
      }
      result = S48_UNSPECIFIC;
    }
  } else {
    if (cexternal_return_value)
      s48_free_local_ref(call, cexternal_return_value);
  }

  if(sbt != NULL)
      s48_free_local_ref(call, sbt);

  return result;
}

s48_value
s48_external_call_2(s48_value sch_proc, s48_value proc_name,
		    long nargs, char *char_argv)
{
  return s48_external_ecall (s48_get_current_call(), sch_proc,
			     proc_name, nargs, char_argv);
}

/*
 * Call Scheme function `proc' from C.  We push the call-back depth, `proc',
 * and the arguments on the Scheme stack and then restart the VM.  The restarted
 * VM calls the Scheme procedure `callback' which wraps the call to `proc' with
 * a dynamic-wind.  This prevents downward throws back into the call to `proc',
 * which C can't handle, and allows the C stack to be cleaned up if an upward
 * throw occurs.
 *
 * The maximum number of arguments is determined by the amount of space reserved
 * on the Scheme stack for exceptions. See the definition of stack-slack in
 * scheme/vm/stack.scm.
 */
s48_value
s48_call_scheme(s48_value proc, long nargs, ...)
{
  int i;
  va_list arguments;
  s48_value value;
  s48_value unwind, stack_block;
  S48_DECLARE_GC_PROTECT(2);

  S48_GC_PROTECT_2(unwind, proc);
  
  va_start(arguments, nargs);

  S48_SHARED_BINDING_CHECK(s48_deref(callback_binding));

  /* It would be nice to push a list of the arguments, but we have no way
     of preserving them across a cons. */
  if (nargs < 0 || 12 < nargs) {  /* DO NOT INCREASE THIS NUMBER */
    s48_value sch_nargs = s48_enter_integer(nargs);  /* `proc' is protected */
    s48_raise_scheme_exception(S48_EXCEPTION_TOO_MANY_ARGUMENTS_IN_CALLBACK,
			       2, proc, sch_nargs);
  }

#ifdef DEBUG_FFI
  fprintf(stderr, "[s48_call_scheme, %ld args, depth %d]\n",
	  nargs, callback_depth());
#endif

  s48_push(S48_UNSPECIFIC);	/* placeholder */
  s48_push(proc);
  for (i = 0; i < nargs; i++)
    s48_push(va_arg(arguments, s48_value));

  va_end(arguments);

  /* With everything safely on the stack we can do the necessary allocation. */

  unwind = S48_MAKE_VALUE(struct s_jmp_buf);
  S48_EXTRACT_VALUE(unwind, struct s_jmp_buf) = current_return_point;

  stack_block = s48_make_record(s48_deref(stack_block_type_binding));
  STACK_BLOCK_UNWIND(stack_block) = unwind;
  STACK_BLOCK_PROC(stack_block) = s48_deref(current_procedure);
  STACK_BLOCK_NEXT(stack_block) = s48_deref(current_stack_block);
  STACK_BLOCK_FREE(stack_block) = S48_FALSE;
  STACK_BLOCK_THREAD(stack_block) = S48_FALSE;

  S48_GC_UNPROTECT();		/* no more references to `unwind' or `proc'. */

  s48_setref(current_stack_block, stack_block);

#ifdef DEBUG_FFI
  if(s48_stack_ref(nargs + 1) != S48_UNSPECIFIC)
    fprintf(stderr, "[stack_block set missed]\n");
#endif

  s48_stack_setB(nargs + 1, stack_block);

#ifdef DEBUG_FFI
  fprintf(stderr, "[s48_call_scheme, %ld args, depth %d, off we go]\n",
	  nargs, callback_depth());
#endif

  value = s48_restart(S48_UNSAFE_SHARED_BINDING_REF(s48_deref(callback_binding)),
		      nargs + 2);

  for (;s48_Scallback_return_stack_blockS != s48_deref(current_stack_block);) {
    if (s48_Scallback_return_stack_blockS == S48_FALSE) {

#ifdef DEBUG_FFI
      fprintf(stderr, "[s48_call_scheme returning from VM %ld]\n", callback_depth());
#endif

      exit(value);
    }
    else {

      /* Someone has returned (because of threads) to the wrong section of the
	 C stack.  We call back to a Scheme procedure that will suspend until
	 our block is at the top of the stack. */

      s48_push(s48_Scallback_return_stack_blockS);
      s48_push(S48_UNSAFE_SHARED_BINDING_REF(s48_deref(delay_callback_return_binding)));
      s48_push(s48_Scallback_return_stack_blockS);
      s48_push(value);

#ifdef DEBUG_FFI
      fprintf(stderr, "[Premature return, %ld args, depth %d, back we go]\n",
	      nargs, callback_depth());
#endif

      s48_disable_interruptsB();
      value = s48_restart(S48_UNSAFE_SHARED_BINDING_REF(s48_deref(callback_binding)), 4);
    }
  }

  /* Restore the state of the current stack block. */

  unwind = STACK_BLOCK_UNWIND(s48_deref(current_stack_block));
  current_return_point = S48_EXTRACT_VALUE(unwind, struct s_jmp_buf);
  s48_setref(current_procedure, STACK_BLOCK_PROC(s48_deref(current_stack_block)));
  s48_setref(current_stack_block, STACK_BLOCK_NEXT(s48_deref(current_stack_block)));

#ifdef DEBUG_FFI
  fprintf(stderr, "[s48_call_scheme returns from depth %d]\n", callback_depth());
#endif

  return value;
}

s48_ref_t 
s48_call_scheme_2(s48_call_t call, s48_ref_t proc, long nargs, ...)
{
  int i;
  va_list arguments;
  s48_value value;
  s48_ref_t unwind;
  s48_value stack_block;

  va_start(arguments, nargs);

#ifdef DEBUG_FFI
  fprintf(stderr, "[s48_call_scheme_2, %ld args, depth %d]\n",
	  nargs, callback_depth());
#endif
  
  s48_copy_local_bvs_to_scheme (call);

  s48_shared_binding_check_2(call, callback_binding);

  /* It would be nice to push a list of the arguments, but we have no way
     of preserving them across a cons. */
  if (nargs < 0 || 12 < nargs) {  /* DO NOT INCREASE THIS NUMBER */
    s48_value sch_nargs = s48_enter_integer(nargs);  /* `proc' is protected */
    s48_raise_scheme_exception(S48_EXCEPTION_TOO_MANY_ARGUMENTS_IN_CALLBACK,
			       2, s48_deref(proc), sch_nargs);
  }

#ifdef DEBUG_FFI
  fprintf(stderr, "[s48_call_scheme_2, %ld args, depth %d]\n",
	  nargs, callback_depth());
#endif

  s48_push(S48_UNSPECIFIC);	/* placeholder */
  s48_push(s48_deref(proc));
  for (i = 0; i < nargs; i++) {
    s48_ref_t ref = va_arg(arguments, s48_ref_t);
#ifdef DEBUG_FFI
    fprintf(stderr, "call_scheme_2: pushing arg %d ref %x\n", i, ref);
#endif
    s48_push(s48_deref(ref));
  }

  va_end(arguments);

  /* With everything safely on the stack we can do the necessary allocation. */

  unwind = s48_make_value_2(call, struct s_jmp_buf);
  s48_extract_value_2(call, unwind, struct s_jmp_buf) = current_return_point;

  stack_block = s48_make_record(s48_deref(stack_block_type_binding));
  STACK_BLOCK_UNWIND(stack_block) = s48_deref(unwind);
  STACK_BLOCK_PROC(stack_block) = s48_deref(current_procedure);
  STACK_BLOCK_NEXT(stack_block) = s48_deref(current_stack_block);
  STACK_BLOCK_FREE(stack_block) = S48_FALSE;
  STACK_BLOCK_THREAD(stack_block) = S48_FALSE;

  s48_setref(current_stack_block, stack_block);

#ifdef DEBUG_FFI
  if(s48_stack_ref(nargs + 1) != S48_UNSPECIFIC)
    fprintf(stderr, "[stack_block set missed]\n");
#endif

  s48_stack_setB(nargs + 1, stack_block);

#ifdef DEBUG_FFI
  fprintf(stderr, "[s48_call_scheme_2, %ld args, depth %d, off we go]\n",
	  nargs, callback_depth());
#endif

  value = s48_restart(s48_deref(s48_unsafe_shared_binding_ref_2(call, callback_binding)),
		      nargs + 2);

  for (;s48_Scallback_return_stack_blockS != s48_deref(current_stack_block);) {
    if (s48_Scallback_return_stack_blockS == S48_FALSE) {

#ifdef DEBUG_FFI
      fprintf(stderr, "[s48_call_scheme_2 returning from VM %ld]\n", callback_depth());
#endif

      exit(value);
    }
    else {

      /* Someone has returned (because of threads) to the wrong section of the
	 C stack.  We call back to a Scheme procedure that will suspend until
	 our block is at the top of the stack. */

      s48_push(s48_Scallback_return_stack_blockS);
      s48_push_2(call, s48_unsafe_shared_binding_ref_2(call, delay_callback_return_binding));
      s48_push(s48_Scallback_return_stack_blockS);
      s48_push(value);

#ifdef DEBUG_FFI
      fprintf(stderr, "[Premature return, %ld args, depth %d, back we go]\n",
	      nargs, callback_depth());
#endif

      s48_disable_interruptsB();
      value = s48_restart(s48_deref(s48_unsafe_shared_binding_ref_2(call, callback_binding)), 4);
    }
  }

  /* Restore the state of the current stack block. */

  unwind = STACK_BLOCK_UNWIND_2(call, current_stack_block);
  current_return_point = s48_extract_value_2(call, unwind, struct s_jmp_buf);
  s48_setref(current_procedure, s48_deref(STACK_BLOCK_PROC_2(call, current_stack_block)));
  s48_setref(current_stack_block, s48_deref(STACK_BLOCK_NEXT_2(call, current_stack_block)));

#ifdef DEBUG_FFI
  fprintf(stderr, "[s48_call_scheme_2 returns from depth %d]\n", callback_depth());
#endif

  s48_copy_local_bvs_from_scheme (call);

  return s48_make_local_ref (call, value);
}

/*
 * Because the top of the stack is cleared on the return from every external
 * call, this doesn't have to do anything but exist.
 */
static s48_value
s48_clear_stack_top()
{
#ifdef DEBUG_FFI
  fprintf(stderr, "[Clearing stack top]\n");
#endif
  return S48_UNSPECIFIC;
}

#ifdef DEBUG_FFI
/*
 * For testing callbacks.  This just calls its argument on the specified number
 * of values.
 */
static s48_value
s48_trampoline(s48_value proc, s48_value nargs)
{

  fprintf(stderr, "[C trampoline, %ld args]\n", S48_UNSAFE_EXTRACT_FIXNUM(nargs));

  switch (s48_extract_fixnum(nargs)) {
  case -2: { /* provoke exception: GC protection mismatch */
    S48_DECLARE_GC_PROTECT(1);
    
    S48_GC_PROTECT_1(proc);

    return S48_FALSE;
  }
  case -1: { /* this is broken, dunno what this should do, anyway --Marcus */
    long n = - s48_extract_integer(proc);
    fprintf(stderr, "[extract magnitude is %ld (%lx)]\n", n, n);
    return s48_enter_integer(n);
  }
  case 0: {
    s48_value value = s48_call_scheme(proc, 0);
    if (value == S48_FALSE)
      s48_assertion_violation("s48_trampoline", "trampoline bouncing", 0);
    return value;
  }
  case 1:
    return s48_call_scheme(proc, 1, s48_enter_fixnum(100));
  case 2:
    return s48_call_scheme(proc, 2, s48_enter_fixnum(100), s48_enter_fixnum(200));
  case 3:
    return s48_call_scheme(proc, 3, s48_enter_fixnum(100), s48_enter_fixnum(200),
		    s48_enter_fixnum(300));
  default:
    s48_assertion_violation("s48_trampoline", "invalid number of arguments", 1, nargs);
    return S48_UNDEFINED; /* not that we ever get here */
  }
}
#endif

static s48_ref_t 
s48_trampoline_2(s48_call_t call, s48_ref_t proc, s48_ref_t nargs)
{
#ifdef DEBUG_FFI
  fprintf(stderr, "[C trampoline_2, %ld args]\n", s48_unsafe_extract_long_2(call, nargs));
#endif
  switch (s48_extract_long_2(call, nargs)) {
  case -2: { /* provoke exception: GC protection mismatch */
    S48_DECLARE_GC_PROTECT(1);
    
    S48_GC_PROTECT_1(proc);

    return s48_false_2(call);
  }
  case 0: {
    s48_ref_t result = s48_call_scheme_2(call, proc, 0);
    if (s48_false_p_2(call, result))
      s48_assertion_violation_2(call, "s48_trampoline_2", "trampoline bouncing", 0);
    return result;
  }
  case 1:
    return s48_call_scheme_2(call, proc, 1, 
			     s48_make_local_ref (call, s48_enter_fixnum(100)));
  case 2:
    return s48_call_scheme_2(call, proc, 2, 
			     s48_make_local_ref (call, s48_enter_fixnum(100)),
			     s48_make_local_ref (call, s48_enter_fixnum(200)));
  case 3:
    return s48_call_scheme_2(call, proc, 3, 
			     s48_make_local_ref (call, s48_enter_fixnum(100)),
			     s48_make_local_ref (call, s48_enter_fixnum(200)),
			     s48_make_local_ref (call, s48_enter_fixnum(300)));
  default:
    s48_assertion_violation_2(call, "s48_trampoline_2", "invalid number of arguments", 1, nargs);
    return s48_undefined_2(call); /* not that we ever get here */
  }
}

static s48_ref_t
s48_system_2(s48_call_t call, s48_ref_t string)
{
  return s48_enter_long_2(call, 
			  system(s48_false_p_2(call, string)
				 ? NULL
				 : s48_extract_byte_vector_readonly_2(call, string)));
}

/********************************/
/*
 * Raising exceptions.  We push the arguments on the stack end then throw out
 * of the most recent call from Scheme.
 *
 * The maximum number of arguments is determined by the amount of space reserved
 * on the Scheme stack for exceptions. See the definition of stack-slack in
 * scheme/vm/interp/stack.scm.
 */

static long
raise_scheme_exception_prelude(long why, long nargs)
{
  s48_setup_external_exception(why, nargs);
  
  if (11 < nargs) {   /* DO NOT INCREASE THIS NUMBER */
    fprintf(stderr, "too many arguments to external exception, discarding surplus\n");
    nargs = 11;
  }
  return nargs;
}

static long
raise_scheme_exception_prelude_2(s48_call_t call, long why, long nargs)
{
  s48_copy_local_bvs_to_scheme(call);
  return raise_scheme_exception_prelude(why, nargs);
}

static void
raise_scheme_exception_postlude(void)
{
   external_return_value = S48_UNSPECIFIC;
   longjmp(current_return_point.buf, EXCEPTION_THROW);
}

void
s48_raise_scheme_exception(long why, long nargs, ...)
{
  int i;
  va_list irritants;

  nargs = raise_scheme_exception_prelude(why, nargs + 1) - 1;

  s48_push(s48_deref(current_procedure));

  va_start(irritants, nargs);

  for (i = 0; i < nargs; i++)
    s48_push(va_arg(irritants, s48_value));

  va_end(irritants);

  raise_scheme_exception_postlude();
}

void
s48_raise_scheme_exception_2(s48_call_t call, long why, long nargs, ...)
{
  int i;
  va_list irritants;

  nargs = raise_scheme_exception_prelude_2(call, why, nargs + 1) - 1;

  s48_push_2(call, current_procedure);

  va_start(irritants, nargs);

  for (i = 0; i < nargs; i++)
    s48_push_2(call, va_arg(irritants, s48_ref_t));

  va_end(irritants);

  raise_scheme_exception_postlude();
}

static void
raise_scheme_standard_exception(long why, const char* who, const char* message,
				long irritant_count, va_list irritants)
{
  int i;
  long nargs = irritant_count + 2; /* who and message */

  nargs = raise_scheme_exception_prelude(why, nargs);
  irritant_count = nargs - 2;
  
  for (i = 0; i < irritant_count; i++)
    s48_push(va_arg(irritants, s48_value));

  va_end(irritants);

  /* these must be last because of GC protection */
  if (who == NULL)
    s48_push(s48_deref(current_procedure));
  else
    s48_push(s48_enter_string_utf_8((char*)who));
  s48_push(s48_enter_byte_string((char*)message));

  raise_scheme_exception_postlude();
}

static void
raise_scheme_standard_exception_2(s48_call_t call, long why, const char* who, const char* message,
				  long irritant_count, va_list irritants)
{
  int i;
  long nargs = irritant_count + 2; /* who and message */

  nargs = raise_scheme_exception_prelude_2(call, why, nargs);
  irritant_count = nargs - 2;
  
  for (i = 0; i < irritant_count; i++)
    s48_push_2(call, va_arg(irritants, s48_ref_t));

  va_end(irritants);

  /* these must be last because of GC protection */
  if (who == NULL)
    s48_push_2(call, current_procedure);
  else
    s48_push_2(call, s48_enter_string_utf_8_2(call, (char*) who));
  s48_push_2(call, s48_enter_byte_string_2(call, (char*) message));

  raise_scheme_exception_postlude();
}

/* Specific exceptions */

void
s48_error(const char* who, const char* message,
	  long irritant_count, ...)
{
  va_list irritants;
  va_start(irritants, irritant_count);
  raise_scheme_standard_exception(S48_EXCEPTION_EXTERNAL_ERROR,
				  who, message, irritant_count, irritants);
}

void
s48_error_2(s48_call_t call, const char* who, const char* message,
	    long irritant_count, ...)
{
  va_list irritants;
  va_start(irritants, irritant_count);
  raise_scheme_standard_exception_2(call, S48_EXCEPTION_EXTERNAL_ERROR,
				    who, message, irritant_count, irritants);
}

void
s48_assertion_violation(const char* who, const char* message,
			long irritant_count, ...)
{
  va_list irritants;
  va_start(irritants, irritant_count);
  raise_scheme_standard_exception(S48_EXCEPTION_EXTERNAL_ASSERTION_VIOLATION,
				  who, message, irritant_count, irritants);
}

void
s48_assertion_violation_2(s48_call_t call, const char* who, const char* message,
			  long irritant_count, ...)
{
  va_list irritants;
  va_start(irritants, irritant_count);
  raise_scheme_standard_exception_2(call, S48_EXCEPTION_EXTERNAL_ASSERTION_VIOLATION,
				    who, message, irritant_count, irritants);
}

void
s48_os_error(const char* who, int the_errno,
	     long irritant_count, ...)
{
  int i;
  long nargs = irritant_count + 2; /* who and errno */
  va_list irritants;

  nargs = raise_scheme_exception_prelude(S48_EXCEPTION_EXTERNAL_OS_ERROR, nargs);
  irritant_count = nargs - 2;
  
  va_start(irritants, irritant_count);

  for (i = 0; i < irritant_count; i++)
    s48_push(va_arg(irritants, s48_value));

  va_end(irritants);

  /* last because of GC protection */
  if (who == NULL)
    s48_push(s48_deref(current_procedure));
  else
    s48_push(s48_enter_string_utf_8((char*)who));
  s48_push(s48_enter_fixnum(the_errno));

  raise_scheme_exception_postlude();
}

void
s48_os_error_2(s48_call_t call, const char* who, int the_errno, 
	       long irritant_count, ...)
{
  int i;
  long nargs = irritant_count + 2; /* who and errno */
  va_list irritants;

  nargs = raise_scheme_exception_prelude_2(call, S48_EXCEPTION_EXTERNAL_OS_ERROR, nargs);
  irritant_count = nargs - 2;
  
  va_start(irritants, irritant_count);

  for (i = 0; i < irritant_count; i++)
    s48_push_2(call, va_arg(irritants, s48_ref_t));

  va_end(irritants);

  /* last because of GC protection */
  if (who == NULL)
    s48_push_2(call, current_procedure);
  else
    s48_push_2(call, s48_enter_string_utf_8_2(call, who));
  s48_push_2(call, s48_enter_long_as_fixnum_2(call, the_errno));

  raise_scheme_exception_postlude();
}

void
s48_out_of_memory_error()
{
  s48_raise_scheme_exception(S48_EXCEPTION_OUT_OF_MEMORY, 0);
}

void
s48_out_of_memory_error_2(s48_call_t call)
{
  s48_raise_scheme_exception_2(call, S48_EXCEPTION_OUT_OF_MEMORY, 0);
}

/* For internal use by the VM: */

void
s48_argument_type_violation(s48_value value) {
  s48_assertion_violation(NULL, "argument-type violation", 1, value);
}

void
s48_argument_type_violation_2(s48_call_t call, s48_ref_t value) {
  s48_assertion_violation_2(call, NULL, "argument-type violation", 1, value);
}

void
s48_range_violation(s48_value value, s48_value min, s48_value max) {
  s48_assertion_violation(NULL, "argument out of range", 3, value, min, max);
}

void
s48_range_violation_2(s48_call_t call, s48_ref_t value, s48_ref_t min, s48_ref_t max) {
  s48_assertion_violation_2(call, NULL, "argument out of range", 3, value, min, max);
}

/* The following are deprecated: */

void
s48_raise_argument_type_error(s48_value value) {
  s48_raise_scheme_exception(S48_EXCEPTION_WRONG_TYPE_ARGUMENT, 1, value);
}

void
s48_raise_argument_number_error(s48_value value, s48_value min, s48_value max) {
  s48_raise_scheme_exception(S48_EXCEPTION_WRONG_NUMBER_OF_ARGUMENTS,
			     3, value, min, max);
}

void
s48_raise_range_error(s48_value value, s48_value min, s48_value max) {
  s48_raise_scheme_exception(S48_EXCEPTION_INDEX_OUT_OF_RANGE,
			     3, value, min, max);
}

void
s48_raise_closed_channel_error() {
  s48_raise_scheme_exception(S48_EXCEPTION_CLOSED_CHANNEL, 0);
}

void
s48_raise_os_error(int the_errno) {
  s48_os_error(NULL, the_errno, 0);
}

void
s48_raise_string_os_error(char *reason) {
  s48_error(NULL, (const char*)s48_enter_string_latin_1(reason), 0);
}

void
s48_raise_out_of_memory_error() {
  s48_raise_scheme_exception(S48_EXCEPTION_OUT_OF_MEMORY, 0);
}


/********************************/
/* Support routines for external code */

/*
 * Type-safe procedures for checking types and dereferencing and setting slots.
 */

int
s48_stob_has_type(s48_value thing, int type)
{
  return S48_STOB_P(thing) && (S48_STOB_TYPE(thing) == type);
}

int
s48_stob_has_type_2(s48_call_t call, s48_ref_t thing, int type)
{
  return s48_stob_p_2(call, thing) && (s48_stob_type_2(call, thing) == type);
}

long
s48_stob_length(s48_value thing, int type)
{
  if (!(S48_STOB_P(thing) && (S48_STOB_TYPE(thing) == type)))
    s48_assertion_violation("s48_stob_length", "not a stob", 1, thing);
  
  return S48_STOB_DESCRIPTOR_LENGTH(thing);
}

long
s48_stob_length_2(s48_call_t call, s48_ref_t thing, int type)
{
  if (!(s48_stob_p_2(call, thing) && (s48_stob_type_2(call, thing) == type)))
    s48_assertion_violation_2(call, "s48_stob_length_2", "not a stob", 1, thing);
  
  return s48_unsafe_stob_descriptor_length_2(call, thing);
}

long
s48_stob_byte_length(s48_value thing, int type)
{
  if (!(S48_STOB_P(thing) && (S48_STOB_TYPE(thing) == type)))
    s48_assertion_violation("s48_stob_byte_length", "not a stob", 1, thing);

  if (type == S48_STOBTYPE_STRING)
    return S48_STOB_BYTE_LENGTH(thing) - 1;
  else
    return S48_STOB_BYTE_LENGTH(thing);
}

long
s48_stob_byte_length_2(s48_call_t call, s48_ref_t thing, int type)
{
  if (!(s48_stob_p_2(call, thing) && (s48_stob_type_2(call, thing) == type)))
    s48_assertion_violation_2(call, "s48_stob_byte_length_2", "not a stob", 1, thing);

  if (type == S48_STOBTYPE_STRING)
    return s48_unsafe_stob_byte_length_2(call, thing) - 1;
  else
    return s48_unsafe_stob_byte_length_2(call, thing);
}


s48_value
s48_stob_ref(s48_value thing, int type, long offset)
{
  long length;

  if (!(S48_STOB_P(thing) && (S48_STOB_TYPE(thing) == type)))
    s48_assertion_violation("s48_stob_ref", "not a stob", 1, thing);

  length = S48_STOB_DESCRIPTOR_LENGTH(thing);

  if (offset < 0 || length <= offset)
    s48_assertion_violation("s48_stob_ref", "invalid stob index", 3,
			    s48_enter_integer(offset),
			    S48_UNSAFE_ENTER_FIXNUM(0),
			    S48_UNSAFE_ENTER_FIXNUM(length - 1));
			  
  return S48_STOB_REF(thing, offset);
}

s48_ref_t
s48_stob_ref_2(s48_call_t call, s48_ref_t thing, int type, long offset)
{
  long length;

  if (!(s48_stob_p_2(call, thing) && (s48_stob_type_2(call, thing) == type)))
    s48_assertion_violation_2(call, "s48_stob_ref_2", "not a stob", 1, thing);

  length = s48_unsafe_stob_descriptor_length_2(call, thing);

  if (offset < 0 || length <= offset)
    s48_assertion_violation_2(call, "s48_stob_ref_2", "invalid stob index", 3,
			      s48_enter_long_2(call, offset),
			      s48_unsafe_enter_long_as_fixnum_2(call, 0),
			      s48_unsafe_enter_long_as_fixnum_2(call, length - 1));
			  
  return s48_unsafe_stob_ref_2(call, thing, offset);
}

void
s48_stob_set(s48_value thing, int type, long offset, s48_value value)
{
  long length;
  
  if (!(S48_STOB_P(thing) &&
	(S48_STOB_TYPE(thing) == type) &&
	!S48_STOB_IMMUTABLEP(thing)))
    s48_assertion_violation("s48_stob_set", "not a mutable stob", 1, thing);
  
  length = S48_STOB_DESCRIPTOR_LENGTH(thing);

  if (offset < 0 || length <= offset)
    s48_assertion_violation("s48_stob_set", "invalid stob index", 3,
			    s48_enter_integer(offset),
			    S48_UNSAFE_ENTER_FIXNUM(0),
			    S48_UNSAFE_ENTER_FIXNUM(length - 1));
			  
  S48_STOB_SET(thing, offset, value);
}

void
s48_stob_set_2(s48_call_t call, s48_ref_t thing, int type, long offset, s48_ref_t value)
{
  long length;
  
  if (!(s48_stob_p_2(call, thing) &&
	(s48_stob_type_2(call, thing) == type) &&
	!s48_stob_immutablep_2(call, thing)))
    s48_assertion_violation_2(call, "s48_stob_set_2",
			      "not a mutable stob", 1, thing);
  
  length = s48_unsafe_stob_descriptor_length_2(call, thing);

  if (offset < 0 || length <= offset)
    s48_assertion_violation_2(call, "s48_stob_set_2", "invalid stob index", 3,
			      s48_enter_integer(offset),
			      s48_unsafe_enter_long_as_fixnum_2(call, 0),
			      s48_unsafe_enter_long_as_fixnum_2(call, length - 1));
			  
  s48_unsafe_stob_set_2(call, thing, offset, value);
}

char
s48_stob_byte_ref(s48_value thing, int type, long offset)
{
  long length;

  if (!(S48_STOB_P(thing) && (S48_STOB_TYPE(thing) == type)))
    s48_assertion_violation("s48_stob_byte_ref", "not a stob", 1, thing);
  
  length = (type == S48_STOBTYPE_STRING) ?
           S48_STOB_BYTE_LENGTH(thing) - 1 :
           S48_STOB_BYTE_LENGTH(thing);
  
  if (offset < 0 || length <= offset)
    s48_assertion_violation("s48_stob_byte_ref", "invalid stob index", 3,
			    s48_enter_integer(offset),
			    S48_UNSAFE_ENTER_FIXNUM(0),
			    S48_UNSAFE_ENTER_FIXNUM(length - 1));
			  
  return S48_STOB_BYTE_REF(thing, offset);
}

char
s48_stob_byte_ref_2(s48_call_t call, s48_ref_t thing, int type, long offset)
{
  long length;

  if (!(s48_stob_p_2(call, thing) && (s48_stob_type_2(call, thing) == type)))
    s48_assertion_violation_2(call, "s48_stob_byte_ref_2", "not a stob", 1, thing);
  
  length = (type == s48_stobtype_string) ?
           s48_unsafe_stob_byte_length_2(call, thing) - 1 :
           s48_unsafe_stob_byte_length_2(call, thing);
  
  if (offset < 0 || length <= offset)
    s48_assertion_violation_2(call, "s48_stob_byte_ref_2", "invalid stob index", 3,
			      s48_enter_integer(offset),
			      s48_unsafe_enter_long_as_fixnum_2(call, 0),
			      s48_unsafe_enter_long_as_fixnum_2(call, length - 1));
			  
  return s48_unsafe_stob_byte_ref_2(call, thing, offset);
}

void
s48_stob_byte_set(s48_value thing, int type, long offset, char value)
{
  long length;

  if (!(S48_STOB_P(thing) && (S48_STOB_TYPE(thing) == type)))
    s48_assertion_violation("s48_stob_byte_set", "not a stob", 1, thing);
  
  length = (type == S48_STOBTYPE_STRING) ?
           S48_STOB_BYTE_LENGTH(thing) - 1 :
           S48_STOB_BYTE_LENGTH(thing);
  
  if (offset < 0 || length <= offset)
    s48_assertion_violation("s48_stob_byte_set", "invalid stob index", 3,
			    s48_enter_integer(offset),
			    S48_UNSAFE_ENTER_FIXNUM(0),
			    S48_UNSAFE_ENTER_FIXNUM(length - 1));
			  
  S48_STOB_BYTE_SET(thing, offset, value);
}

void
s48_stob_byte_set_2(s48_call_t call, s48_ref_t thing, int type, long offset, char value)
{
  long length;

  if (!(s48_stob_p_2(call, thing) && (s48_stob_type_2(call, thing) == type)))
    s48_assertion_violation_2(call, "s48_stob_byte_set_2", "not a stob", 1, thing);
  
  length = (type == S48_STOBTYPE_STRING) ?
           s48_unsafe_stob_byte_length_2(call, thing) - 1 :
           s48_unsafe_stob_byte_length_2(call, thing);
  
  if (offset < 0 || length <= offset)
    s48_assertion_violation_2(call, "s48_stob_byte_set_2", "invalid stob index", 3,
			      s48_enter_integer(offset),
			      s48_unsafe_enter_long_as_fixnum_2(call, 0),
			      s48_unsafe_enter_long_as_fixnum_2(call, length - 1));
			  
  s48_unsafe_stob_byte_set_2(call, thing, offset, value);
}

void *
s48_value_pointer(s48_value value)
{
  S48_CHECK_VALUE(value);

  return S48_ADDRESS_AFTER_HEADER(value, void *);
}

void *
s48_value_pointer_2(s48_call_t call, s48_ref_t value)
{
  s48_check_value_2(call, value);

  return s48_address_after_header_2(call, value, void *);
}

/********************************/
/* Numbers, characters, and pointers. */

/*
 * These two functions have the same range as the unsafe macros, but they signal
 * an error if things go wrong, instead of silently producing garbage.  Unlike
 * the integer versions they cannot cause a GC.
 */

s48_value
s48_enter_fixnum(long value)
{
  if (value < S48_MIN_FIXNUM_VALUE || S48_MAX_FIXNUM_VALUE < value)
    s48_assertion_violation("s48_enter_fixnum", "not a fixnum", 1, s48_enter_integer(value));

  return S48_UNSAFE_ENTER_FIXNUM(value);
}

s48_ref_t
s48_enter_long_as_fixnum_2(s48_call_t call, long value)
{
  if (value < S48_MIN_FIXNUM_VALUE || S48_MAX_FIXNUM_VALUE < value)
    s48_assertion_violation_2(call, "s48_enter_long_as_fixnum_2", "not a fixnum",
			      1, s48_enter_long_2(call, value));

  return s48_unsafe_enter_long_as_fixnum_2(call, value);
}

long
s48_extract_fixnum(s48_value value)
{
  if (! S48_FIXNUM_P(value))
    s48_assertion_violation("s48_extract_fixnum", "not a fixnum", 1, value);
  
  return S48_UNSAFE_EXTRACT_FIXNUM(value);
}

/* If we have a fixnum we just extract it. For bignums call the
 * functions in bignum.c.
 */

s48_ref_t
s48_enter_long_2(s48_call_t call, long value)
{
  return s48_make_local_ref(call, s48_enter_integer(value));
}

long
s48_extract_integer(s48_value value)
{
  if (S48_FIXNUM_P(value))
    return S48_UNSAFE_EXTRACT_FIXNUM(value);

  if (S48_BIGNUM_P(value)){
    bignum_type bignum = S48_ADDRESS_AFTER_HEADER(value, long);
    
    if (! s48_bignum_fits_in_word_p(bignum, 32, 1))
      s48_assertion_violation("s48_extract_integer", "does not fit in word", 1, value);
    else return s48_bignum_to_long(bignum);
  }
  else s48_assertion_violation("s48_extract_integer", "not an exact integer", 1, value);
}

long
s48_extract_long_2(s48_call_t call, s48_ref_t value)
{
  if (s48_fixnum_p_2(call, value))
    return s48_unsafe_extract_long_2(call, value);

  if (s48_bignum_p_2(call, value)){
    bignum_type bignum = s48_address_after_header_2(call, value, long);
    
    if (! s48_bignum_fits_in_word_p(bignum, sizeof(long) * BITS_PER_BYTE, 1))
      s48_assertion_violation_2(call, "s48_extract_long_2",
				"does not fit in word", 1, value);
    else return s48_bignum_to_long(bignum);
  }
  else s48_assertion_violation_2(call, "s48_extract_long_2",
				 "not an exact integer", 1, value);
}

s48_ref_t
s48_enter_unsigned_long_2(s48_call_t call, unsigned long value)
{
  return s48_make_local_ref(call, s48_enter_unsigned_integer(value));
}

unsigned long
s48_extract_unsigned_integer(s48_value value)
{
  if (S48_FIXNUM_P(value))
    {
      long fixnum = S48_UNSAFE_EXTRACT_FIXNUM(value);
      if (fixnum < 0)
	s48_assertion_violation("s48_extract_unsigned_integer", "negative argument", 1,
				value);
      return (unsigned long) fixnum;
    }

  if (S48_BIGNUM_P(value)){
    bignum_type bignum = S48_ADDRESS_AFTER_HEADER(value, long);
    
    if (! s48_bignum_fits_in_word_p(bignum, 32, 0))
      s48_assertion_violation("s48_extract_unsigned_integer", "does not fit in word", 1,
			      value);
    else return s48_bignum_to_ulong(bignum);
  }
  else s48_assertion_violation("s48_extract_unsigned_integer", "not an exact integer", 1,
			       value);
}

unsigned long
s48_extract_unsigned_long_2(s48_call_t call, s48_ref_t value)
{
  if (s48_fixnum_p_2(call, value))
    {
      long fixnum = s48_unsafe_extract_long_2(call, value);
      if (fixnum < 0)
	s48_assertion_violation_2(call, "s48_extract_unsigned_long_2",
				  "negative argument", 1, value);
      return (unsigned long) fixnum;
    }

  if (s48_bignum_p_2(call, value)){
    bignum_type bignum = s48_address_after_header_2(call, value, long);
    
    if (! s48_bignum_fits_in_word_p(bignum, sizeof(long) * BITS_PER_BYTE, 0))
      s48_assertion_violation_2(call, "s48_extract_unsigned_long_2",
			      "does not fit in word", 1, value);
    else return s48_bignum_to_ulong(bignum);
  }
  else s48_assertion_violation_2(call, "s48_extract_unsigned_long_2",
				 "not an exact integer", 1, value);
}

/*
 * Strings from and to encodings
 */

/*
 * These are just wrappers to ensure the right types.
 */

s48_ref_t
s48_enter_string_latin_1_2(s48_call_t call, const char *s)
{
  return s48_make_local_ref(call, s48_enter_string_latin_1((char*) s));
}

s48_ref_t
s48_enter_string_latin_1_n_2(s48_call_t call, const char *s, long count)
{
  return s48_make_local_ref(call, s48_enter_string_latin_1_n((char*) s, count));
}

void
s48_copy_string_to_latin_1_2(s48_call_t call, s48_ref_t sch_s, char *s)
{
  s48_copy_string_to_latin_1(s48_deref(sch_s), s);
}

void
s48_copy_string_to_latin_1_n_2(s48_call_t call, s48_ref_t sch_s, long start, long count, char *s)
{
  s48_copy_string_to_latin_1_n(s48_deref(sch_s), start, count, s);
}

void
s48_copy_latin_1_to_string_2(s48_call_t call, const char *s, s48_ref_t sch_s)
{
  s48_copy_latin_1_to_string((char*) s, s48_deref(sch_s));
}

void
s48_copy_latin_1_to_string_n_2(s48_call_t call, const char *s, long len, s48_ref_t sch_s)
{
  s48_copy_latin_1_to_string_n((char*) s, len, s48_deref(sch_s));
}

s48_ref_t
s48_enter_string_utf_8_2(s48_call_t call, const char *s)
{
  return s48_make_local_ref(call, s48_enter_string_utf_8((char*) s));
}

s48_value
s48_enter_string_utf_16be(const uint16_t *s)
{
  return s48_enter_string_utf_16beU((char*) s);
}

s48_ref_t
s48_enter_string_utf_16be_2(s48_call_t call, const uint16_t *s)
{
  return s48_make_local_ref(call, s48_enter_string_utf_16beU((char*) s));
}

s48_value
s48_enter_string_utf_16be_n(const uint16_t * s, long l)
{
  return s48_enter_string_utf_16be_nU((char*) s, l);
}

s48_ref_t
s48_enter_string_utf_16be_n_2(s48_call_t call, const uint16_t * s, long l)
{
  return s48_make_local_ref(call, s48_enter_string_utf_16be_nU((char*) s, l));
}

long
s48_copy_string_to_utf_16be(s48_value sch_s, uint16_t * s)
{
  return s48_copy_string_to_utf_16beU(sch_s, (char*) s);
}

long
s48_copy_string_to_utf_16be_2(s48_call_t call, s48_ref_t sch_s, uint16_t * s)
{
  return s48_copy_string_to_utf_16beU(s48_deref(sch_s), (char*) s);
}

long
s48_copy_string_to_utf_16be_n(s48_value sch_s, long start, long count, uint16_t *s)
{
  return s48_copy_string_to_utf_16be_nU(sch_s, start, count, (char*) s);
}

long
s48_copy_string_to_utf_16be_n_2(s48_call_t call, s48_ref_t sch_s, long start, long count, uint16_t *s)
{
  return s48_copy_string_to_utf_16be_nU(s48_deref(sch_s), start, count, (char*) s);
}

s48_value
s48_enter_string_utf_16le(const uint16_t *s)
{ 
  return s48_enter_string_utf_16leU((char *) s);
}

s48_ref_t
s48_enter_string_utf_16le_2(s48_call_t call, const uint16_t *s)
{ 
  return s48_make_local_ref(call, s48_enter_string_utf_16leU((char *) s));
}


s48_value
s48_enter_string_utf_16le_n(const uint16_t *s, long l)
{
  return s48_enter_string_utf_16le_nU((char *) s,l);
}

s48_ref_t
s48_enter_string_utf_16le_n_2(s48_call_t call, const uint16_t *s, long l)
{
  return s48_make_local_ref(call, s48_enter_string_utf_16le_nU((char *) s,l));
}

long
s48_copy_string_to_utf_16le(s48_value sch_s, uint16_t *s)
{
  return s48_copy_string_to_utf_16leU(sch_s, (char *) s);
}

long
s48_copy_string_to_utf_16le_2(s48_call_t call, s48_ref_t sch_s, uint16_t *s)
{
  return s48_copy_string_to_utf_16leU(s48_deref(sch_s), (char *) s);
}

long
s48_copy_string_to_utf_16le_n(s48_value sch_s, long start, long count, uint16_t *s)
{
  return s48_copy_string_to_utf_16le_nU(sch_s, start, count, (char *) s);
}

long
s48_copy_string_to_utf_16le_n_2(s48_call_t call, s48_ref_t sch_s, long start, long count, uint16_t *s)
{
  return s48_copy_string_to_utf_16le_nU(s48_deref(sch_s), start, count, (char *) s);
}

s48_ref_t
s48_enter_string_utf_8_n_2(s48_call_t call, const char* s, long count)
{
  return s48_make_local_ref(call, s48_enter_string_utf_8_n((char*) s, count));
}

long
s48_string_utf_8_length_2(s48_call_t call, s48_ref_t s)
{ 
  return s48_string_utf_8_length(s48_deref(s));
}

long
s48_string_utf_8_length_n_2(s48_call_t call, s48_ref_t s, long start, long count)
{
  return s48_string_utf_8_length_n(s48_deref(s), start, count);
}

long
s48_copy_string_to_utf_8_2(s48_call_t call, s48_ref_t sch_s, char* s)
{
  return s48_copy_string_to_utf_8(s48_deref(sch_s), s);
}

long
s48_copy_string_to_utf_8_n_2(s48_call_t call, s48_ref_t sch_s, long start, long count, char* s)
{
  return s48_copy_string_to_utf_8_n(s48_deref(sch_s), start, count, s);
}

long
s48_string_utf_16be_length_2(s48_call_t call, s48_ref_t sch_s)
{
  return s48_string_utf_16be_length(s48_deref(sch_s));
}

long
s48_string_utf_16be_length_n_2(s48_call_t call, s48_ref_t sch_s, long start, long count)
{
  return s48_string_utf_16be_length_n(s48_deref(sch_s), start, count);
}

long
s48_string_utf_16le_length_2(s48_call_t call, s48_ref_t sch_s)
{
  return s48_string_utf_16le_length(s48_deref(sch_s));
}

long
s48_string_utf_16le_length_n_2(s48_call_t call, s48_ref_t sch_s, long start, long count)
{
  return s48_string_utf_16le_length_n(s48_deref(sch_s), start, count);
}

long
s48_string_length_2(s48_call_t call, s48_ref_t string)
{
  return s48_string_length(s48_deref(string));
}

long
s48_string_latin_1_length_2(s48_call_t call, s48_ref_t string)
{
  return s48_string_length_2(call, string);
}

long
s48_string_latin_1_length_n_2(s48_call_t call, s48_ref_t string, long start, long count)
{
  return count;
}

void
s48_string_set_2(s48_call_t call, s48_ref_t s, long i, long c)
{
  s48_string_set(s48_deref(s), i, c);
}

long
s48_string_ref_2(s48_call_t call, s48_ref_t s, long i)
{
  return s48_string_ref(s48_deref(s), i);
}

/*
 * Extract strings to local buffer
 */

#define MAKE_STRING_EXTRACT_FUNCTION(encoding)				\
  char *s48_extract_##encoding##_from_string_2(s48_call_t call, s48_ref_t sch_s) { \
    char *buf = s48_make_local_buf(call, s48_string_##encoding##_length_2(call, sch_s)); \
    s48_copy_string_to_##encoding##_2(call, sch_s, buf);		\
    return buf;								\
  }

char *
s48_extract_latin_1_from_string_2(s48_call_t call, s48_ref_t sch_s) {
  long size = s48_string_latin_1_length_2(call, sch_s) + 1;
  char *buf = s48_make_local_buf(call, size + 1);
  s48_copy_string_to_latin_1_2(call, sch_s, buf);
  buf[size] = '\0';
  return buf;
}

char *
s48_extract_utf_8_from_string_2(s48_call_t call, s48_ref_t sch_s) {
  long size = s48_string_utf_8_length_2(call, sch_s) + 1;
  char *buf = s48_make_local_buf(call, size + 1);
  s48_copy_string_to_utf_8_2(call, sch_s, buf);
  buf[size] = '\0';
  return buf;
}

uint16_t *
s48_extract_utf_16be_from_string_2(s48_call_t call, s48_ref_t sch_s) {
  long size = s48_string_utf_16be_length_2(call, sch_s);
  uint16_t *buf = 
    (uint16_t *) s48_make_local_buf(call, (size + 1) * sizeof(uint16_t));
  s48_copy_string_to_utf_16be_2(call, sch_s, buf);
  buf[size] = 0;
  return buf;
}

uint16_t *
s48_extract_utf_16le_from_string_2(s48_call_t call, s48_ref_t sch_s) {
  long size = s48_string_utf_16le_length_2(call, sch_s);
  uint16_t *buf = 
    (uint16_t *) s48_make_local_buf(call, (size + 1) * sizeof(uint16_t));
  s48_copy_string_to_utf_16le_2(call, sch_s, buf);
  buf[size] = 0;
  return buf;
}

/*
 * Doubles and characters are straightforward.
 */

s48_value
s48_enter_double(double value)
{
  s48_value obj;

  obj = s48_allocate_stob(S48_STOBTYPE_DOUBLE, sizeof(double));
  S48_UNSAFE_EXTRACT_DOUBLE(obj) = value;

  return obj;
}

s48_ref_t
s48_enter_double_2(s48_call_t call, double value)
{
  s48_ref_t ref;
  ref = s48_make_local_ref(call, s48_allocate_stob(S48_STOBTYPE_DOUBLE, sizeof(double)));
  s48_unsafe_extract_double_2(call, ref) = value;

  return ref;
}

double
s48_extract_double(s48_value s48_double)
{
  if (! S48_DOUBLE_P(s48_double))
    s48_assertion_violation("s48_extract_double", "not a double", 1, s48_double);
  
  return S48_UNSAFE_EXTRACT_DOUBLE(s48_double);
}

double
s48_extract_double_2(s48_call_t call, s48_ref_t s48_double)
{
  if (! s48_double_p_2(call, s48_double))
    s48_assertion_violation_2(call, "s48_extract_double_2",
			      "not a double", 1, s48_double);
  
  return s48_unsafe_extract_double_2(call, s48_double);
}

s48_value
s48_enter_char(long a_char)
{
  if (! ((a_char >= 0)
	 && ((a_char <= 0xd7ff)
	     || ((a_char >= 0xe000) && (a_char <= 0x10ffff)))))
    s48_assertion_violation("s48_enter_char", "not a scalar value", 1, s48_enter_fixnum(a_char));

  return S48_UNSAFE_ENTER_CHAR(a_char);
}

s48_ref_t
s48_enter_char_2(s48_call_t call, long a_char)
{
  if (! ((a_char >= 0)
	 && ((a_char <= 0xd7ff)
	     || ((a_char >= 0xe000) && (a_char <= 0x10ffff)))))
    s48_assertion_violation_2(call, "s48_enter_char_2",
			      "not a scalar value", 1, s48_enter_long_as_fixnum_2(call, a_char));

  return s48_unsafe_enter_char_2(call, a_char);
}

long
s48_extract_char(s48_value a_char)
{
  if (! S48_CHAR_P(a_char))
    s48_assertion_violation("s48_extract_char", "not a char", 1, a_char);
  
  return S48_UNSAFE_EXTRACT_CHAR(a_char);
}

long
s48_extract_char_2(s48_call_t call, s48_ref_t a_char)
{
  if (! s48_char_p_2(call, a_char))
    s48_assertion_violation_2(call, "s48_extract_char_2", "not a char", 1, a_char);
  
  return s48_unsafe_extract_char_2(call, a_char);
}

/********************************/
/* Allocation */

s48_value
s48_enter_pointer(void *pointer)
{
  s48_value obj;

  obj = s48_allocate_stob(S48_STOBTYPE_BYTE_VECTOR, sizeof(void *));
  *(S48_ADDRESS_AFTER_HEADER(obj, void *)) = pointer;

  return obj;
}

s48_ref_t
s48_enter_pointer_2(s48_call_t call, void *pointer)
{
  s48_ref_t ref;

  ref = s48_make_local_ref(call, s48_allocate_stob(S48_STOBTYPE_BYTE_VECTOR, sizeof(void *)));
  *(s48_address_after_header_2(call, ref, void *)) = pointer;

  return ref;
}

void*
s48_extract_pointer(s48_value sch_pointer)
{
  S48_CHECK_VALUE(sch_pointer);
  return *(S48_ADDRESS_AFTER_HEADER(sch_pointer, void *));
}

void*
s48_extract_pointer_2(s48_call_t call, s48_ref_t sch_pointer)
{
  s48_check_value_2(call, sch_pointer);
  return *(s48_address_after_header_2(call, sch_pointer, void *));
}

s48_ref_t
s48_get_imported_binding_2(char *name)
{
  return s48_make_global_ref(s48_get_imported_binding(name));
}

s48_ref_t
s48_get_imported_binding_local_2(s48_call_t call, char *name)
{
  return s48_make_local_ref(call, s48_get_imported_binding(name)); 
}

s48_ref_t
s48_define_exported_binding_2(s48_call_t call, char *name, s48_ref_t binding)
{
  return s48_make_local_ref(call, s48_define_exported_binding(name, s48_deref(binding)));
}

s48_value
s48_cons(s48_value v1, s48_value v2)
{
  s48_value obj;
  S48_DECLARE_GC_PROTECT(2);

  S48_GC_PROTECT_2(v1, v2);

  obj = s48_allocate_stob(S48_STOBTYPE_PAIR, 2);
  S48_UNSAFE_SET_CAR(obj, v1);
  S48_UNSAFE_SET_CDR(obj, v2);

  S48_GC_UNPROTECT();
  return obj;
}

s48_ref_t
s48_cons_2(s48_call_t call, s48_ref_t v1, s48_ref_t v2)
{
  s48_ref_t ref;
  ref = s48_make_local_ref(call, s48_allocate_stob(S48_STOBTYPE_PAIR, 2));
  s48_unsafe_set_car_2(call, ref, v1);
  s48_unsafe_set_cdr_2(call, ref, v2);
  return ref;
}

s48_value
s48_make_weak_pointer(s48_value value)
{
  s48_value obj;
  S48_DECLARE_GC_PROTECT(1);

  S48_GC_PROTECT_1(value);

  obj = s48_allocate_weak_stob(S48_STOBTYPE_WEAK_POINTER, 1);
  S48_STOB_SET(obj, 0, value);

  S48_GC_UNPROTECT();
  return obj;
}

s48_ref_t
s48_make_weak_pointer_2(s48_call_t call, s48_ref_t value)
{
  s48_ref_t ref = s48_make_local_ref(call, s48_allocate_weak_stob(S48_STOBTYPE_WEAK_POINTER, 1));
  s48_unsafe_stob_set_2(call, ref, 0, value);
  return ref;
}

/*
 * Entering and extracting byte vectors.
 */

s48_value
s48_enter_byte_vector(char *bytes, long length)
{
  s48_value obj = s48_make_byte_vector(length);
  memcpy(S48_UNSAFE_EXTRACT_BYTE_VECTOR(obj), bytes, length);
  return obj;
}

s48_ref_t
s48_enter_byte_vector_2(s48_call_t call, const char *bytes, long length)
{
  s48_ref_t ref = s48_make_byte_vector_2(call, length);
  s48_enter_byte_vector_region_2(call, ref, 0, length, (char *) bytes);
  return ref;
}

s48_value
s48_enter_unmovable_byte_vector(char *bytes, long length)
{
  s48_value obj = s48_make_unmovable_byte_vector(length);
  memcpy(S48_UNSAFE_EXTRACT_BYTE_VECTOR(obj), bytes, length);
  return obj;
}

s48_ref_t
s48_enter_unmovable_byte_vector_2(s48_call_t call, const char *bytes, long length)
{
  s48_ref_t ref = s48_make_unmovable_byte_vector_2(call, length);
  s48_enter_byte_vector_region_2(call, ref, 0, length, (char *) bytes);
  return ref;
}

char *
s48_extract_byte_vector(s48_value byte_vector)
{
  S48_CHECK_VALUE(byte_vector);

  return S48_UNSAFE_EXTRACT_BYTE_VECTOR(byte_vector);
}

char *
s48_extract_byte_vector_2(s48_call_t call, s48_ref_t byte_vector)
{
  long s = s48_byte_vector_length_2(call, byte_vector);
  char *buf = s48_make_local_bv(call, byte_vector, s);
  return buf;
}

char *
s48_extract_byte_vector_readonly_2(s48_call_t call, s48_ref_t byte_vector)
{
  long s = s48_byte_vector_length_2(call, byte_vector);
  char *buf = s48_make_local_bv_readonly(call, byte_vector, s);
  return buf;
}

void
s48_extract_byte_vector_region_2(s48_call_t call, s48_ref_t byte_vector,
				 long start, long length, char *buf)
{
  char *scheme_buf;
  s48_check_value_2(call, byte_vector);
  scheme_buf = s48_unsafe_extract_byte_vector_2(call, byte_vector);
  memcpy(buf, scheme_buf + start, length);
}

void
s48_enter_byte_vector_region_2(s48_call_t call, s48_ref_t byte_vector,
			       long start, long length, char *buf)
{
  char *scheme_buf;
  s48_check_value_2(call, byte_vector);
  scheme_buf = s48_unsafe_extract_byte_vector_2(call, byte_vector);
  memcpy(scheme_buf + start, buf, length);
}

void
s48_copy_from_byte_vector_2(s48_call_t call, s48_ref_t byte_vector, char *buf)
{
  s48_extract_byte_vector_region_2(call, byte_vector, 0,
				   s48_byte_vector_length_2(call, byte_vector), buf);
}

void
s48_copy_to_byte_vector_2(s48_call_t call, s48_ref_t byte_vector, char *buf)
{
  s48_enter_byte_vector_region_2(call, byte_vector, 0,
				   s48_byte_vector_length_2(call, byte_vector), buf);
}

psbool
s48_unmovable_p(s48_call_t call, s48_ref_t ref)
{
  return s48_unmovableP(s48_deref(ref));
}

char *
s48_extract_unmovable_byte_vector_2(s48_call_t call, s48_ref_t byte_vector)
{
  s48_check_value_2(call, byte_vector);
  if (!s48_unmovable_p(call, byte_vector))
    s48_assertion_violation("s48_extract_unmovable_byte_vector_2",
			    "not an unmovable byte vector", 1, byte_vector);
  return s48_unsafe_extract_byte_vector_2(call, byte_vector);
}

/* 
   The returned byte vector by s48_extract_byte_vector_unmanaged_2 may
   be a copy of the Scheme byte vector, changes made to the returned
   byte vector will not necessarily be reflected in Scheme until
   s48_release_byte_vector_2 is called. 
*/
char *
s48_extract_byte_vector_unmanaged_2(s48_call_t call, s48_ref_t byte_vector)
{
  if (s48_unmovable_p(call, byte_vector))
    {
      return s48_extract_unmovable_byte_vector_2(call, byte_vector);
    }
  else
    {
      long len = s48_byte_vector_length_2(call, byte_vector);
      char *buf = s48_make_local_buf(call, len);
      s48_extract_byte_vector_region_2(call, byte_vector, 0, len, buf);
      return buf;
    }
}

void
s48_release_byte_vector_2(s48_call_t call, s48_ref_t byte_vector, char *buf)
{
  if (!s48_unmovable_p(call, byte_vector))
    s48_copy_to_byte_vector_2(call, byte_vector, buf);
}

/*
 * Making various kinds of stored objects.
 */

s48_value
s48_make_string(int length, long init)
{
  int i;
  s48_value obj = s48_allocate_string(length);
  /* We should probably offer a VM function for this. */
  for (i = 0; i < length; ++i)
    s48_string_set(obj, i, init);
  return obj;
}

s48_ref_t
s48_make_string_2(s48_call_t call, int length, long init)
{
  int i;
  s48_ref_t ref = s48_make_local_ref(call, s48_allocate_string(length));
  /* We should probably offer a VM function for this. */
  for (i = 0; i < length; ++i)
    s48_string_set(s48_deref(ref), i, init);
  return ref;
}

s48_value
s48_make_vector(long length, s48_value init)
{
  long i;
  s48_value obj;
  S48_DECLARE_GC_PROTECT(1);

  S48_GC_PROTECT_1(init);

  obj = s48_allocate_stob(S48_STOBTYPE_VECTOR, length);
  for (i = 0; i < length; ++i)
    S48_UNSAFE_VECTOR_SET(obj, i, init);

  S48_GC_UNPROTECT();

  return obj;
}

s48_ref_t
s48_make_vector_2(s48_call_t call, long length, s48_ref_t init)
{
  long i;
  s48_ref_t ref = s48_make_local_ref(call, s48_allocate_stob(S48_STOBTYPE_VECTOR, length));
  for (i = 0; i < length; ++i)
    s48_unsafe_vector_set_2(call, ref, i, init);
  return ref;
}

s48_value
s48_make_byte_vector(long length)
{
    return s48_allocate_stob(S48_STOBTYPE_BYTE_VECTOR, length);
}

s48_ref_t
s48_make_byte_vector_2(s48_call_t call, long length)
{
  return s48_make_local_ref(call, s48_allocate_stob(S48_STOBTYPE_BYTE_VECTOR, length));
}

s48_value
s48_make_unmovable_byte_vector(long length)
{
    return s48_allocate_unmovable_stob(S48_STOBTYPE_BYTE_VECTOR, length);
}

s48_ref_t
s48_make_unmovable_byte_vector_2(s48_call_t call, long length)
{
  return s48_make_local_ref(call, s48_allocate_unmovable_stob(S48_STOBTYPE_BYTE_VECTOR, length));
}

s48_value
s48_enter_byte_substring(char *str, long length)
{
  s48_value obj = s48_allocate_stob(S48_STOBTYPE_BYTE_VECTOR, length + 1);
  memcpy(S48_UNSAFE_EXTRACT_BYTE_VECTOR(obj), str, length);
  *(S48_UNSAFE_EXTRACT_BYTE_VECTOR(obj) + length) = '\0';
  return obj;
}

s48_ref_t
s48_enter_byte_substring_2(s48_call_t call, const char *str, long length)
{
  s48_ref_t ref = s48_make_byte_vector_2(call, length + 1);
  s48_enter_byte_vector_region_2(call, ref, 0, length, (char *) str);
  s48_byte_vector_set_2(call, ref, length, '\0');
  return ref;
}

s48_value
s48_enter_byte_string(char *str)
{
  return s48_enter_byte_substring(str, strlen(str));
}

s48_ref_t
s48_enter_byte_string_2(s48_call_t call, const char *str)
{
  return s48_enter_byte_substring_2(call, str, strlen(str));
}

s48_value
s48_make_record(s48_value type_shared_binding)
{
    long i, number_of_fields;
    s48_value record = S48_FALSE;
    s48_value record_type = S48_FALSE;
    S48_DECLARE_GC_PROTECT(1);

    S48_GC_PROTECT_1(record_type);

    S48_SHARED_BINDING_CHECK(type_shared_binding);
    S48_SHARED_BINDING_CHECK(s48_deref(the_record_type_binding));

    record_type = S48_SHARED_BINDING_REF(type_shared_binding);

    s48_check_record_type(record_type, s48_deref(the_record_type_binding));

    number_of_fields =
      S48_UNSAFE_EXTRACT_FIXNUM(S48_RECORD_TYPE_NUMBER_OF_FIELDS(record_type));

    record = s48_allocate_stob(S48_STOBTYPE_RECORD, number_of_fields + 1);

    S48_UNSAFE_RECORD_SET(record, -1, record_type);
    for (i = 0; i < number_of_fields; ++i)
      S48_UNSAFE_RECORD_SET(record, i, S48_UNSPECIFIC);

    S48_GC_UNPROTECT();

    return record;
}

s48_ref_t
s48_make_record_2(s48_call_t call, s48_ref_t type_shared_binding)
{
    long i, number_of_fields;
    s48_ref_t record;
    s48_ref_t record_type;

    s48_shared_binding_check_2(call, type_shared_binding);
    s48_shared_binding_check_2(call, the_record_type_binding);

    record_type = s48_shared_binding_ref_2(call, type_shared_binding);

    s48_check_record_type_2(call, record_type, the_record_type_binding);

    number_of_fields =
      s48_unsafe_extract_long_2(call, 
				s48_record_type_number_of_fields_2(call, record_type));

    record = s48_make_local_ref(call, s48_allocate_stob(S48_STOBTYPE_RECORD, number_of_fields + 1));

    s48_unsafe_record_set_2(call, record, -1, record_type);
    for (i = 0; i < number_of_fields; ++i)
      s48_unsafe_record_set_2(call, record, i, s48_unspecific_2(call));

    return record;
}

/*
 * Raise an exception if `record' is not a record whose type is the one
 * found in `type_binding'.
 */
void
s48_check_record_type(s48_value record, s48_value type_binding)
{
  if (! S48_RECORD_P(S48_SHARED_BINDING_REF(type_binding)))
    s48_raise_scheme_exception(S48_EXCEPTION_UNBOUND_EXTERNAL_NAME, 1,
			       S48_SHARED_BINDING_NAME(type_binding));

  if ((! S48_RECORD_P(record)) ||
      (S48_UNSAFE_SHARED_BINDING_REF(type_binding) !=
       S48_UNSAFE_RECORD_REF(record, -1)))
    s48_assertion_violation("s48_check_record_type", "not a record of the appropriate type", 2,
			    record, S48_SHARED_BINDING_REF(type_binding));
}

void
s48_check_record_type_2(s48_call_t call, s48_ref_t record, s48_ref_t type_binding)
{
  if (! s48_record_p_2(call, s48_shared_binding_ref_2(call, type_binding)))
    s48_raise_scheme_exception_2(call,S48_EXCEPTION_UNBOUND_EXTERNAL_NAME, 1,
				 s48_shared_binding_name_2(call, type_binding));

  if ((! s48_record_p_2(call, record)) ||
      (!s48_eq_p_2(call,
		   s48_unsafe_shared_binding_ref_2(call, type_binding),
		   s48_unsafe_record_ref_2(call, record, -1))))
    s48_assertion_violation_2(call, "s48_check_record_type_2",
			      "not a record of the appropriate type", 2,
			      record, s48_shared_binding_ref_2(call, type_binding));
}

long
s48_length(s48_value list)
{
  long i = 0;

  while (!(S48_EQ(list, S48_NULL)))
    {
      list = S48_CDR(list);
      ++i;
    }
  return S48_UNSAFE_ENTER_FIXNUM(i);
}

s48_ref_t
s48_length_2(s48_call_t call, s48_ref_t list)
{
  s48_ref_t l = s48_copy_local_ref(call, list);
  long i = 0;
  while (!(s48_null_p_2(call, l)))
    {
      s48_ref_t temp = l;
      l = s48_cdr_2(call, l);
      s48_free_local_ref(call, temp);
      ++i;
    }
  return s48_unsafe_enter_long_as_fixnum_2(call, i);
}
