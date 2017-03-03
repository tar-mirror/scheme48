/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Marcus Crestani
 */

/*
 * Externally visible objects defined in scheme48vm.c.
 */

#include "scheme48.h"
#include "c-mods.h"

/* initializing */
extern void		s48_init(void);
extern long		s48_initialize(int *argc, char ***argv);
extern void		s48_initialize_vm(char *, long);

/* running */
extern long		s48_call_startup_procedure(char **, long);
extern s48_value	s48_restart(s48_value proc, long nargs);
extern s48_value	s48_Scallback_return_stack_blockS;
	  
/* for extension.c */
extern void		s48_set_extension_valueB(s48_value);
extern s48_value	s48_Sextension_valueS;

/* interrupts */
extern void		s48_note_event(void);
extern psbool		s48_Spending_eventsPS;
extern char *		s48_Sstack_limitS;
extern void		s48_disable_interruptsB(void);
extern void		s48_enable_interruptsB(void);
extern void		s48_set_os_signals(s48_value list);
extern void		s48_reset_interruptsB();
extern char		s48_os_signal_pending(void);
extern void		s48_note_external_eventBUunsafe(long);
extern char		s48_external_event_readyPUunsafe(void);
extern char		s48_external_event_pendingPUunsafe(void);
extern long		s48_dequeue_external_eventBUunsafe(char*);

/* imported and exported bindings */	  
S48_EXTERN s48_value	s48_define_exported_binding(char *, s48_value);
S48_EXTERN s48_value	s48_get_imported_binding(char *);

/* for raising exceptions in external code */
extern void		s48_setup_external_exception(s48_value exception,
						     long nargs);
extern s48_value	s48_resetup_external_exception(s48_value exception,
						       long additional_nargs);
extern void		s48_argument_type_violation(s48_value value);
extern void		s48_range_violation(s48_value value, s48_value min, s48_value max);
extern void		s48_push(s48_value value);
extern s48_value	s48_stack_ref(long offset);
extern void		s48_stack_setB(long offset, s48_value value);

/* strings */

S48_EXTERN s48_value	s48_allocate_string(long l);

extern s48_value s48_enter_string_utf_16beU(char *);
extern s48_value s48_enter_string_utf_16be_nU(char *, long);
extern long s48_copy_string_to_utf_16beU(s48_value, char *);
extern long s48_copy_string_to_utf_16be_nU(s48_value, long, long, char *);

extern s48_value s48_enter_string_utf_16leU(char *);
extern s48_value s48_enter_string_utf_16le_nU(char *, long);
extern long s48_copy_string_to_utf_16leU(s48_value, char *);
extern long s48_copy_string_to_utf_16le_nU(s48_value, long, long, char *);

/* called when writing an image */
extern s48_value	s48_symbol_table(void);
extern s48_value	*s48_channels(void);
extern long		s48_channel_count(void);
extern s48_value	s48_imported_bindings(void);
extern s48_value	s48_exported_bindings(void);

/* for initializion on SMP machines */
extern void		s48_initialize_shared_registersB(long, long, long, long);

/* manipulating channels */
S48_EXTERN void		s48_close_channel(long);
S48_EXTERN s48_value	s48_set_channel_os_index(s48_value, long);
extern s48_value	s48_really_add_channel(s48_value, s48_value, long);

/* external allocation and GC roots */
extern void		s48_gc_root(void);
extern long		s48_gc_run_time(long *);       
extern s48_value	s48_allocate_stob(long type, long size);
extern s48_value	s48_allocate_weak_stob(long type, long size);
extern s48_value	s48_allocate_unmovable_stob(long type, long size);
extern psbool		s48_unmovableP(s48_value);
extern psbool		s48_gc_can_allocate_unmovableP(void);
S48_EXTERN void		s48_push_gc_rootsB(char *, long);
S48_EXTERN psbool	s48_pop_gc_rootsB(void);
extern char *		s48_set_gc_roots_baseB(void);
extern psbool		s48_release_gc_roots_baseB(char *);
S48_EXTERN char *	s48_register_gc_rootB(char *);
S48_EXTERN void		s48_unregister_gc_rootB(char *);
extern void		s48_reset_external_rootsB(void);
extern void		s48_post_gc_cleanup(psbool, psbool);
extern void		s48_trace_external_calls(void);

/* for native code */
extern void		s48_copy_stack_into_heap();

/* variables for native code (the names need to be fixed) */
extern s48_value	StemplateS;
extern char *		Scode_pointerS;
extern char *		ScontS;
extern char *		SstackS;
extern s48_value	SenvS;
extern s48_value	SvalS;
extern long	        s48_Snative_protocolS;
