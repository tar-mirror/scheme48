/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Richard Kelsey, Jonathan Rees, Mike Sperber
 */

/* must be synchronized with EVENTS enumeration in ps-channel.scm and s48-channel.scm */
enum event_enum { KEYBOARD_INTERRUPT_EVENT, IO_COMPLETION_EVENT, IO_ERROR_EVENT,
		  ALARM_EVENT, OS_SIGNAL_EVENT, ERROR_EVENT, EXTERNAL_EVENT, NO_EVENT };

S48_EXTERN psbool s48_add_pending_fd(int fd, psbool is_input);
S48_EXTERN psbool s48_remove_fd(int fd);

S48_EXTERN long s48_schedule_alarm_interrupt(long delta);
S48_EXTERN void s48_start_alarm_interrupts(void);
S48_EXTERN void s48_stop_alarm_interrupts(void);

#ifndef _WIN32
S48_EXTERN void s48_add_os_signal(long);
S48_EXTERN void s48_when_keyboard_interrupt(int ign);
S48_EXTERN void s48_when_alarm_interrupt(int ign);
S48_EXTERN void s48_when_external_event_interrupt(int ign);
#endif

extern long s48_run_time(long *mseconds);
extern long s48_real_time(long *mseconds);
extern int  s48_wait_for_event(long max_wait, psbool is_minutes);
extern int  s48_get_next_event(long *ready_fd, long *status);
extern long s48_dequeue_external_event(char *pendingp);

/* these are here only for the CHEAP_TIME() macro */
#define TICKS_PER_SECOND 1000	/* clock resolution */
#define POLLS_PER_SECOND   20   /* how often we poll */
#define TICKS_PER_POLL  (TICKS_PER_SECOND / POLLS_PER_SECOND)

extern long s48_current_time;
#define CHEAP_TIME()  (s48_current_time * TICKS_PER_POLL)

#ifndef _WIN32
S48_EXTERN char s48_Spending_interruptPS;
S48_EXTERN char s48_Spending_eventsPS;
S48_EXTERN char *s48_Sstack_limitS;
#endif

/*
 * Fix (HCC) NOTE_EVENT so that it will act like a single
 * statement.
 */
#define	NOTE_EVENT					\
	do {						\
             s48_Spending_eventsPS = 1;			\
             s48_Sstack_limitS = (((char *) -1));	\
	} while (0)

/*
 * For Unix.
 */
#ifdef SIGUSR1
#define SIG_EXTERNAL_EVENT SIGUSR1
#endif
