/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Marcus Crestani,
 * Robert Ransom, Will Noble, Roderic Morris
 */

/*
 * Scheme 48/POSIX process environment interface
 */

#define _GNU_SOURCE
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "c-mods.h"
#include "scheme48.h"
#include "scheme48vm.h"
#include "event.h"
#include "posix.h"
#include "unix.h"
#include "sysdep.h"

/*
 * Mapping from our `canonical' signal numbers to the local OS's
 * numbers. To avoid having to manually keep the values here in sync
 * with the NAMED-SIGNALS finite record type, we generate the values
 * using a Scheme program.
 */
static int signal_map[] = {
#include "s48_signals.h"
};

extern void		s48_init_posix_proc(void),
			s48_uninit_posix_proc(void);
static s48_ref_t	posix_fork(s48_call_t call),
			posix_exec(s48_call_t call, s48_ref_t program, s48_ref_t lookup_p,
				   s48_ref_t env, s48_ref_t args),
  			posix_waitpid(s48_call_t call),
			posix_initialize_named_signals(s48_call_t call),
			posix_request_interrupts(s48_call_t call, s48_ref_t int_number),  
			posix_cancel_interrupt_request(s48_call_t call, s48_ref_t sch_signal),
  			posix_kill(s48_call_t call, s48_ref_t sch_pid, s48_ref_t sch_signal);

static void		cancel_interrupt_requests(void);

static char		**enter_byte_vector_array(s48_call_t call, s48_ref_t strings);

/*
 * Vector of Scheme signal objects imported from Scheme, and a marker that
 * is put in unnamed signals.
 */

static s48_ref_t	posix_signals_vector_binding;

/*
 * Queue of received interrupts that need to be passed on to Scheme.
 * Kept in a finite array to avoid consing.
 */

/*
 * Install all exported functions in Scheme48.
 */
void
s48_init_posix_proc(void)
{
  S48_EXPORT_FUNCTION(posix_fork);
  S48_EXPORT_FUNCTION(posix_exec);
  S48_EXPORT_FUNCTION(posix_waitpid);
  S48_EXPORT_FUNCTION(posix_initialize_named_signals);
  S48_EXPORT_FUNCTION(posix_request_interrupts);
  S48_EXPORT_FUNCTION(posix_cancel_interrupt_request);
  S48_EXPORT_FUNCTION(posix_kill);

  posix_signals_vector_binding =
    s48_get_imported_binding_2("posix-signals-vector");
}

void
s48_uninit_posix_proc(void)
{
  /* this will lose our signal handlers without reinstalling them; too bad */
  cancel_interrupt_requests();
}

/*
 * Waiting for children.  We get finished pid's until we reach one for which
 * there is a Scheme pid record.  The exit status or terminating signal is
 * saved in the record which is then returned.
 *
 * This does not looked for stopped children, only terminated ones.
 */
static s48_ref_t
posix_waitpid(s48_call_t call)
{
  int status;
  pid_t pid;
  s48_ref_t result;

 retry:
  pid = waitpid(-1, &status, WNOHANG);
  if (pid < 0) {
    if (errno == EINTR) goto retry;
    else if (errno == ECHILD) return s48_false_2(call);
    else s48_os_error_2(call, "posix_waitpid", errno, 0);
  } else if (pid == 0) {
    return s48_false_2(call); /* no statuses available now */
  }

  result = s48_make_vector_2(call, 3, s48_false_2(call));
  s48_unsafe_vector_set_2(call, result, 0, s48_enter_long_2(call, pid));
  if (WIFEXITED(status))
    s48_unsafe_vector_set_2(call, result, 1,
			    s48_enter_long_2(call, WEXITSTATUS(status)));
  else
    s48_unsafe_vector_set_2(call, result, 2,
			    s48_enter_long_2(call, WTERMSIG(status)));
  return result;
}

/*
 * Fork and exec.
 */

static s48_ref_t
posix_fork(s48_call_t call)
{
  pid_t pid = fork();
  if (pid < 0)
    s48_os_error_2(call, "posix_fork", errno, 0);
  return s48_enter_long_2(call, pid);
}

#ifndef HAVE_EXECVPE
static int execvpe(const char *file, char **argv, char **env) {
  char *path, *buf;
  int path_len, file_len;

  path = getenv("PATH");
  if (path == NULL) path = "/bin:/usr/bin";
  else if (*path == '\0') path = ".";
  path_len = strlen(path);
  file_len = strlen(file);
  buf = malloc(path_len + file_len + 2);
  if (buf == NULL)
    s48_out_of_memory_error();

  while (*path) {
    int len;
    char *colon = strchr(path, ':');

    if (path == colon) {
      path++;
      path_len--;
      continue;
    }

    len = (colon == NULL) ? path_len : (colon - path);
    memcpy(buf, path, len);
    buf[len] = '/';
    memcpy(buf + len + 1, file, file_len);
    buf[len + file_len + 1] = '\0';
    execve(buf, argv, env);

    if (errno == EACCES || errno == ENOENT || errno == ENOTDIR) {
      path_len -= len;
      path += len;
    } else {
      /* File accessible but failed to execute */
      break;
    }
  }

  free(buf);
  return -1;
}
#endif /* HAVE_EXECVPE */

/*
 * The environment is an array of strings of the form "name=value", where
 * `name' cannot contain `='.
 */

static s48_ref_t
posix_exec(s48_call_t call, s48_ref_t program, s48_ref_t lookup_p,
	   s48_ref_t env, s48_ref_t args)
{
  char **c_args = enter_byte_vector_array(call, args);
  char *c_program;
  int status;

  c_program = s48_extract_byte_vector_readonly_2(call, program);

  s48_stop_alarm_interrupts();

  if (s48_false_p_2(call, env)) {
    if (s48_false_p_2(call, lookup_p))
      status = execv(c_program, c_args);
    else
      status = execvp(c_program, c_args);
  }
  else {
    char **c_env = enter_byte_vector_array(call, env);
    
    if (s48_false_p_2(call, lookup_p) || strchr(c_program, '/'))
      status = execve(c_program, c_args, c_env);
    else
      status = execvpe(c_program, c_args, c_env);

    free(c_env);
  }

  /* If we get here, then something has gone wrong. */

  free(c_args);
  s48_start_alarm_interrupts();
  s48_os_error_2(call, "posix_exec", errno, 0);

  /* appease gcc -Wall */
  return s48_false_2(call);
}

/*
 * Convert a list of byte vectors into an array of char pointers.
 */

static char **
enter_byte_vector_array(s48_call_t call, s48_ref_t vectors)
{
  int length = s48_unsafe_extract_long_2(call, s48_length_2(call, vectors));
  char **result = (char **)malloc((length + 1) * sizeof(char *));
  int i;

  if (result == NULL)
    s48_out_of_memory_error();
  
  for(i = 0; i < length; i++, vectors = s48_unsafe_cdr_2(call, vectors)) {
    s48_ref_t vector = s48_unsafe_car_2(call, vectors);
    if (! s48_byte_vector_p_2(call, vector)) {
      free(result);
      s48_assertion_violation_2(call, NULL, "not a byte vector", 1, vector); }
    result[i] = s48_extract_byte_vector_readonly_2(call, vector); }
  result[length] = NULL;

  return result;
}
  
/*
 * Signals
 */

/*
 * Simple front for kill().  We have to retry if interrupted.
 */

s48_ref_t
posix_kill(s48_call_t call, s48_ref_t pid, s48_ref_t signal)
{
  int status;

  RETRY_OR_RAISE_NEG(status,
		     kill(s48_extract_long_2(call, pid),
			  s48_extract_long_2(call, signal)));

  return s48_unspecific_2(call);
}

/*
 * Use the signal map to set the os-number slot in each named signal to
 * its value in the current OS.
 */

static s48_ref_t
posix_initialize_named_signals(s48_call_t call)
{
  int i, length;
  s48_ref_t named_signals;

  s48_shared_binding_check_2(call, posix_signals_vector_binding);

  named_signals = s48_shared_binding_ref_2(call, posix_signals_vector_binding);

  if(! s48_vector_p_2(call, named_signals))
    s48_assertion_violation_2(call, 
			      "posix_initialize_named_signals", "not a vector", 1,
			      named_signals);
    
  length = s48_unsafe_vector_length_2(call, named_signals);

  for(i = 0; i < length; i++) {
    s48_ref_t signal = s48_unsafe_vector_ref_2(call, named_signals, i);
    int canonical = s48_extract_long_2(call, s48_unsafe_record_ref_2(call, signal, 1));
    int c_signal = signal_map[canonical];
    s48_ref_t scm_signal = (c_signal == -1) ?
                           s48_false_2(call) :
                           s48_enter_long_2(call, c_signal);
    
    s48_unsafe_record_set_2(call, signal, 2, scm_signal); }

  return s48_unspecific_2(call);
}

/*
 * Queue the interrupt.  For SIGINT and SIGALRM we call the event-system's
 * handler as well.
 */

static void
generic_interrupt_catcher(int signum)
{
  extern void s48_add_os_signal(long);
  s48_add_os_signal(signum);

  switch (signum) {
  case SIGINT: {
    s48_when_keyboard_interrupt(0);
    break; }
  case SIGALRM: {
    s48_when_alarm_interrupt(0);
    break; }
  case SIG_EXTERNAL_EVENT: {
    s48_when_external_event_interrupt(0);
    break; }
  default:
    NOTE_EVENT; }
  
  return;
}

/*
 * Array of actions to be restored when we no longer listen for a signal.
 */

#define MAX_SIGNAL 1023			/* Just a guess. */

struct sigaction *saved_actions[MAX_SIGNAL + 1] = {NULL};

/*
 * If there is a saved action then our handler is already in place and
 * we need do nothing.  Otherwise we save the current action and install
 * our own.
 */

s48_ref_t
posix_request_interrupts(s48_call_t call, s48_ref_t sch_signum)
{
  int			signum = s48_extract_long_2(call, sch_signum);
  struct sigaction	sa;

  if (saved_actions[signum] == NULL) {
    struct sigaction *	old = (struct sigaction *)
                                malloc(sizeof(struct sigaction));
    
    if (old == NULL)
      s48_out_of_memory_error();

    sa.sa_handler = generic_interrupt_catcher;
    sigfillset(&sa.sa_mask);
    sa.sa_flags = 0;

    if (sigaction(signum, &sa, old) != 0) {
      free(old);
      s48_os_error_2(call, "posix_request_interrupts", errno, 1, sch_signum); }

    saved_actions[signum] = old; }
    
  return s48_unspecific_2(call);
}

/*
 * The reverse of the above.  If there is a saved action then we install it
 * and remove it from the saved_action array.
 */

static void
cancel_interrupt_request(int signum)
{
  struct sigaction *	old = saved_actions[signum];

  if (old != NULL)
    {
      
      if (sigaction(signum, old, (struct sigaction *) NULL) != 0)
	/* THIS IS STILL OLD FFI!  FIX THIS! */
	s48_os_error_2(NULL, NULL, errno, 1, s48_enter_fixnum(signum));
      
      free(old);
      saved_actions[signum] = NULL; 
    }
}

s48_ref_t
posix_cancel_interrupt_request(s48_call_t call, s48_ref_t sch_signum)
{
  cancel_interrupt_request(s48_extract_long_2(call, sch_signum));
  return s48_unspecific_2(call);
}

static void
cancel_interrupt_requests(void)
{
  int signum = 0;
  while (signum <= MAX_SIGNAL)
    {
      cancel_interrupt_request(signum);
      ++signum;
    }
}
