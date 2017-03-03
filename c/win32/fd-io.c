/* Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Mike Sperber, Marcus Crestani, Robert Ransom
 */

#define NO_OLD_FFI 1

#define _WIN32_WINNT 0x0400 /* for QueueUserAPC */

#include <winsock2.h>
#include <mswsock.h>
#include <windows.h>
#include <ws2tcpip.h>

#include <stdio.h>
#include <errno.h>              /* for errno, (POSIX?/ANSI) */
#include "scheme48vm.h"
#include "event.h"
#include "fd-io.h"

#include "socket.h"
#include "address.h"

extern int s48_utf_8of16_to_utf_16(const unsigned char* utf_8of16,
				   LPWSTR utf_16,
				   int* errorp);

enum stream_descriptor_type {
  STREAM_FILE_REGULAR, /* overlapped I/O works */
  STREAM_FILE_SPECIAL, /* overlapped I/O doesn't work */
  STREAM_SOCKET
};

typedef struct
{
  HANDLE thread_handle; /* parameter */
  HANDLE file_handle; /* parameter */
  HANDLE check_semaphore; /* parameter */
  HANDLE abort_semaphore; /* parameter */

  /* in */
  psbool abort;
  char* buffer;
  size_t requested;

  PAPCFUNC callback;
  HANDLE callback_thread;

  /* out */
  size_t available; /* bytes written, readers only */
  psbool eof; /* readers only */
  psbool error;
  DWORD error_code;
} file_thread_data_t;

typedef struct {
  /* so we can pass this as LPOVERLAPPED; must be at the beginning */
  OVERLAPPED overlap;
  long fd;

  /* for successive calls in the main thread;
     only set and read from there */
  psbool checking; 

  CHAR* buffer;
  CHAR* current;
  size_t current_size, max_size;
} callback_data_t;

typedef struct {
  enum stream_descriptor_type type;

  psbool is_free;

  union {

    struct {
      HANDLE handle;
      DWORD current_offset;
      DWORD current_offset_high;
      psbool eof; /* readers only */
    } file_regular_data;

    struct {
      file_thread_data_t thread_data;
    } file_special_data;
  
    struct {
      SOCKET socket;
      WSABUF wsabuf;
      SOCKET hatched_socket;
      /* callback to set socket hatched by connect/accept  */
      PAPCFUNC hatch_callback;
      /* address returned by recvfrom */
      struct sockaddr_storage recv_address;
      socklen_t recv_address_len;
    } socket_data;
  };

  callback_data_t callback_data;

} stream_descriptor_t;

/* This is the same as DEFAULT-BUFFER-SIZE in rts/port-buffer.scm */
#define FILE_CALLBACK_BUFFER_INITIAL_SIZE 4096

/* If things work smoothly, this will never have to do anything. */

static void
maybe_grow_callback_data_buffer(callback_data_t* callback_data,
				size_t new_max_size)
{
  if (new_max_size <= callback_data->max_size)
    return;

  callback_data->buffer = realloc(callback_data->buffer, new_max_size);
  
  if (callback_data->buffer == NULL)
    {
      fprintf(stderr,
	      "failed to allocate memory for stream buffer\n");
      exit(1);
    }

  callback_data->max_size = new_max_size;
}


#define STREAM_DESCRIPTORS_INITIAL_SIZE 1024

stream_descriptor_t* stream_descriptors;
size_t stream_descriptors_max_size;

static void
setup_stream_descriptor(stream_descriptor_t* stream_descriptor, long fd)
{
  stream_descriptor->is_free = PSTRUE;
  stream_descriptor->callback_data.fd = fd;
  stream_descriptor->callback_data.buffer = NULL;
}


static void
initialize_stream_descriptors(void)
{
  stream_descriptors = malloc(sizeof(stream_descriptor_t) *
			      STREAM_DESCRIPTORS_INITIAL_SIZE);
  if (stream_descriptors == NULL)
    {
      fprintf(stderr,
	      "failed to allocate memory for stream descriptors\n");
      exit(1);
    }
  
  {
    size_t i = 0;
    while (i < STREAM_DESCRIPTORS_INITIAL_SIZE)
      {
	setup_stream_descriptor(&(stream_descriptors[i]), i);
	++i;
      }
  }

  stream_descriptors_max_size = STREAM_DESCRIPTORS_INITIAL_SIZE;
}


/* We might want to do some sort of freelist thing later. */

static void
grow_stream_descriptors(void)
{
  size_t stream_descriptors_previous_max_size = stream_descriptors_max_size;
  stream_descriptors_max_size += STREAM_DESCRIPTORS_INITIAL_SIZE;

  stream_descriptors = realloc(stream_descriptors,
			       stream_descriptors_max_size);
  if (stream_descriptors == NULL)
    {
      fprintf(stderr,
	      "Failed to allocate memory for stream descriptor\n");
      exit(1);
    }

  {
    size_t i = stream_descriptors_previous_max_size;
    while (i < stream_descriptors_max_size)
      {
	setup_stream_descriptor(&(stream_descriptors[i]), i);
	++i;
      }
  }
}

static void
initialize_stream_descriptor(stream_descriptor_t* stream_descriptor,
			     enum stream_descriptor_type type)
{

  stream_descriptor->type = type;
  stream_descriptor->is_free = PSFALSE;

  /* #### this should probably move to the caller */

  switch (type)
    {
    case STREAM_FILE_REGULAR:
      stream_descriptor->file_regular_data.current_offset_high = 0;
      stream_descriptor->file_regular_data.current_offset = 0;
      stream_descriptor->file_regular_data.eof = PSFALSE;
      break;
    }

  stream_descriptor->callback_data.current_size = 0;

  if (stream_descriptor->callback_data.buffer == NULL)
    {
      stream_descriptor->callback_data.current
	= stream_descriptor->callback_data.buffer
	= malloc(FILE_CALLBACK_BUFFER_INITIAL_SIZE);

      if (stream_descriptor->callback_data.buffer == NULL)
	{
	  fprintf(stderr,
		  "Failed to allocate memory for stream buffer\n");
	  exit(1);
	}

      stream_descriptor->callback_data.max_size
	= FILE_CALLBACK_BUFFER_INITIAL_SIZE;
      stream_descriptor->callback_data.current_size = 0;
    }

  stream_descriptor->callback_data.checking = PSFALSE;
}

static void
deallocate_fd(long fd)
{
  stream_descriptors[fd].is_free = PSTRUE;
}

static long
allocate_fd(enum stream_descriptor_type type)
{
  size_t i = 0;

  while ((i < stream_descriptors_max_size)
	 && (!(stream_descriptors[i].is_free)))
      ++i;

  if (i == stream_descriptors_max_size)
    grow_stream_descriptors();

  initialize_stream_descriptor(&(stream_descriptors[i]), type);

  return i;
}


/* Windows allows doing fully asynchronous I/O on files and sockets.
   However, Windows still doesn't fit the Scheme 48 model completely.

   Here are the notable mismatches:

   - Scheme 48 uses longs uniformly to represent stream descriptors.
     Windows uses different types for files and sockets (HANDLE and SOCKET)

   - Scheme 48 reads first, sees that it can't complete immediately,
     queues up that it wants to read, then tries to read again.

     In Windows, you request a read, and a callback will get called
     *with the data*.

   - For writes we're better off because the model of Scheme 48
     actually fits Windows quite closely. */

int
ps_open_fd(char *filename, psbool is_input, long *status)
{
#define FILE_NAME_SIZE 1024
#define PERMISSION 0666   /* read and write for everyone */

  HANDLE handle;
  char filename_temp[FILE_NAME_SIZE];
  char *expanded;
  WCHAR filename_utf16[FILE_NAME_SIZE];
  extern char *s48_expand_file_name(char *, char *, int);

  expanded = s48_expand_file_name(filename, filename_temp, FILE_NAME_SIZE);
  if (expanded == NULL)
    return -1;

  s48_utf_8of16_to_utf_16(expanded, filename_utf16, NULL);

  handle = CreateFileW(filename_utf16,
		       is_input ? GENERIC_READ : GENERIC_WRITE,
		       is_input ? FILE_SHARE_READ : FILE_SHARE_WRITE,
		       NULL,
		       is_input ? OPEN_EXISTING : CREATE_ALWAYS,
		       FILE_ATTRIBUTE_NORMAL | FILE_FLAG_OVERLAPPED,
		       NULL);
  
  if (handle == INVALID_HANDLE_VALUE)
    {
      *status = (long) GetLastError();
      return -1;
    }
  else
    {
      long fd = allocate_fd(STREAM_FILE_REGULAR);
	  stream_descriptor_t* stream_descriptor = &(stream_descriptors[fd]);
	  callback_data_t* callback_data = &(stream_descriptor->callback_data);
      *status = NO_ERRORS;
      /* here, things are always regular */
      stream_descriptor->file_regular_data.handle = handle;
	  callback_data->overlap.Offset = 0;
	  callback_data->overlap.OffsetHigh = 0;
	  
      return fd;
    }
}

/*
 * This is for I/O streams that do not support overlapped I/O or a
 * select-like operation.  (Stdin/out/err don't support overlapped
 * I/O; pipes don't support select.)  The ideas are stolen from
 * MzScheme; the help of Matthew Flatt is gratefully acknowledged.
 *
 * Each such stream has an associated thread which carries out I/O
 * actions on behalf of some other thread.  Upon completion, an
 * asynchronous procedure call to a procedure specified by the caller
 * is registered.
 */

/*
 * This controls the text encoding used.
 */

static BOOL is_STD_INPUT_console,
  is_STD_OUTPUT_console,
  is_STD_ERROR_console;

/* defined and initialized in event.c */
extern HANDLE s48_main_thread;

DWORD WINAPI
reader_thread_proc(LPVOID lpParameter)
{
  DWORD n_read;

  stream_descriptor_t* stream_descriptor = (stream_descriptor_t*)lpParameter;
  callback_data_t* callback_data = &(stream_descriptor->callback_data);
  long fd = callback_data->fd;
  file_thread_data_t* thread_data
    = &(stream_descriptor->file_special_data.thread_data);
  DWORD file_type = GetFileType(thread_data->file_handle);
  BOOL is_console = ((fd == STDIN_FD()) && is_STD_INPUT_console);

  while ((!thread_data->eof) && (!thread_data->error))
    {
      /* this comes from the main thread */
      WaitForSingleObject(thread_data->check_semaphore, INFINITE);

      /* printf("[reader_thread_proc: waited for semaphore]\n"); */

      if (thread_data->abort)
	break;

      /* printf("[reader_thread_proc:want %ld bytes\n", (long) thread_data->requested); */
      
      if (is_console ?
	  ReadConsoleW(thread_data->file_handle, thread_data->buffer,
		       thread_data->requested / 2, &n_read, NULL)
	  : ReadFile(thread_data->file_handle, thread_data->buffer,
		     thread_data->requested, &n_read, NULL))
	{
	  thread_data->error = PSFALSE;
	  /*
	   * Windows reports in terms of characters, not bytes.
	   */
	  thread_data->available = (is_console ? (n_read * 2) : n_read);
	  /* kludge: pressing Ctrl-C looks like EOF on stdin */
	  if ((n_read == 0)  && (file_type != FILE_TYPE_CHAR))
	    thread_data->eof = PSTRUE;
	}
      else
	{
	  thread_data->error = PSTRUE;
	  thread_data->error_code = GetLastError();
	}

      /* printf("[reader_thread_proc:got %ld bytes]\n", (long) n_read); */
      
      /* notify */
       if (!QueueUserAPC(thread_data->callback,
			thread_data->callback_thread,
			(DWORD) stream_descriptor))

	{
	  fprintf(stderr, "QueueUserAPC failed\n");
	  exit(-1);
	}
    }

  /* clean up */
  if (CloseHandle(thread_data->check_semaphore))
    thread_data->error = PSFALSE;
  else
    {
      thread_data->error = PSTRUE;
      thread_data->error_code = GetLastError();
    }
  /* notify */
  ReleaseSemaphore(thread_data->abort_semaphore, 1, NULL);
  ExitThread(0);
  return 0;
}

/* #### most of this is the same as reader_thread_proc #### */

DWORD WINAPI
writer_thread_proc(LPVOID lpParameter)
{
  DWORD n_written;

  stream_descriptor_t* stream_descriptor = (stream_descriptor_t*)lpParameter;
  callback_data_t* callback_data = &(stream_descriptor->callback_data);
  long fd = callback_data->fd;
  file_thread_data_t* thread_data
    = &(stream_descriptor->file_special_data.thread_data);

  BOOL is_console = (((fd == STDOUT_FD()) && is_STD_OUTPUT_console)
		     || ((fd == STDERR_FD()) && is_STD_ERROR_console));

  while (!thread_data->error)
    {
      /* this comes from the main thread */
      WaitForSingleObject(thread_data->check_semaphore, INFINITE);

      /* printf("[writer_thread_proc: waited for semaphore]\n"); */

      if (thread_data->abort)
	break;

      /* printf("[writer_thread_proc:want %ld bytes\n", (long) thread_data->requested); */
      
      if (is_console ?
	  WriteConsoleW(thread_data->file_handle, thread_data->buffer,
			thread_data->requested / 2, &n_written, NULL)
	  : WriteFile(thread_data->file_handle, thread_data->buffer,
		      thread_data->requested, &n_written, NULL))
	{
	  thread_data->error = PSFALSE;
	  /*
	   * Windows reports in terms of characters, not bytes.
	   */
	  thread_data->available = (is_console ? (n_written * 2) : n_written);
	}
      else
	{
	  thread_data->error = PSTRUE;
	  thread_data->error_code = GetLastError();
	}
      
      /* printf("[writer_thread_proc:got %ld bytes]\n", (long) n_written); */
      

      /* notify */
      if (!QueueUserAPC(thread_data->callback,
			thread_data->callback_thread,
			(DWORD) stream_descriptor))

	{
	  fprintf(stderr, "QueueUserAPC failed\n");
	  exit(-1);
	}
    }

  /* clean up */
  if (CloseHandle(thread_data->check_semaphore))
      thread_data->error = PSFALSE;
  else
    {
      thread_data->error = PSTRUE;
      thread_data->error_code = GetLastError();
    }
  /* notify */
  ReleaseSemaphore(thread_data->abort_semaphore, 1, NULL);
  ExitThread(0);
  return 0;
}

extern HANDLE s48_create_mutex_semaphore();

static void
start_reader_slash_writer_thread(HANDLE file_handle,
				 stream_descriptor_t* stream_descriptor,
				 LPTHREAD_START_ROUTINE thread_proc)
{
  file_thread_data_t* thread_data = &(stream_descriptor->file_special_data.thread_data);
  HANDLE thread_handle;
  DWORD id;

  thread_data->abort = PSFALSE;

  thread_data->available = 0;

  thread_data->error = PSFALSE;
  thread_data->eof = PSFALSE;
  
  thread_data->file_handle = file_handle;
  thread_data->check_semaphore = s48_create_mutex_semaphore();
  thread_data->abort_semaphore = s48_create_mutex_semaphore();

  thread_handle = CreateThread(NULL, /* lpThreadAttributes */
			       4096, /* dwStackSize, */
			       (LPTHREAD_START_ROUTINE)thread_proc,
			       stream_descriptor,
			       0, /* dwCreationFlags, */
			       &id);
  if (thread_handle == NULL)
    {
      fprintf(stderr, "CreateThread failed in start_reader_slash_writer_thread\n");
      free(thread_data);
      exit(-1);
    }

  thread_data->thread_handle = thread_handle;
}


static void
open_special_fd(HANDLE handle, size_t fd,
		psbool is_input, BOOL is_redirected)
{
  stream_descriptor_t* stream_descriptor;

  if (fd >= stream_descriptors_max_size)
    grow_stream_descriptors();

  if (!(stream_descriptors[fd].is_free))
    {
      fprintf(stderr, "fd %d isn't available\n", fd);
      exit(-1);
    }

  stream_descriptor = &(stream_descriptors[fd]);

  initialize_stream_descriptor(stream_descriptor, STREAM_FILE_SPECIAL);
  
  start_reader_slash_writer_thread(handle,
				   stream_descriptor,
				   is_input
				   ? (LPTHREAD_START_ROUTINE)reader_thread_proc
				   : (LPTHREAD_START_ROUTINE)writer_thread_proc);
}

int
ps_close_fd(long fd)
{
  stream_descriptor_t* stream_descriptor = &(stream_descriptors[fd]);
  int status;

  switch (stream_descriptor->type)
    {
    case STREAM_FILE_REGULAR:
      {
	HANDLE handle = stream_descriptor->file_regular_data.handle;

	if (CloseHandle(handle))
	  status = NO_ERRORS;
	else
	  status = (int) GetLastError();
	break;
      }
    case STREAM_FILE_SPECIAL:
      {
	file_thread_data_t* thread_data =
	  &(stream_descriptor->file_special_data.thread_data);

	thread_data->abort = PSTRUE;
	ReleaseSemaphore(thread_data->check_semaphore, 1, NULL);
	/* wait for completion */
	WaitForSingleObject(thread_data->abort_semaphore, INFINITE);

	status = thread_data->error ? thread_data->error_code : NO_ERRORS;
	break;
      }
    case STREAM_SOCKET:
      {
	SOCKET socket = stream_descriptor->socket_data.socket;

	if (closesocket(socket) == 0)
	  status = NO_ERRORS;
	else
	  status = (int) WSAGetLastError();
	break;
      }
    }
  deallocate_fd(fd);
  return status;
}

extern psbool s48_is_pending(long);
extern void s48_add_ready_fd(long, psbool, psbool, long);

/* This is called as the result of a completed read operation; either
   from the overlapped I/O completion routine, or from the callback
   from the reader thread. */

static void
read_done(DWORD dwErr,
	  size_t bytes_read,
	  stream_descriptor_t* stream_descriptor)
{
  callback_data_t* callback_data = &(stream_descriptor->callback_data);
  long fd = callback_data->fd;

  /* ### need to do offset_high as well */
  stream_descriptor->callback_data.current_size = bytes_read;
  stream_descriptor->callback_data.current = 
    stream_descriptor->callback_data.buffer;

  if (stream_descriptor->type == STREAM_FILE_REGULAR)
    {
      stream_descriptor->file_regular_data.current_offset += bytes_read;
      /* #### need to do offset_high as well */
      stream_descriptor->file_regular_data.eof 
	= ((dwErr == ERROR_HANDLE_EOF) ? PSTRUE : PSFALSE);
    }

  if (callback_data->checking) /* ps_check_fd */
    callback_data->checking = PSFALSE;
    
  if (s48_is_pending(fd))
    {
      if ((dwErr != 0) && (dwErr != ERROR_HANDLE_EOF))
	s48_add_ready_fd(fd, PSTRUE, PSTRUE, dwErr);
      else
	s48_add_ready_fd(fd, PSTRUE, PSFALSE, (long)0); /* *not* bytes_read */
    }
}

/* for regular files; from overlapped I/O */
static VOID WINAPI
read_completed(DWORD dwErr, DWORD cbBytesRead, LPOVERLAPPED lpOverLap)
{
  callback_data_t* callback_data = (callback_data_t*) lpOverLap;
  long fd = callback_data->fd;
  stream_descriptor_t* stream_descriptor = &(stream_descriptors[fd]);

  read_done(dwErr, (size_t)cbBytesRead, stream_descriptor);
}

/* for special files; from QueueUserAPC */
static VOID CALLBACK
read_callback(DWORD dwParam)
{
  stream_descriptor_t* stream_descriptor = (stream_descriptor_t*)dwParam;
  file_thread_data_t* thread_data
    = &(stream_descriptor->file_special_data.thread_data);

  read_done(thread_data->error ? thread_data->error_code : NO_ERRORS,
	    (size_t)thread_data->available,
	    stream_descriptor);
}

/* from sockets */
static VOID CALLBACK
recv_completed(DWORD dwErr, DWORD cbBytesRead, LPOVERLAPPED lpOverLap, DWORD dwFlags)
{
  read_completed(dwErr, cbBytesRead, lpOverLap);
}

psbool
ps_check_fd(long fd, psbool is_read, long *status)
{
  stream_descriptor_t* stream_descriptor = &(stream_descriptors[fd]);
  callback_data_t* callback_data = &(stream_descriptor->callback_data);

  *status = NO_ERRORS;

  if (!is_read)
    /* conservative */
    return stream_descriptor->callback_data.current_size == 0;

  /* from now on, we know is_read is true */
  if (stream_descriptor->callback_data.current_size > 0)
    /* the buffer is non-empty */
    return PSTRUE;

  switch (stream_descriptor->type) {
  case STREAM_FILE_REGULAR:
    {
      /* we might consider always returning true here */
      HANDLE handle = stream_descriptor->file_regular_data.handle;

      callback_data->overlap.Offset = stream_descriptor->file_regular_data.current_offset;
      callback_data->overlap.OffsetHigh = stream_descriptor->file_regular_data.current_offset_high;

      callback_data->checking = PSTRUE;

      if (ReadFileEx(handle,
		     (LPVOID)callback_data->buffer,
		     (DWORD)1,
		     (LPOVERLAPPED)callback_data,
		     read_completed))
	return PSFALSE;
      else
	{
	  DWORD last_error = GetLastError();

	  if (last_error != ERROR_HANDLE_EOF)
	    *status = (int) last_error;
	  return PSTRUE;
	}
    }
  case STREAM_FILE_SPECIAL:
    {
      file_thread_data_t* thread_data = &(stream_descriptor->file_special_data.thread_data);

      if (thread_data->eof)
	return PSTRUE;

      if (thread_data->error)
	{
	  *status = thread_data->error_code;
	  return PSTRUE;
	}
	
      if (callback_data->checking)
	return PSFALSE;

      /* get the reader thread started */
      callback_data->checking = PSTRUE;

      thread_data->buffer = stream_descriptor->callback_data.buffer;
      thread_data->requested = 1;
      thread_data->callback = read_callback;
      thread_data->callback_thread = s48_main_thread;
      ReleaseSemaphore(thread_data->check_semaphore, 1, NULL);

      return PSFALSE;
    }
  case STREAM_SOCKET:
    {
      SOCKET socket = stream_descriptor->socket_data.socket;
      DWORD numberOfBytesRecvd;
      DWORD flags = 0;
      int wsa_status;
      int block;

      /* just making sure */
      callback_data->overlap.Offset = 0;
      callback_data->overlap.OffsetHigh = 0;

      stream_descriptor->socket_data.wsabuf.len = 1;
      stream_descriptor->socket_data.wsabuf.buf = (char*)callback_data->buffer;

      /* just making sure */
      callback_data->overlap.Offset = 0;
      callback_data->overlap.OffsetHigh = 0;
      callback_data->overlap.hEvent = NULL; /* this is hopefully invalid */

      block = 1; /* non-blocking */
      ioctlsocket(socket, FIONBIO, &block);
      
      wsa_status = WSARecv(socket,
			   (LPWSABUF)&stream_descriptor->socket_data.wsabuf,
			   1,
			   &numberOfBytesRecvd,
			   &flags,
			   (LPOVERLAPPED)callback_data,
			   recv_completed);

	if ((wsa_status == 0)
	    && (numberOfBytesRecvd == 0))
	  return PSTRUE;

      
      if ((wsa_status == 0)
	  || (WSAGetLastError() == WSA_IO_PENDING))
	return PSFALSE;
      else
	{
	  *status = WSAGetLastError();
	  return 0;
	}
    }
  }
  return PSFALSE; /* shouldn't happen */
}



/* waitp doesn't mean it should wait---it only means that the read
   shouldn't be registered anywhere if this is false.
   
   If EOF has been reached (i.e. no characters can be read),
   *eofp is set.

   If the read operation is queued up for asynchronous completion,
   *pending is set.

   Returns number of characters actually read.  (This may be 0.)
*/

long
ps_read_fd(long fd, char *buffer, long max, psbool waitp,
	   psbool *eofp, psbool *pending, long *status)
{
  stream_descriptor_t* stream_descriptor = &(stream_descriptors[fd]);
  callback_data_t* callback_data = &(stream_descriptor->callback_data);

  /* for the normal return */
  *eofp = PSFALSE;
  *pending = PSFALSE;
  *status = NO_ERRORS;

  if (callback_data->checking) /* ps_check_fd is active */
    return 0;

  /* there's still stuff in the buffer */
  if (callback_data->current_size > 0)
    {
      /* we want more than what's in the buffer */
      if (max >= (long)callback_data->current_size)
	{
	  size_t size = callback_data->current_size;
	  memcpy(buffer, callback_data->current, size);
	  callback_data->current_size = 0;
	  callback_data->current = stream_descriptor->callback_data.buffer;
	  return size;
	}
      else
	/* less; shouldn't happen */
	{
	  memcpy(buffer, callback_data->current, max);
	  callback_data->current += max;
	  callback_data->current_size -= max;
	  return max;
	}
    }

  switch (stream_descriptor->type)
    {
    case STREAM_FILE_REGULAR:
      {
	HANDLE handle = stream_descriptor->file_regular_data.handle;

	if (stream_descriptor->file_regular_data.eof)
	  {
	    *eofp = PSTRUE;
	    return 0;
	  }
	
	/* There's nothing in the buffer---need to do an actual read. */
	maybe_grow_callback_data_buffer(callback_data, (size_t)max);
	
	callback_data->overlap.Offset = stream_descriptor->file_regular_data.current_offset;
	callback_data->overlap.OffsetHigh = stream_descriptor->file_regular_data.current_offset_high;
	
	if (ReadFileEx(handle,
		       (LPVOID)callback_data->buffer,
		       (DWORD)max,
		       (LPOVERLAPPED)callback_data,
		       read_completed))
	  {
	    /* success */
	    if (waitp)
	      {
		if (!s48_add_pending_fd(fd, PSTRUE))
		  *status = ERROR_OUTOFMEMORY;
		else
		  *pending = PSTRUE;
	      }
	    return 0;
	  }
	else
	  {
	    DWORD last_error = GetLastError();
	    
	    if (last_error == ERROR_HANDLE_EOF)
	      {
		*eofp = PSTRUE;
		return 0;
	      }
	    
	    /* failure */
	    *status = (int) last_error;
	    return 0;
	  }
      }
    case STREAM_FILE_SPECIAL:
      {
	file_thread_data_t* thread_data =
	  &(stream_descriptor->file_special_data.thread_data);
	
	if (thread_data->eof)
	  {
	    *eofp = PSTRUE;
	    return 0;
	  }
	
	/* There's nothing in the buffer---need to do an actual read. */
	maybe_grow_callback_data_buffer(callback_data, (size_t)max);
	
	if (waitp)
	  {
	    if (!s48_add_pending_fd(fd, PSTRUE))
	      {
		*status = ERROR_OUTOFMEMORY;
		return 0;
	      }
	    thread_data->buffer = callback_data->buffer;
	    thread_data->requested = max;
	    thread_data->callback_thread = s48_main_thread;
	    thread_data->callback = read_callback;
	    ReleaseSemaphore(thread_data->check_semaphore, 1, NULL);
	    *pending = PSTRUE;
	  }
	return 0;
      }
    case STREAM_SOCKET:
      {
	SOCKET socket = stream_descriptor->socket_data.socket;
	DWORD numberOfBytesRecvd;
	DWORD flags = 0;
	int wsa_status;
	int block;

	/* There's nothing in the buffer---need to do an actual read. */
	maybe_grow_callback_data_buffer(callback_data, (size_t)max);

	/* just making sure */
	callback_data->overlap.Offset = 0;
	callback_data->overlap.OffsetHigh = 0;

	stream_descriptor->socket_data.wsabuf.len = max;
	stream_descriptor->socket_data.wsabuf.buf = (char*)callback_data->buffer;

	block = 1; /* non-blocking */
	ioctlsocket(socket, FIONBIO, &block);

	wsa_status = WSARecv(socket,
			     (LPWSABUF)&stream_descriptor->socket_data.wsabuf,
			     1,
			     &numberOfBytesRecvd,
			     &flags,
			     (LPOVERLAPPED)callback_data,
			     recv_completed);

	if ((wsa_status == 0)
	    && (numberOfBytesRecvd == 0))
	    *eofp = PSTRUE;

	if ((wsa_status == 0)
	    || (WSAGetLastError() == WSA_IO_PENDING))
	  {
	    if (waitp)
	      {
		if (!s48_add_pending_fd(fd, PSTRUE))
		  *status = ERROR_OUTOFMEMORY;
		else
		  *pending = PSTRUE;
	      }
	    return 0;
	  }
	else
	  {
	    *status = WSAGetLastError();
	    return 0;
	  }
      }
    }
  return 0; /* shouldn't happen */
}

static VOID WINAPI
write_done(DWORD dwErr,
	   size_t bytes_written,
	   stream_descriptor_t* stream_descriptor) 
{
  callback_data_t* callback_data = &(stream_descriptor->callback_data);
  long fd = callback_data->fd;

  if ((bytes_written != 0) && (s48_is_pending(fd)))
    {
      switch (stream_descriptor->type) {
      case STREAM_FILE_REGULAR:
	{
	  stream_descriptor->file_regular_data.current_offset += bytes_written;
	  /* ### need to do offset_high as well */
	  break;
	}
      }
      if (dwErr != 0)
	s48_add_ready_fd(fd, PSFALSE, PSTRUE, dwErr);
      else
	s48_add_ready_fd(fd, PSFALSE, PSFALSE, (long)bytes_written);
    }
}

/* for regular files; from overlapped I/O */
static VOID WINAPI
write_completed(DWORD dwErr, DWORD cbWritten, LPOVERLAPPED lpOverLap) 
{
  callback_data_t* callback_data = (callback_data_t*) lpOverLap;
  long fd = callback_data->fd;
  stream_descriptor_t* stream_descriptor = &(stream_descriptors[fd]);

  write_done(dwErr, (size_t)cbWritten, stream_descriptor);
}

/* for special files; from QueueUserAPC */
static VOID CALLBACK
write_callback(DWORD dwParam)
{
  stream_descriptor_t* stream_descriptor = (stream_descriptor_t*)dwParam;
  file_thread_data_t* thread_data
    = &(stream_descriptor->file_special_data.thread_data);

  write_done(thread_data->error ? thread_data->error_code : 0,
	     (size_t)thread_data->available,
	     stream_descriptor);
}

/* for sockets */
static VOID CALLBACK
send_completed(DWORD dwErr, DWORD cbTransferred, LPOVERLAPPED lpOverLap, DWORD dwFlags)
{
  write_completed(dwErr, cbTransferred, lpOverLap);
}

long
ps_write_fd(long fd, char *buffer, long max, psbool *pending, long *status)
{
  stream_descriptor_t* stream_descriptor = &(stream_descriptors[fd]);
  callback_data_t* callback_data = &(stream_descriptor->callback_data);

  maybe_grow_callback_data_buffer(callback_data, (size_t)max);

  memcpy(callback_data->buffer, buffer, (size_t)max);

  switch (stream_descriptor->type)
    {
    case STREAM_FILE_REGULAR:
      {
	HANDLE handle = stream_descriptor->file_regular_data.handle;

	callback_data->overlap.Offset = stream_descriptor->file_regular_data.current_offset;
	callback_data->overlap.OffsetHigh = stream_descriptor->file_regular_data.current_offset_high;

	if (WriteFileEx(handle,
			(LPVOID)callback_data->buffer,
			(DWORD)max,
			(LPOVERLAPPED)callback_data,
			write_completed))
	  {
	    if (!s48_add_pending_fd(fd, PSFALSE))
	      {
		*status = ERROR_OUTOFMEMORY;
		return 0;
	      }
	    *pending = PSTRUE;
	    *status = NO_ERRORS;
	  }
	else
	  {
	    *pending = PSFALSE;
	    *status = (int) GetLastError();
	  }
	return 0; /* we always wait for the callback */
      }
    case STREAM_FILE_SPECIAL:
      {
	file_thread_data_t* thread_data =
	  &(stream_descriptor->file_special_data.thread_data);
	thread_data->buffer = callback_data->buffer;
	thread_data->requested = max;
	thread_data->callback_thread = s48_main_thread;
	thread_data->callback = write_callback;
	ReleaseSemaphore(thread_data->check_semaphore, 1, NULL);
	if (!s48_add_pending_fd(fd, PSFALSE))
	  {
	    *status = ERROR_OUTOFMEMORY;
	    return 0;
	  }
	*pending = PSTRUE;
	*status = NO_ERRORS;
	return 0; /* the thread needs to do the work */
      }
    case STREAM_SOCKET:
      {
	SOCKET socket = stream_descriptor->socket_data.socket;
	DWORD numberOfBytesSent;
	DWORD flags = 0;
	int wsa_status;

	/* just making sure */
	callback_data->overlap.Offset = 0;
	callback_data->overlap.OffsetHigh = 0;

	stream_descriptor->socket_data.wsabuf.len = max;
	stream_descriptor->socket_data.wsabuf.buf = (char*)callback_data->buffer;

	wsa_status = WSASend(socket,
			     (LPWSABUF)&stream_descriptor->socket_data.wsabuf,
			     1,
			     &numberOfBytesSent,
			     flags,
			     (LPOVERLAPPED)callback_data,
			     send_completed);

	if ((wsa_status == 0)
	    | (WSAGetLastError() == WSA_IO_PENDING))
	  {
	    /* success */
	    if (!s48_add_pending_fd(fd, PSFALSE))
	      {
		*status = ERROR_OUTOFMEMORY;
		return 0;
	      }
	    *pending = PSTRUE;
	    *status = NO_ERRORS;
	  }
	else
	  {
	    *pending = PSFALSE;
	    *status = (int) WSAGetLastError();
	  }
	return 0; /* we always wait for the callback */
      }
    }
  return 0; /* shouldn't happen */
}

long
ps_io_buffer_size(void)
{
  return 4096;
}

psbool
ps_io_crlf_p(void)
{
  return PSTRUE;
}

char *
ps_console_encoding(long fd_as_long)
{
  if (fd_as_long == STDIN_FD())
    return is_STD_INPUT_console ? "UTF-16LE" : "ISO8859-1";
  else if (fd_as_long == STDOUT_FD())
    return is_STD_OUTPUT_console ? "UTF-16LE" : "ISO8859-1";
  else if (fd_as_long == STDERR_FD())
    return is_STD_ERROR_console ? "UTF-16LE" : "ISO8859-1";
  else
    return NULL;
}

long
ps_abort_fd_op(long fd_as_long)
{
  int fd = (int)fd_as_long;
  stream_descriptor_t* stream_descriptor = &(stream_descriptors[fd]);
  psbool pending_p = s48_is_pending(fd);

  if (!s48_remove_fd(fd))
    fprintf(stderr, "Error: ps_abort_fd_op, no pending operation on fd %d\n",
	            fd);

  switch (stream_descriptor->type)
    {
    case STREAM_FILE_REGULAR:
      {
	HANDLE handle = stream_descriptor->file_regular_data.handle;
	CancelIo(handle);

	/* flush any pending completion routines */
	if (pending_p)
	  while (SleepEx(0, TRUE) == IO_COMPLETION_EVENT)
	    ;

	break;
      }
    case STREAM_SOCKET:
      {
	SOCKET socket = stream_descriptor->socket_data.socket;
	CancelIo((HANDLE) socket);

	/* flush any pending completion routines */
	if (pending_p)
	  while (SleepEx(0, TRUE) == IO_COMPLETION_EVENT)
	    ;

	break;
      }
    }

  return 0;      /* because we do not actually do any I/O in parallel the
		    status is always zero: no characters transferred. */
}

/*
 * This checks that fd's destined for output channels are marked
 * as nonblocking.  Stdout and stderr are a special case because
 * we do not want to screw up the shell, if we were invoked from
 * one.
 */

s48_ref_t
s48_add_channel_2(s48_call_t call, s48_ref_t mode, s48_ref_t id, long fd)
{
  /* back to the VM */
  return s48_make_local_ref(call, s48_really_add_channel(s48_deref(mode), s48_deref(id), fd));
}

s48_ref_t
s48_set_channel_os_index_2(s48_call_t call, s48_ref_t channel, long fd)
{
  /* back to the VM */
  return s48_make_local_ref(call, s48_set_channel_os_index(s48_deref(channel), fd));
}

void
s48_fd_io_init()
{
  DWORD numberOfEventsRead; /* dummy, actually */
  CONSOLE_CURSOR_INFO consoleCursorInfo; /* dummy, too */
  HANDLE handle;

  initialize_stream_descriptors();

  /* these Unix-style indices are hard-wired into the VM */
  handle = GetStdHandle(STD_INPUT_HANDLE);
  /*
   * PeekConsoleInput returns non-zero if we're indeed looking at a
   * console, zero if a handle has been redirected.  We need this info
   * to decide whether to use the console I/O functions and Unicode.
   */
  is_STD_INPUT_console = PeekConsoleInput(handle, NULL, 0, &numberOfEventsRead);
  open_special_fd(handle, 0, PSTRUE, is_STD_INPUT_console);
  handle = GetStdHandle(STD_OUTPUT_HANDLE);
  /*
   * Similarly for GetConsoleCursorInfo.
   */
  is_STD_OUTPUT_console = GetConsoleCursorInfo(handle, &consoleCursorInfo);
  open_special_fd(handle, 1, PSFALSE, is_STD_OUTPUT_console);
  handle = GetStdHandle(STD_ERROR_HANDLE);
  is_STD_ERROR_console = GetConsoleCursorInfo(handle, &consoleCursorInfo);
  open_special_fd(handle, 2, PSFALSE, is_STD_ERROR_console);
}

/*
 * An interface to Windows Sockets 2
 */

/*
 * Windows Sockets 2 is a mumbojumbo of Berkeley sockets, and
 * Microsoft's own stuff thrown in.  Of course, the two halves use
 * different naming conventions, and things are generally confusing.
 * Moreover, socket handles and file handles are distinct (at least
 * partially), so we can't use the file operations we want on sockets.
 */

socket_t
s48_extract_socket_fd(s48_call_t call, s48_ref_t sch_channel)
{
  s48_check_channel_2(call, sch_channel);
  {
    long socket_fd
      = s48_unsafe_extract_long_2(call, s48_unsafe_channel_os_index_2(call, sch_channel));
    stream_descriptor_t* stream_descriptor = &(stream_descriptors[socket_fd]);
    callback_data_t* callback_data = &(stream_descriptor->callback_data);
    socket_t socket = stream_descriptor->socket_data.socket;
    return socket;
  }
}

static s48_ref_t
s48_socket(s48_call_t call, s48_ref_t sch_af, s48_ref_t sch_type, s48_ref_t sch_protocol)
{
  long fd;
  int af = s48_extract_af(call, sch_af);
  int socktype = s48_extract_socket_type(call, sch_type);
  int protocol = s48_extract_long_2(call, sch_protocol);
  s48_ref_t channel;
  stream_descriptor_t* stream_descriptor;

  SOCKET socket = WSASocket(af, socktype, protocol,
			    NULL,
			    0, /* reserved */
			    WSA_FLAG_OVERLAPPED);

  if (socket == INVALID_SOCKET)
    s48_os_error_2(call, "s48_socket", WSAGetLastError(), 3,
		   sch_af, sch_type, sch_protocol);

  fd = allocate_fd(STREAM_SOCKET);
  stream_descriptor = &(stream_descriptors[fd]);
  stream_descriptor->socket_data.socket = socket;

  channel = s48_add_channel_2(call, s48_channel_status_special_input_2(call),
			      s48_enter_string_latin_1_2(call, "socket"), fd);

  if (!s48_channel_p_2(call, channel))
    {
      ps_close_fd(fd);
      s48_raise_scheme_exception_2(call, s48_extract_long_2(call, channel), 0);
    }

  return channel;
}

#define RETRY_OR_RAISE_NEG(C, STATUS, CALL)			\
do {								\
    STATUS = (CALL);						\
    if (STATUS == SOCKET_ERROR)					\
      s48_os_error_2(C, NULL, WSAGetLastError(), 0);		\
 } while (0)

static s48_ref_t
s48_socketpair(s48_call_t call, s48_ref_t sch_af, s48_ref_t sch_type, s48_ref_t sch_protocol)
{
  int status;
  s48_ref_t sch_channel0, sch_channel1;
  s48_ref_t sch_result;
  int af = s48_extract_af(call, sch_af);
  int socktype = s48_extract_socket_type(call, sch_type);
  int protocol = s48_extract_long_2(call, sch_protocol);
  socket_t listener, socket0, socket1;
  long fd0, fd1;
  stream_descriptor_t* stream_descriptor;
  struct sockaddr_in addr;
  socklen_t salen = sizeof(addr);

  listener = WSASocket(af, socktype, protocol,
		      NULL,
		      0, /* reserved */
		      WSA_FLAG_OVERLAPPED);
  if (listener == INVALID_SOCKET)
    s48_os_error_2(call, "s48_socketpair", WSAGetLastError(), 3,
		   sch_af, sch_type, sch_protocol);
  
  addr.sin_family = AF_INET;
  addr.sin_port = 0;
  addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);

  RETRY_OR_RAISE_NEG(call, status, bind(listener, (struct sockaddr*)&addr, sizeof(addr)));
  RETRY_OR_RAISE_NEG(call, status, listen(listener, 1));
  RETRY_OR_RAISE_NEG(call, status,
		     getsockname(listener, (struct sockaddr*)&addr, &salen));

  socket1 = WSASocket(af, socktype, protocol,
		      NULL,
		      0, /* reserved */
		      WSA_FLAG_OVERLAPPED);
  if (socket1 == INVALID_SOCKET)
    s48_os_error_2(call, "s48_socketpair", WSAGetLastError(), 3,
		   sch_af, sch_type, sch_protocol);
  
  RETRY_OR_RAISE_NEG(call, status, connect(socket1, (struct sockaddr*) &addr, salen));
  socket0 = accept(listener, (struct sockaddr*) &addr, &salen);
  if (socket0 == INVALID_SOCKET)
    s48_os_error_2(call, "s48_socketpair", WSAGetLastError(), 3,
		   sch_af, sch_type, sch_protocol);
  
  closesocket(listener);

  fd0 = allocate_fd(STREAM_SOCKET);
  stream_descriptor = &(stream_descriptors[fd0]);
  stream_descriptor->socket_data.socket = socket0;

  fd1 = allocate_fd(STREAM_SOCKET);
  stream_descriptor = &(stream_descriptors[fd1]);
  stream_descriptor->socket_data.socket = socket1;

  sch_channel0 = s48_add_channel_2(call, s48_channel_status_input_2(call),
				   s48_enter_string_latin_1_2(call, "socket"), fd0);
  sch_channel1 = s48_add_channel_2(call, s48_channel_status_input_2(call),
				   s48_enter_string_latin_1_2(call, "socket"), fd1);
  sch_result = s48_cons_2(call, sch_channel0, sch_channel1);
  return sch_result;
}

/*
 * dup() `socket_fd' and return an output channel holding the result.
 *
 * We have two versions, one to be called from C and one to be called
 * from Scheme.
 */

static s48_ref_t
dup_socket_channel(s48_call_t call, long socket_fd)
{
  long output_fd;
  SOCKET output_socket;
  s48_ref_t output_channel;
  stream_descriptor_t* stream_descriptor = &(stream_descriptors[socket_fd]);
  callback_data_t* callback_data = &(stream_descriptor->callback_data);
  SOCKET socket = stream_descriptor->socket_data.socket;
  stream_descriptor_t* output_stream_descriptor;
  int buf_size;

  WSAPROTOCOL_INFO protocolInfo;
  if (WSADuplicateSocket(socket,
			 GetCurrentProcessId(),
			 &protocolInfo)
      == SOCKET_ERROR)
    s48_os_error_2(call, NULL, WSAGetLastError(), 1,
		   s48_enter_long_2(call, socket_fd));

  output_socket = WSASocket(AF_INET,
			    SOCK_STREAM,
			    0, /* protocol */
			    &protocolInfo,
			    0, /* reserved */
			    WSA_FLAG_OVERLAPPED);
  if (output_socket == INVALID_SOCKET)
    s48_os_error_2(call, NULL, WSAGetLastError(), 1,
		   s48_enter_long_2(call, socket_fd));
  
  buf_size = 0;
  setsockopt(output_socket,
	     SOL_SOCKET, SO_SNDBUF, 
	     (const char*)buf_size,
	     sizeof(buf_size));

  output_fd = allocate_fd(STREAM_SOCKET);
  output_stream_descriptor = &(stream_descriptors[output_fd]);
  output_stream_descriptor->socket_data.socket = output_socket;

  output_channel = s48_add_channel_2(call, s48_channel_status_output_2(call),
				     s48_enter_string_latin_1_2(call, "socket connection"),
				     output_fd);
  
  if (!s48_channel_p_2(call, output_channel))
    {
      ps_close_fd(output_fd);
      s48_raise_scheme_exception_2(call, s48_extract_long_2(call, output_channel), 0);
    }

  return output_channel;
}

static s48_ref_t
s48_dup_socket_channel(s48_call_t call, s48_ref_t channel)
{
  int socket_fd;

  s48_check_channel_2(call, channel);
  socket_fd = s48_unsafe_extract_long_2(call, s48_unsafe_channel_os_index_2(call, channel));
  
  return dup_socket_channel(call, socket_fd);
}

/*
 * Given a bound socket, accept a connection and return a pair of the
 * input channel and the raw socket address.
 *
 * If the accept fails because the client hasn't connected yet, then we
 * return #f.
 *
 * If it fails for any other reason, then an exception is raised.
 */

/*
 * This guy finally registers the completed accept.
 */

static VOID CALLBACK
accept_callback(DWORD dwParam)
{
  stream_descriptor_t* stream_descriptor = (stream_descriptor_t*)dwParam;
  callback_data_t* callback_data = &(stream_descriptor->callback_data);

  s48_add_ready_fd(callback_data->fd, PSTRUE, PSFALSE, (long)0);
}

/*
 *  This guy just waits for the event triggered by an accept,
 *  and notifies the VM.
 */

static DWORD WINAPI
hatch_thread_proc(LPVOID lpParameter)
{
  stream_descriptor_t* stream_descriptor = (stream_descriptor_t*)lpParameter;
  callback_data_t* callback_data = &(stream_descriptor->callback_data);

  WaitForSingleObject(callback_data->overlap.hEvent, INFINITE);

  if (!QueueUserAPC(stream_descriptor->socket_data.hatch_callback,
		    s48_main_thread,
		    (DWORD)stream_descriptor))
    {
      fprintf(stderr, "QueueUserAPC failed\n");
      exit(-1);
    }
  return 0;
}

static s48_ref_t
s48_accept(s48_call_t call, s48_ref_t channel, s48_ref_t retry_p)
{
  long socket_fd;
  BOOL status;
  s48_ref_t input_channel;
  stream_descriptor_t* stream_descriptor;
  callback_data_t* callback_data;
  SOCKET socket;
  SOCKET accept_socket;
  DWORD bytecount;

  s48_check_channel_2(call, channel);
  socket_fd = s48_unsafe_extract_long_2(call, s48_unsafe_channel_os_index_2(call, channel));
  stream_descriptor = &(stream_descriptors[socket_fd]);
  callback_data = &(stream_descriptor->callback_data);
  socket = stream_descriptor->socket_data.socket;

  if (s48_false_p_2(call, retry_p))
    /* first time */
    {
      callback_data->overlap.Offset = 0; /* just making sure */
      callback_data->overlap.OffsetHigh = 0;
      callback_data->overlap.hEvent = CreateEvent(NULL, /* no security attributes */
						  TRUE, /* manual reset */
						  FALSE, /* nonsignaled */
						  NULL); /* name */
      
      accept_socket = WSASocket(AF_INET,
				SOCK_STREAM,
				0, /*  protocol */
				NULL,
				0, /* reserved */
				WSA_FLAG_OVERLAPPED);
      if (accept_socket == INVALID_SOCKET)
	s48_os_error_2(call, "s48_accept", WSAGetLastError(), 2,
		       channel, retry_p);
      
      stream_descriptor->socket_data.hatched_socket = accept_socket;

#define SOCKADDR_BUFFER_SIZE (sizeof(struct sockaddr_storage) + 16)

      maybe_grow_callback_data_buffer(callback_data, SOCKADDR_BUFFER_SIZE * 2);
      
      status = AcceptEx(socket,
			accept_socket,
			callback_data->buffer,
			0,
			SOCKADDR_BUFFER_SIZE,
			SOCKADDR_BUFFER_SIZE,
			&bytecount,
			&callback_data->overlap);
    }
  else
    /* rebound */
    {
      DWORD numberOfBytesTransferred;
      if (GetOverlappedResult((HANDLE)socket,
			      &callback_data->overlap,
			      &numberOfBytesTransferred,
			      FALSE)) /* don't block */
	{
	  accept_socket = stream_descriptor->socket_data.hatched_socket;
	  status = TRUE;
	}
      else if (GetLastError() == ERROR_IO_INCOMPLETE)
	return s48_false_2(call); /* still not done */
      else
	s48_os_error_2(call, "s48_accept", WSAGetLastError(), 2,
		       channel, retry_p);
    } 
			 
  /*
   * Check for a connection.
   */
  
  if (status)
    /* success */
    {
      long accept_socket_fd = allocate_fd(STREAM_SOCKET);
      stream_descriptor_t* accept_stream_descriptor = &(stream_descriptors[accept_socket_fd]);
      accept_stream_descriptor->socket_data.socket = accept_socket;

      CloseHandle(callback_data->overlap.hEvent);
      
      /* We just need to do it, or things behave ... unexpectedly.
       * Go figure.
       */

      if (setsockopt(accept_socket, 
		     SOL_SOCKET, 
		     SO_UPDATE_ACCEPT_CONTEXT, 
		     (char *)&socket, 
		     sizeof(socket))
	  != 0)
	s48_os_error_2(call, "s48_accept", WSAGetLastError(), 2,
		       channel, retry_p);

      input_channel = s48_add_channel_2(call, s48_channel_status_input_2(call),
					s48_enter_string_latin_1_2(call, "socket connection"),
					accept_socket_fd);

      if (!s48_channel_p_2(call, input_channel))
	{
	  ps_close_fd(accept_socket_fd);
	  s48_raise_scheme_exception_2(call, s48_extract_long_2(call, input_channel), 0);
	}

      {
	LPSOCKADDR localSockaddr = NULL;
	int localSockaddrLength = 0;
	LPSOCKADDR remoteSockaddr = NULL;
	int remoteSockaddrLength = 0;

	GetAcceptExSockaddrs(callback_data->buffer,
			     0,
			     SOCKADDR_BUFFER_SIZE,
			     SOCKADDR_BUFFER_SIZE,
			     &localSockaddr,
			     &localSockaddrLength,
			     &remoteSockaddr,
			     &remoteSockaddrLength);

	return s48_cons_2(call,
			  input_channel,
			  s48_enter_sockaddr(call,
					     (const struct sockaddr*)(remoteSockaddr),
					     remoteSockaddrLength));
      }
    }

  if (WSAGetLastError() == ERROR_IO_PENDING)
    {
      DWORD id;
      HANDLE thread_handle;

      if (!s48_add_pending_fd(socket_fd, PSTRUE))
	s48_out_of_memory_error_2(call);

      stream_descriptor->socket_data.hatch_callback = accept_callback;
      thread_handle = CreateThread(NULL, /* lpThreadAttributes */
				   4096, /* dwStackSize */
				   (LPTHREAD_START_ROUTINE)hatch_thread_proc,
				   stream_descriptor,
				   0, /* dwCreationFlags, */
				   &id);
      return s48_false_2(call);
    }
  
  s48_os_error_2(call, "s48_accept", WSAGetLastError(), 2,
		 channel, retry_p);
  
  /* not reachable: */
  return s48_false_2(call);
}

/* Connect a socket to an address.
 *
 * If this succeeds, it returns an output channel for the connection.
 * If it fails because the connect would block, add the socket to the
 * pending queue (for output) and return #f.
 * If it fails for any other reason, raise an exception.
 */

/*
 * We need to jump through the same hoops as with s48_accept.
 */

static VOID CALLBACK
connect_callback(DWORD dwParam)
{
  stream_descriptor_t* stream_descriptor = (stream_descriptor_t*)dwParam;
  callback_data_t* callback_data = &(stream_descriptor->callback_data);

  s48_add_ready_fd(callback_data->fd, PSFALSE, PSFALSE, (long)0);
}

static s48_ref_t
s48_connect(s48_call_t call, s48_ref_t sch_channel,
	    s48_ref_t sch_address,
	    s48_ref_t sch_retry_p)
{
  long socket_fd;
  stream_descriptor_t* stream_descriptor;
  callback_data_t* callback_data;
  SOCKET socket;
  int status;

  s48_check_channel_2(call, sch_channel);
  socket_fd = s48_unsafe_extract_long_2(call, s48_unsafe_channel_os_index_2(call, sch_channel));
  stream_descriptor = &(stream_descriptors[socket_fd]);
  callback_data = &(stream_descriptor->callback_data);
  socket = stream_descriptor->socket_data.socket;

  /*
   * Try the connection.  If it works we make an output channel and return it.
   * The original socket channel will be used as the input channel.
   */

  /*
   * ConnectEx, which works like AcceptEx, only exists on the very
   * latest Windows versions.
   */

  status = WSAConnect(socket,
		      (const struct sockaddr*)s48_extract_value_pointer_2(call, sch_address, struct sockaddr),
		      s48_value_size_2(call, sch_address),
		      NULL, /* lpCallerData */
		      NULL, /* lpCalleeData */
		      NULL, /* lpSQOS */
		      NULL); /* lpGQOS */
  /*
   * Check for errors.  If we need to wait we set up a callback
   * and return #F to tell the Scheme procedure to try again.
   */
  if (status == 0)
    /* success */
    {
      s48_unsafe_stob_set_2(call, sch_channel, s48_channel_status_offset, s48_channel_status_input_2(call));
      return dup_socket_channel(call, socket_fd);
    }
  else if (WSAGetLastError() == WSAEWOULDBLOCK)
    {
      DWORD id;
      HANDLE thread_handle;

      if (!s48_add_pending_fd(socket_fd, PSFALSE))
	s48_out_of_memory_error_2(call);

      stream_descriptor->socket_data.hatch_callback = connect_callback;
      thread_handle = CreateThread(NULL, /* lpThreadAttributes */
				   4096, /* dwStackSize */
				   (LPTHREAD_START_ROUTINE)hatch_thread_proc,
				   stream_descriptor,
				   0, /* dwCreationFlags, */
				   &id);
      callback_data->overlap.hEvent = CreateEvent(NULL, /* no security attributes */
						  TRUE, /* manual reset */
						  FALSE, /* nonsignaled */
						  NULL); /* name */
      if (WSAEventSelect(socket, callback_data->overlap.hEvent, FD_ACCEPT)
	  != 0)
	s48_os_error_2(call, "s48_connect", WSAGetLastError(), 3,
		       sch_channel, sch_address, sch_retry_p);
      
      return s48_false_2(call);
    }
  else
    s48_os_error_2(call, "s48_connect", WSAGetLastError(), 3,
		   sch_channel, sch_address, sch_retry_p);

  /* not reachable */
  return s48_false_2(call);
}

/*
 * Receive a message.  Returns pair (<byte-count> . <sender>) or just
 * <byte-count> if want_sender_p is false.
 */


static s48_ref_t
s48_recvfrom(s48_call_t call, s48_ref_t sch_channel,
	     s48_ref_t sch_buffer, s48_ref_t sch_start, s48_ref_t sch_count,
	     s48_ref_t sch_flags,
	     s48_ref_t sch_want_sender_p,
	     s48_ref_t sch_retry_p)
{
  int retry_p = !(s48_false_p_2(call, sch_retry_p));
  int want_sender_p = !(s48_false_p_2(call, sch_want_sender_p));
  int flags = s48_extract_msg_flags(call, sch_flags);
  size_t buffer_size = s48_byte_vector_length_2(call, sch_buffer);
  size_t start = s48_extract_unsigned_long_2(call, sch_start);
  size_t count = s48_extract_unsigned_long_2(call, sch_count);
  long socket_fd
    = s48_unsafe_extract_long_2(call, s48_channel_os_index_2(call, sch_channel));
  stream_descriptor_t* stream_descriptor = &(stream_descriptors[socket_fd]);
  callback_data_t* callback_data = &(stream_descriptor->callback_data);
  socket_t socket = stream_descriptor->socket_data.socket;

  if ((start + count) > buffer_size)
    s48_assertion_violation_2(call, "s48_sendto", "buffer start or count is wrong", 3,
			      sch_buffer, sch_start, sch_count);

  if (!retry_p)
    {
      DWORD numberOfBytesRecvd;
      int wsa_status;
      int block;
      
      /* There's nothing in the buffer---need to do an actual read. */
      maybe_grow_callback_data_buffer(callback_data, (size_t)count);
      
      /* just making sure */
      callback_data->overlap.Offset = 0;
      callback_data->overlap.OffsetHigh = 0;
      
      stream_descriptor->socket_data.wsabuf.len = count;
      stream_descriptor->socket_data.wsabuf.buf = (char*)callback_data->buffer;
      
      stream_descriptor->socket_data.recv_address_len
	= sizeof(stream_descriptor->socket_data.recv_address);
      block = 1; /* non-blocking */
      ioctlsocket(socket, FIONBIO, &block);
      
      wsa_status = WSARecvFrom(socket,
			       (LPWSABUF)&stream_descriptor->socket_data.wsabuf,
			       1,
			       &numberOfBytesRecvd,
			       &flags,
			       want_sender_p
			       ? (struct sockaddr*)&stream_descriptor->socket_data.recv_address
			       : NULL,
			       want_sender_p
			       ? &stream_descriptor->socket_data.recv_address_len
			       : NULL,
			       (LPOVERLAPPED)callback_data,
			       recv_completed);
      
      if ((wsa_status == 0)
	  || (WSAGetLastError() == WSA_IO_PENDING))
	{
	  if (! (s48_add_pending_fd(socket_fd, PSTRUE)))
	    s48_out_of_memory_error_2(call);
	  return s48_false_2(call);
	}
      else
	s48_os_error_2(call, "s48_recvfrom", WSAGetLastError(), 7,
		       sch_channel, sch_buffer, sch_start, sch_count,
		       sch_flags, sch_want_sender_p, sch_retry_p);
    }
  else
    {
      /* rebound */
      size_t size = callback_data->current_size;
      memcpy(s48_extract_byte_vector_2(call, sch_buffer) + start,
	     callback_data->current, size);
      callback_data->current_size = 0;
      callback_data->current = stream_descriptor->callback_data.buffer;

      if (want_sender_p)
	{
	  s48_ref_t sch_count, sch_saddr;
	  s48_ref_t sch_result;
	  sch_count = s48_enter_unsigned_long_2(call, size);
	  sch_saddr
	    = s48_enter_sockaddr(call, (struct sockaddr *)&stream_descriptor->socket_data.recv_address,
				 stream_descriptor->socket_data.recv_address_len);
	  sch_result = s48_cons_2(call, sch_count, sch_saddr);
	  return sch_result;
	}
      else
	return s48_enter_unsigned_long_2(call, size);
    }
}

static s48_ref_t
s48_sendto(s48_call_t call, s48_ref_t sch_channel,
	   s48_ref_t sch_buffer, s48_ref_t sch_start, s48_ref_t sch_count,
	   s48_ref_t sch_flags,
	   s48_ref_t sch_saddr,
	   s48_ref_t sch_retry_p)
{
  int retry_p = !(s48_false_p_2(call, sch_retry_p));
  const struct sockaddr *sa
    = s48_extract_value_pointer_2(call, sch_saddr, const struct sockaddr);
  socklen_t salen = s48_value_size_2(call, sch_saddr);
  int flags = s48_extract_msg_flags(call, sch_flags);
  size_t buffer_size = s48_byte_vector_length_2(call, sch_buffer);
  size_t start = s48_extract_unsigned_long_2(call, sch_start);
  size_t count = s48_extract_unsigned_long_2(call, sch_count);
  long socket_fd
    = s48_unsafe_extract_long_2(call, s48_channel_os_index_2(call, sch_channel));
  stream_descriptor_t* stream_descriptor = &(stream_descriptors[socket_fd]);
  callback_data_t* callback_data = &(stream_descriptor->callback_data);
  socket_t socket = stream_descriptor->socket_data.socket;

  if ((start + count) > buffer_size)
    s48_assertion_violation_2(call, "s48_sendto", "buffer start or count is wrong", 3,
			      sch_buffer, sch_start, sch_count);
  
  if (!retry_p)
    {
	DWORD numberOfBytesSent;
	DWORD flags = 0;
	int wsa_status;
	char* buffer = s48_extract_byte_vector_readonly_2(call, sch_buffer) + start;

	maybe_grow_callback_data_buffer(callback_data, count);
	
	memcpy(callback_data->buffer, buffer, count);

	/* just making sure */
	callback_data->overlap.Offset = 0;
	callback_data->overlap.OffsetHigh = 0;

	stream_descriptor->socket_data.wsabuf.len = count;
	stream_descriptor->socket_data.wsabuf.buf = (char*)callback_data->buffer;

	wsa_status = WSASendTo(socket,
			       (LPWSABUF)&stream_descriptor->socket_data.wsabuf,
			       1,
			       &numberOfBytesSent,
			       flags,
			       s48_extract_value_pointer_2(call, sch_saddr, struct sockaddr),
			       s48_value_size_2(call, sch_saddr),
			       (LPOVERLAPPED)callback_data,
			       send_completed);
	if ((wsa_status == 0)
	    | (WSAGetLastError() == WSA_IO_PENDING))
	  {
	    if (! (s48_add_pending_fd(socket_fd, PSFALSE)))
	      s48_out_of_memory_error_2(call);
	    
	    return s48_false_2(call);
	  }
	else
	  s48_os_error_2(call, "s48_sendto", WSAGetLastError(), 7,
			 sch_channel, sch_buffer, sch_start, sch_count,
			 sch_flags, sch_saddr, sch_retry_p);
    }
  else
    /* rebound */
    return s48_enter_long_2(call, stream_descriptor->file_regular_data.current_offset);
}

void
s48_init_os_sockets(void)
{
  WORD wVersionRequested;
  WSADATA wsaData;
 
  /* This is the *highest* version we can use.  Great. */
  wVersionRequested = MAKEWORD(2, 0);
 
  if (WSAStartup(wVersionRequested, &wsaData) != 0)
    {
      fprintf(stderr, "Windows Sockets startup failed.\n");
      exit(-1);
    }

  S48_EXPORT_FUNCTION(s48_socket);
  S48_EXPORT_FUNCTION(s48_socketpair);
  S48_EXPORT_FUNCTION(s48_dup_socket_channel);
  S48_EXPORT_FUNCTION(s48_accept);
  S48_EXPORT_FUNCTION(s48_connect);
  S48_EXPORT_FUNCTION(s48_recvfrom);
  S48_EXPORT_FUNCTION(s48_sendto);
}
