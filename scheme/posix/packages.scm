; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber, Roderic Morris,
; Eric Knauel, Martin Gasbichler, Will Noble

(define-interface posix-files-interface
  (export directory-stream?
	  open-directory-stream		; name -> directory
	  read-directory-stream		; directory -> name or #f
	  close-directory-stream	; directory -> ()
	  list-directory		; name -> list of names

	  working-directory
	  set-working-directory!

	  open-file

	  (file-options :syntax)
	  file-options-on?
	  file-options-union

	  make-directory
	  make-fifo

	  link
	  unlink
	  remove-directory
	  rename

	  get-file-info get-file/link-info get-port-info

	  file-info?
	  file-info-name
	  file-info-type
	  file-info-device
	  file-info-inode
	  file-info-mode
	  file-info-link-count
	  file-info-owner
	  file-info-group
	  file-info-size
	  file-info-last-access
	  file-info-last-modification
	  file-info-last-status-change

	  file-type? file-type-name
	  (file-type :syntax)

	  set-file-creation-mask!

	  file-mode?
	  (file-mode :syntax)
	  file-mode+ file-mode-
	  file-mode=? file-mode<=? file-mode>=?
	  file-mode->integer integer->file-mode

	  accessible?
	  (access-mode :syntax)

	  create-symbolic-link
	  read-symbolic-link
	  ))

(define-interface posix-time-interface
  (export make-time time? time-seconds
	  time=? time<? time<=? time>? time>=?
	  time->string
	  current-time
	  make-date date?
	  date-second date-minute date-hour
	  date-month-day date-month
	  date-year
	  date-week-day
	  date-year-day
	  date-dst

	  date->string
	  time->utc-date
	  time->local-date
	  date->time
	  format-date
	  ))

(define-interface posix-users-interface
  (export user-id?
	  user-id->integer integer->user-id
	  user-id=?

	  user-info?
	  user-info-name user-info-id user-info-group
	  user-info-home-directory user-info-shell
	  user-id->user-info
	  name->user-info

	  group-id?
	  group-id->integer integer->group-id
	  group-id=?

	  group-info?
	  group-info-name group-info-id group-info-members
	  group-id->group-info
	  name->group-info
	  ))

(define-interface posix-errnos-interface
  (export (errno :syntax)
	  errno-name
	  errno-os-number
	  integer->errno
	  name->errno
          errno=?
          errno?))

(define-interface posix-syslog-interface
  (export (syslog-option :syntax)
	  syslog-option?

	  make-syslog-options
	  syslog-options?
	  (syslog-options :syntax)

	  (syslog-facility :syntax)
	  syslog-facility?

	  (syslog-level :syntax)
	  syslog-level?

	  make-syslog-mask
	  syslog-mask?
	  (syslog-mask :syntax)
	  syslog-mask-all
	  syslog-mask-upto

	  with-syslog-destination

	  syslog

	  open-syslog-channel
	  close-syslog-channel
	  with-syslog-channel))

(define-structures ((posix-files posix-files-interface)
		    (posix-users posix-users-interface))
  (open scheme define-record-types finite-types
	external-calls load-dynamic-externals
	bitwise			;for manipulating protection masks
	exceptions
	posix-file-options
	posix-time ; external binding for `posix-time-type'
	channels
	channel-i/o
	channel-ports
	os-strings)
  (for-syntax (open scheme bitwise))
  (files dir))

(define-structure posix-time  posix-time-interface
  (open scheme
	define-record-types
	external-calls load-dynamic-externals
	os-strings)
  (files time))

(define-structure posix-file-options (export ((file-option file-options)
					        :syntax)
					     file-options?
					     file-options-on?
					     file-options-union)
  (open scheme define-record-types finite-types enum-sets
	external-calls load-dynamic-externals
	bitwise)
  (files file-options))

(define-interface posix-process-data-interface
  (export get-process-id get-parent-process-id

	  ; I am not happy with these names.  They don't mention the process.
	  get-user-id get-effective-user-id
	  set-user-id! set-effective-user-id!
	  get-group-id get-effective-group-id
	  set-group-id! set-effective-group-id!

	  get-groups
	  get-login-name

	  lookup-environment-variable
	  lookup-environment-variable->string
	  set-environment-variable!
	  environment-alist
	  environment-alist-as-strings
	  ))

(define-interface posix-platform-names-interface
  (export os-name os-node-name os-release-name os-version-name
	  machine-name))

(define-structures ((posix-process-data posix-process-data-interface)
		    (posix-platform-names posix-platform-names-interface))
  (open scheme define-record-types
	external-calls load-dynamic-externals
	os-strings
	interrupts
	posix-processes posix-users posix-time) ; we need these to be loaded
  (files proc-env))

(define-interface posix-processes-interface
  (export process-id?
	  process-id=?

	  process-id->integer
	  integer->process-id

	  process-id-exit-status
	  process-id-terminating-signal

	  fork
	  fork-and-forget

	  exec
	  exec-with-environment
	  exec-file
	  exec-file-with-environment
	  exec-with-alias

	  exit

	  wait-for-child-process

	  signal-process

	  (signal :syntax)
	  signal-name
	  signal-os-number
	  integer->signal
	  name->signal
          signal=?
          signal?

	  make-signal-queue
	  signal-queue?
	  signal-queue-monitored-signals
	  dequeue-signal!
	  maybe-dequeue-signal!
	  signal-queue-signals
	  add-signal-queue-signal!
	  remove-signal-queue-signal!
	  ))

(define-structure posix-processes posix-processes-interface
  (open scheme
	define-record-types finite-types
	reinitializers
	external-calls load-dynamic-externals
	interrupts
	placeholders
	weak
	value-pipes
	debug-messages
	session-data
	exceptions
	root-scheduler		;scheme-exit-now
	channel-ports		;force-channel-output-ports!
	interrupts		;set-interrupt-handler!
	architecture		;interrupts enum
	os-strings)
  (files proc
	 signal))

(define-interface posix-i/o-interface
  (export open-pipe

	  dup
	  dup-switching-mode
	  dup2
	  remap-file-descriptors!
	  close-all-but

	  close-on-exec?
	  set-close-on-exec?!

	  i/o-flags
	  set-i/o-flags!

	  fd-port?
	  port->fd

	  port-is-a-terminal?
	  port-terminal-name
	  ))

(define-structure posix-i/o posix-i/o-interface
  (open scheme
	external-calls load-dynamic-externals
	i/o			;read-block
	channels
	channel-i/o
	channel-ports
	exceptions
	util
	posix-file-options
	ports			;port?
	os-strings
	architecture
	enum-case)
  (files io))

(define-interface posix-regexps-interface
  (export make-regexp
	  (regexp-option :syntax)
	  regexp?
	  regexp-match

	  match?
	  match-start
	  match-end
	  match-submatches
	  ))

(define-structures ((posix-regexps posix-regexps-interface)
		    (posix-regexps-internal (export make-match)))
  (open scheme define-record-types finite-types
	external-calls load-dynamic-externals
	(subset big-util (string->immutable-string))
	exceptions
	os-strings text-codecs)
  (files regexp))

(define-interface regexps-interface
  (export set
	  range ranges
	  ascii-range ascii-ranges

	  negate intersection union subtract

	  regexp?

	  lower-case upper-case alphabetic
	  numeric hexdigit
	  alphanumeric
	  punctuation whitespace blank
	  graphic printing
	  control

	  sequence one-of text repeat

	  string-start string-end

	  ignore-case use-case

	  submatch no-submatches

	  any-match? exact-match?
	  match

	  match?
	  match-start match-end match-submatches))

(define-interface regexps-internal-interface
  (export set? char-in-set? set-string
	  the-empty-set empty-set?
	  string-start? string-end?
	  submatch? submatch-exp submatch-id
	  sequence? sequence-exps
	  epsilon epsilon?
	  one-of? one-of-exps
	  repeat? repeat-low repeat-high repeat-exp))

(define-structures ((regexps regexps-interface)
		    (regexps-internal regexps-internal-interface))
  (open scheme define-record-types mvlet ascii unicode exceptions
	bitwise bigbit
	reduce
	(modify posix-regexps (hide regexp?) (rename (make-regexp make-posix-regexp)))
	posix-regexps-internal
	(subset util (every)))
  (optimize auto-integrate)
  (files func-regexp))


(define-structure posix-errnos posix-errnos-interface
  (open scheme
	exceptions
	define-record-types
	finite-types
	external-calls load-dynamic-externals
	(subset unicode-char-maps (string-upcase string-foldcase))
	session-data
	interrupts
	reinitializers
	weak)
  (files errno))

(define-structure posix-syslog posix-syslog-interface
  (open scheme
	exceptions
	fluids
	locks
	define-record-types
	finite-types enum-sets enum-sets-internal
	external-calls
	load-dynamic-externals
	reinitializers
	os-strings)
  (files syslog))

; All in one chunk.

(define-structure posix (compound-interface
			  (interface-of posix-processes)
			  (interface-of posix-process-data)
			  (interface-of posix-platform-names)
			  (interface-of posix-files)
			  (interface-of posix-i/o)
			  (interface-of posix-time)
			  (interface-of posix-users)
			  (interface-of posix-regexps)
			  (interface-of posix-errnos)
			  (interface-of posix-syslog))
  (open posix-processes
	posix-process-data
	posix-platform-names
	posix-files
	posix-i/o
	posix-time
	posix-users
	posix-regexps
	posix-errnos
	posix-syslog))

