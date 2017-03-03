; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber

; To load the VM into Scheme 48:
;   ,config ,load ../platform-interfaces.scm
; then
;   ,config ,load ps-platform-32-packages.scm
;      - or -
;   ,config ,load ps-platform-64-packages.scm
; (ignoring the warning) and then:
;   ,exec ,load load-vm.scm
; (ignoring "name from opened structure redefined" warnings for the config package)
; Then, for example,
;  (start-vm "=scheme48/../build/initial.image" 2000000 20000 '#())
; in the user package will start up the VM with the initial image.
; Be patient.  It will take a while.
;
; You will need to have a large heap to start with. Using a BIBOP
; build with -h 0 works best.
;
; To send input to the VM, do the following:
;
;  Breakpoint: Waiting
;  1> ,in interpreter (set! s48-*pending-events?* #t)
;  1> ,proceed
;  (+ 2 3)                             ; this will be read by the loaded VM
;  Call to (schedule-interrupt 200)    ; output noise
;  Call to (schedule-interrupt 200)
;  Call to (schedule-interrupt 200)
;  5                                   ; the answer
;  > Call to (schedule-interrupt 200)  ; prompt by the loaded S48
;
;  Breakpoint: Waiting
;  1>                                  ; prompt by the base S48
;
; There can't be a pause between the `,proceed' and the input for
; the loaded VM.  This is easy to accomplish running under Emacs.
; If there is a pause you will hit the breakpoint again and the
; `(+ 2 3)' or whatever will be read by the base system.
;
; It is easier to debug changes to the VM using images smaller than
; the usual build/initial.image and scheme48.image.  What I usually
; do is modify scheme/debug/tiny.scm so that it runs the code I want
; to test and then build scheme/debug/tiny.image.  The version that
; comes with the system prints `Hello' and the first command argument,
; if any, then reads a line from standard input and prints it to
; standard output.  The image file is less than 12k bytes in size so
; it starts up much faster than the larger images.  Here is a transcript:
;
;    % make scheme/debug/tiny.image
;    % ./scheme48vm -i scheme/debug/tiny.image -a the-argument
;    Correcting byte order of resumed image.
;    Hello the-argument
;    now I type a line to be echoed
;    now I type a line to be echoed
;    % 
;
; When modifying scheme/debug/tiny.scm you have to keep in mind that
; you can only use the basic system: only the standard special forms
; (no LET, for example) and only the primitives implemented by the
; VM (no MAP, etc.).  

(config)
(load "../prescheme/interface.scm")
(load "../prescheme/package-defs.scm")
(load "interfaces.scm")
(load "shared-interfaces.scm")
(load "s48-package-defs.scm")
(load "package-defs.scm")
(load "twospace-gc-package-defs.scm")
(load-package 'destructuring)  ; used in FOR-SYNTAX clause
(load-package 'bigbit)
(load-package 'vm)

(user)
(open 'ps-memory)
(open 'vm)
(open 'heap-init)
(open 'read-image)
(open 'memory-debug)

(run '
(define (start-vm image-file heap-size stack-size start-args)
  (reinitialize-memory)
  (s48-read-image image-file heap-size)
  (s48-initialize-vm (allocate-memory stack-size)
		     (quotient stack-size 4))
  (s48-call-startup-procedure start-args
			      (vector-length start-args)))
)
