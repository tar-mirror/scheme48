; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber

; The value of $NOTE-FILE-PACKAGE is called whenever a file is loaded into
; a package.  env/debug.scm uses this to associate packages with files so
; that code stuffed to the REPL will be eval'ed in the correct package.
;
; Is there any point in having this be a fluid?

(define $note-file-package
  (make-fluid (make-cell (lambda (filename package)
			   (values)))))

(define (read-forms pathname package script?)
  (let* ((filename (namestring pathname #f *scheme-file-type*))
         (truename (translate filename))
   	 (port (open-input-file truename))
	 (reader (package-reader package)))
    (dynamic-wind
     (lambda ()
       (if (not port)
	   (assertion-violation 'read-forms "attempt to throw back into READ-FORMS")))
     (lambda ()
       ((fluid-cell-ref $note-file-package) filename package)
       (let ((o-port (current-noise-port)))
	 (display truename o-port)
	 (force-output o-port)
	 (really-read-forms port reader script?)))
     (lambda ()
       (close-input-port port)
       (set! port #f)))))

(define (really-read-forms port reader script?)
  (if script?
      (skip-line port))
  (let loop ((forms '()))
    (let ((form (reader port)))
      (if (eof-object? form)
	  (reverse forms)
	  (loop (cons form forms))))))

(define (skip-line port)
  (let loop ()
    (let ((char (read-char port)))
      (if (and (not (eof-object? char))
	       (not (char=? #\newline char)))
	  (loop)))))
