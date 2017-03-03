; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber

; Command-level integration of nongenerative record types.

(define-user-command-syntax 'list-nongenerative-record-types ""
  "list the nongenerative record types"
  '())

(define-user-command-syntax 'delete-nongenerative-record-type "<uid>"
  "delete a nongenerative record type"
  '(name))

(define (list-nongenerative-record-types-command)
  (for-each (lambda (rtd)
	      (let ((port (command-output)))
		(display (record-type-name rtd) port)
		(display " [" port)
		(display (record-type-uid rtd) port)
		(display "]" port)
		(newline port)))
	    (nongenerative-record-types)))

(define (delete-nongenerative-record-type-command uid)
  (if (not (delete-nongenerative-record-type uid))
      (let ((port (command-output)))
	(display "not found" port)
	(newline port))))
  
(environment-define! (user-command-environment)
		     'list-nongenerative-record-types
		     list-nongenerative-record-types-command)

(environment-define! (user-command-environment)
		     'delete-nongenerative-record-type
		     delete-nongenerative-record-type-command)

