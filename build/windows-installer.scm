; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber

; This produces the auto-generated part of the Windows installer
; source code.

(define *non-slashes* (char-set-complement (char-set #\/)))

; returns a pair of directory (itself a list) and base file name
(define (split-file-name f)
  (let ((rev-components (reverse (string-tokenize f *non-slashes*))))
    (cons (reverse (cdr rev-components))
	  (car rev-components))))

(define (write-file-elements-include-file file-names uuids output-file-name)
  (call-with-output-file output-file-name
    (lambda (port)
      (display "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>" port)
      (newline port)
      (display "<Include>" port)
      (newline port)
      (write-file-elements file-names uuids port)
      (display "</Include>" port)
      (newline port))))

; organize the directories into a tree
; returns a tree = list of (union string (cons subdir tree))
(define (tree-ize-directory-alist alist)
  (call-with-values
      (lambda ()
	(partition (lambda (pair) (null? (car pair))) alist))
    (lambda (file-pairs directory-pairs)
      (let* ((prefixes (delete-duplicates (map caar directory-pairs)))
	     (subdirectories
	      (map (lambda (prefix)
		     (let* ((with-prefix-pairs
			     (filter (lambda (pair)
				       (string=? prefix (caar pair)))
				     directory-pairs))
			    (omit-prefix
			     (map (lambda (pair)
				    (cons (cdar pair) (cdr pair)))
				  with-prefix-pairs)))
		       (cons prefix (tree-ize-directory-alist omit-prefix))))
		   prefixes)))
	(append subdirectories (concatenate (map cdr file-pairs)))))))

; write the WiX file elements for a given list of file names

(define (write-file-elements file-names uuids port)
  (let* ((split-names (map split-file-name file-names))
	 (directories (delete-duplicates (map car split-names)))
	 (alist
	  (map (lambda (directory)
		 (cons directory
		       (filter-map (lambda (split-name)
				     (if (equal? directory (car split-name))
					 (cdr split-name)
					 #f))
				   split-names)))
	       directories))
	 (tree (tree-ize-directory-alist alist)))

    (write-directory-tree '() tree (make-uuid-source uuids) port)))

(define (make-uuid-source uuids)
  (lambda ()
    (let ((uuid (car uuids)))
      (set! uuids (cdr uuids))
      uuid)))

(define (write-directory-tree directory alist uuid-source port)
  (if (not (null? directory))
      (begin
	(display "<Directory Id=\"" port)
	(display (directory-id directory) port)
	(display "\" Name=\"" port)
	(display (car (reverse directory)) port)
	(display "\" >" port)
	(newline port)))

  (call-with-values
      (lambda () (partition string? alist))
    (lambda (file-names directory-entries)

      (if (not (null? file-names))
	  (begin
	    (display "<Component Id=\"" port)
	    (display (component-id directory) port)
	    (display "\" Guid=\"" port)
	    (display (uuid-source) port)
	    (display "\">" port)
	    (newline port)
	    (let ((used-file-names (list '()))) ; poor man's cell
	      (for-each (lambda (file-name)
			  (write-file-element port directory file-name used-file-names))
			file-names))
	    (display "</Component>" port)
	    (newline port)))

      (for-each (lambda (entry)
		  (write-directory-tree (append directory (list (car entry)))
					(cdr entry)
					uuid-source
					port))
		directory-entries)))
  (if (not (null? directory))
      (begin
	(display "</Directory>" port)
	(newline port))))

(define (quote-component comp)
  (list->string
    (map (lambda (ch)
           (if (char=? ch #\-)
               #\.
               ch))
         (string->list comp))))

; insert separators between the components
(define (components->string directory separator)
  (let ((id #f))
    (for-each (lambda (component)
		(if id
		    (set! id (string-append id separator component))
		    (set! id component)))
	      directory)
    id))

(define (components->quoted-string dir sep)
  (components->string (map quote-component dir) sep))

(define (directory-id directory)
  (components->quoted-string directory "_"))

(define (file-id directory base)
  (components->quoted-string (append directory (list base)) "_"))

(define (file-src directory base)
  (components->string (append directory (list base)) "/"))

(define (component-id directory)
  (components->quoted-string (append directory (list "component")) "_"))

(define (write-file-element port directory base-name used-file-names)
  (display "<File Id=\"" port)
  (display (file-id directory base-name) port)
  (display "\" Name=\"" port)
  (display base-name port)
  (display "\" Source=\"" port)
  (display (file-src directory base-name) port)
  (display "\" DiskId=\"1\" Vital=\"yes\" />" port)
  (newline port))
