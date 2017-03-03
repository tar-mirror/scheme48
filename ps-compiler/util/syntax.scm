; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Mike Sperber


; Syntax used by the compiler

; Subrecords
;
; SUPER is the name of the existing record
; SUB is the name of the subrecord
; SLOT is the name of the slot to use in the existing sturcture
; STUFF is the usual stuff from DEFINE-RECORD-TYPE

(define-syntax define-subrecord
  (lambda (form rename compare)
    (let ((super (cadr form))
	  (sub (caddr form))
	  (slot (cadddr form))
	  (stuff (cddddr form)))
      (let ((access-names (map (lambda (spec)
				 (if (pair? spec) (car spec) spec))
			       (append (car stuff) (cadr stuff))))
	    (set-names (append (filter-map (lambda (spec)
					     (if (pair? spec) (car spec) #f))
					   (car stuff))
			       (map (lambda (spec)
				      (if (pair? spec) (car spec) spec))
				    (cadr stuff)))))
	`(begin (,(rename 'rk:define-record-type) ,sub . ,stuff)
		,@(map (lambda (name)
			 `(define ,(concatenate-symbol super '- name)
			    (lambda (v)
			      (,(concatenate-symbol sub '- name)
			       (,slot v)))))
		       access-names)
		,@(map (lambda (name)
			 `(define ,(concatenate-symbol 'set- super '- name '!)
			    (lambda (v n)
			      (,(concatenate-symbol 'set- sub '- name '!)
			       (,slot v)
			       n))))
		       set-names))))))

; Subrecords, version for JAR/SRFI-9 records
; This should eventually replace the above.
; 
; (define-subrecord-type id type-name super-slot
;   (maker ...)
;   predicate?
;   (slot accessor [modifier])
;   ...)
;
; SUPER-SLOT is the name of the slot to use in the existing record.

(define-syntax define-subrecord-type
  (lambda (form rename compare)
    (let ((id (cadr form))
	  (type (caddr form))
	  (slot (cadddr form))
	  (rest (cddddr form))
	  (%define-record-type (rename 'define-record-type))
	  (%define (rename 'define))
	  (%x (rename 'v))
	  (%v (rename 'x)))
      (let ((maker (car rest))
	    (pred (cadr rest))
	    (slots (cddr rest))
	    (gensym (lambda (s i)
		      (rename (string->symbol
			       (string-append (symbol->string s)
					      "%"
					      (number->string i)))))))
	`(begin
	   (,%define-record-type ,id ,type
	       ,maker
	       ,pred
	       ,@(do ((slots slots (cdr slots))
		      (i 0 (+ i 1))
		      (new '() `((,(caar slots)
				  ,(gensym 'subrecord-ref i)
				  ,@(if (null? (cddar slots))
					'()
					`(,(gensym 'subrecord-set i))))
				 . ,new)))
		     ((null? slots)
		      (reverse new))))
	       ,@(do ((slots slots (cdr slots))
		      (i 0 (+ i 1))
		      (new '() `(,@(if (null? (cddar slots))
				       '()
				       `((,%define (,(caddar slots) ,%x ,%v)
					   (,(gensym 'subrecord-set i)
					    (,slot ,%x)
					    ,%v))))
				 (,%define (,(cadar slots) ,%x)
				     (,(gensym 'subrecord-ref i)
				      (,slot ,%x)))
				 . ,new)))
		     ((null? slots)
		      (reverse new))))))))

;(define-syntax define-simple-record-type
;  (lambda (form rename compare)
;    (let ((name (cadr form))
;          (slots (cddr form)))
;      `(begin (define-record-type ,name ,slots ())
;              (define ,(concatenate-symbol 'make- name)
;                ,(concatenate-symbol name '- 'maker))))))

; Nothing actually local about it...

(define-syntax define-local-syntax
  (lambda (form rename compare)
    (let ((pattern (cadr form))
	  (body (cddr form)))
      `(,(rename 'define-syntax) ,(car pattern)
	 (,(rename 'lambda) (form rename compare)
	   (,(rename 'destructure) ((,(cdr pattern)
				     (,(rename 'cdr) form)))
	     . ,body))))))
