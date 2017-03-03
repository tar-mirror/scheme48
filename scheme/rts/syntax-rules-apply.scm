; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Mike Sperber

; We compile each rule into a pattern and template which are then used at
; expansion time.  The compiler performs error checking, replaces pattern
; variables and ellipses with markers, and lists the introduced variables.
;
; Expansion is done in three steps:
;  - see if the pattern matches
;  - build the pattern-variable environment
;  - instantiate the template
; We could match the pattern and build the environment in a single pass, but
; separating them is simpler and may be faster for complex macros, as it
; avoids creating partial environments which are then discarded when the
; match fails.
;
; This would all be simple if it weren't for the ellipses.
;
; RANK is the ellipsis depth, initially zero.

;----------------------------------------------------------------
; This is the expansion part.  All it shares with the definition part
; is the format of the patterns and templates.

(define (apply-rules form rules name? rename compare)
  (let ((lose (lambda () form))		; must be tail-called
	(body (cdr form)))		; drop the macro's name
    (let loop ((rules rules))
      (cond ((null? rules)
	     (lose))
	    ((match? (caar rules) body name? rename compare)
	     (rewrite (cdar rules)	; template
		      (make-pattern-env (caar rules) body)
		      name?
		      rename
		      lose))
	    (else
	     (loop (cdr rules)))))))

(define (match? pattern form name? rename compare)
  (let label ((pattern pattern) (form form))
    (cond ((pair? pattern)
           (and (pair? form)
                (label (car pattern) (car form))
                (label (cdr pattern) (cdr form))))
          ((name? pattern)
           (and (name? form)
                (compare form (rename pattern))))
          ((pattern-variable? pattern)
           #t)
          ((ellipsis-form? pattern)
           (let loop ((form form))
             (or (null? form)
                 (and (pair? form)
                      (label (ellipsis-form-body pattern) (car form))
                      (loop (cdr form))))))
          ((vector-marker? pattern)
           (and (vector? form)
                (label (vector-marker-contents pattern)
                       (vector->list form))))
          (else
           (equal? pattern form)))))
                           
(define (make-pattern-env pattern form)
  (let label ((pattern pattern) (form form) (pattern-env '()))
    (cond ((pair? pattern)
           (label (cdr pattern)
                  (cdr form)
                  (label (car pattern) (car form) pattern-env)))
          ((pattern-variable? pattern)
           (cons (cons pattern form)
                 pattern-env))
          ((ellipsis-form? pattern)
           (let ((envs (map (lambda (form)
                              (label (ellipsis-form-body pattern)
                                     form
                                     '()))
                            form)))
             (append (map (lambda (var)
                            (cons var
                                  (map (lambda (env)
                                         (cdr (assq var env)))
                                       envs)))
                          (ellipsis-form-vars pattern))
                     pattern-env)))
          ((vector-marker? pattern)
           (label (vector-marker-contents pattern)
                  (vector->list form)
                  pattern-env))
          (else
           pattern-env))))

(define (rewrite template pattern-env name? rename lose)
  (let label ((template template) (pattern-env pattern-env))
    (cond ((null? template)
           '())
          ((pair? template)
           ((if (ellipsis-form? (car template))
                append
                cons)
            (label (car template) pattern-env)
            (label (cdr template) pattern-env)))
          ((name? template)
           (rename template))
          ((pattern-variable? template)
           (cdr (assq template pattern-env)))
          ((ellipsis-form? template)
           (rewrite-ellipsis label lose template pattern-env))
          ((vector-marker? template)
           (list->vector (label (vector-marker-contents template)
                                pattern-env)))
          (else
           template))))
  
(define (rewrite-ellipsis label lose template pattern-env)
  (let ((template (ellipsis-form-body template))
        (vars     (ellipsis-form-vars template)))
    (let ((vals (map (lambda (var)
                       (reverse (cdr (assq var pattern-env))))
                     vars)))
      (if (or (null? (cdr vals))
              (apply = (map length vals)))
          (let loop ((vals vals) (results '()))
            (if (null? (car vals))
                results
                (loop (map cdr vals)
                      ((if (ellipsis-form? template)
                           append
                           cons)
                       (label template
                              (append (map (lambda (var vals)
                                             (cons var (car vals)))
                                           vars
                                           vals)
                                      pattern-env))
                       results))))
          (lose)))))
