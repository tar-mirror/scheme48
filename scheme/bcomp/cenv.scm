; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber

; Compile-time environments
; These are functions
;  name -> node			; lexical variable
;          binding              ; package variable, any syntax
;          #f			; free
;
; Special names are used to retrieve various values from compiler environments.

(define-record-type compiler-specials :compiler-specials
  (make-compiler-specials lookup define! macro-eval package source-file-name)
  compiler-specials?
  (lookup compiler-specials-lookup)
  (define! compiler-specials-define!)
  (macro-eval compiler-specials-macro-eval)
  (package compiler-specials-package)
  (source-file-name compiler-specials-source-file-name))

(define-record-type compiler-env :compiler-env
  (really-make-compiler-env specials alist)
  compiler-env?
  (specials compiler-env-specials)
  (alist compiler-env-alist))

(define (lookup cenv name)
  (cond
   ((assq name (compiler-env-alist cenv)) => cdr)
   (else
    ((compiler-specials-lookup (compiler-env-specials cenv)) name))))

(define (bind1 name binding cenv)
  (really-make-compiler-env (compiler-env-specials cenv)
			    (cons (cons name binding) (compiler-env-alist cenv))))

(define (bind names bindings cenv)
  (really-make-compiler-env (compiler-env-specials cenv)
			    (append (map cons names bindings)
				    (compiler-env-alist cenv))))

; Making the initial compiler environment.
;
;  lookup : name -> binding or (binding . path) or #f
;  define! : name type [static] -> void
;  macro-eval : reflective tower, i.e. promise that returns
;               (<eval> . <env>) for evaluating macro expanders

(define (make-compiler-env lookup define! macro-eval package)
  (really-make-compiler-env (make-compiler-specials lookup define! macro-eval package #f)
			    '()))

; EVAL function for evaluating macro expanders.

(define (comp-env-macro-eval cenv)
  (compiler-specials-macro-eval (compiler-env-specials cenv)))

; Function for adding definitions to the outer package.

(define (comp-env-define! cenv name type . maybe-value)
  (apply (compiler-specials-define! (compiler-env-specials cenv))
	 name type maybe-value))

; The package on which the compiler environment is based.  This is a
; temporary hack to keep the package-editing code working.

(define (extract-package-from-comp-env cenv)
  (compiler-specials-package (compiler-env-specials cenv)))

; The name of the source file.
;   This is used by the %FILE-NAME% special form,
;   which is in turn used by the (MODULE ...) form to save the current file in
;    each package, 
;   which is (finally) used to look up filenames in the correct directory.

(define (bind-source-file-name filename env)
  (if filename
      (let ((specials (compiler-env-specials env)))
	(really-make-compiler-env (make-compiler-specials 
				   (compiler-specials-lookup specials)
				   (compiler-specials-define! specials)
				   (compiler-specials-macro-eval specials)
				   (compiler-specials-package specials)
				   filename)
				  (compiler-env-alist env)))
      env))

(define (source-file-name cenv)
  (compiler-specials-source-file-name (compiler-env-specials cenv)))



