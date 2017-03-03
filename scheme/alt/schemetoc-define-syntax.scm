; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees

(define-macro define-syntax
  (lambda (form expander)
    (expander `(define-macro ,(cadr form)
		 (let ((transformer ,(caddr form)))
		   (lambda (form expander)
		     (expander (transformer form
					    (lambda (x) x)
					    eq?)
			       expander))))
	      expander)))

