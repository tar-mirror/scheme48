; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Marcel Turino, Manuel Dietrich

; This optimizer does the instrumentation for the exact call profiler,
; by calling the profiler before executing the real function code.

; It therefore needs a reference to the profile-count procedure,
; which is exported by the profiler structure.

(set-optimizer! 'profiler-instrumentation
		(lambda (forms package)
		  (get-pcount-name!)
		  (map (lambda (form)
			 (instrument-form (force-node form)))
		       forms)))

;;; returns a bound name-node for "name" out of "env"
(define (expand-name name env)
  (let ((binding (generic-lookup env name)))
    (if (node? binding)
	binding
	(let ((node (make-node operator/name name)))
	  (node-set! node 'binding (or binding 'unbound))
	  node))))

;;; caches the reference to the profile-count function
(define *pcount-name* #f)

(define (get-pcount-name!)
  (let* ((p (environment-ref (config-package) 'profiler))
	 (name (expand-name 'profile-count p)))
    (set! *pcount-name* name)))



(define (instrument-form node)
  (let ((out  (current-noise-port))
	(form (node-form node)))
    (if (define-node? node)
	(begin
	  (make-similar-node node
			     `(define ,(cadr form)
				,(instrument-node (caddr form)))))
	node)))

(define (instrument-node node)
  (cond
   ((node? node)
    ((operator-table-ref instrumentors (node-operator-id node)) node))
   ((list? node)
    (instrument-list node))
   (else
    node)))

(define (instrument-list nodes)
  (if (list? nodes)
      (map (lambda (node)
	     (instrument-node node))
	   nodes)
      nodes))

(define (no-instrumentation node)
  (let ((form (node-form node)))
    (make-similar-node node (instrument-list form))))

(define instrumentors
  (make-operator-table no-instrumentation))

(define (define-instrumentor name proc)
  (operator-define! instrumentors name #f proc))

(define-instrumentor 'literal             no-instrumentation)
(define-instrumentor 'quote               no-instrumentation)
(define-instrumentor 'primitive-procedure no-instrumentation)
(define-instrumentor 'call                no-instrumentation)
(define-instrumentor 'name                no-instrumentation)
(define-instrumentor 'set!                no-instrumentation)
(define-instrumentor 'loophole            no-instrumentation)
(define-instrumentor 'letrec              no-instrumentation)
(define-instrumentor 'pure-letrec         no-instrumentation)
(define-instrumentor 'lambda
  (lambda (node)
    (let* ((form     (node-form node))
	   (param    (cadr form))
	   (body     (cddr form)))
      (make-similar-node node
			 `(lambda ,param
			    ,(make-node operator/begin
					`(begin
					   ,(make-node operator/call
						       (list *pcount-name*))
					   ,@(instrument-list body))))))))
