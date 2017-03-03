; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber

; Check that a node is well-formed

(define (check-node node)
  (cond
   ((lambda-node? node)
    (check-lambda node))
   ((call-node? node)
    (check-call node))
   ((literal-node? node)
    (check-literal node))
   ((reference-node? node)
    (check-reference node))
   (else
    (assertion-violation 'check-node "unknown node type" node))))

(define (check-lambda node)
  (if (not (memq (lambda-type node) '(cont proc jump)))
      (assertion-violation 'check-node "invalid lambda type" node))
  (if (and (eq? 'jump (lambda-type node))
	   (not (memq (call-primop-id (node-parent node)) '(let letrec2))))
      (assertion-violation 'check-node "jump lambda must be bound by let or letrec2" node))
  (for-each (lambda (var)
	      (set-variable-flag! var #t))
	    (lambda-variables node))
  (let ((body (lambda-body node)))
    (if (not (call-node? body))
	(assertion-violation 'check-node "lambda body is not a call" node))
    (if (trivial-primop-call? body)
	(assertion-violation 'check-node "body call of a lambda must have non-trivial primop" node))
    (check-nontrivial-primop-call body))
  (for-each (lambda (var)
	      (set-variable-flag! var #f))
	    (lambda-variables node)))

(define (trivial-primop-call? node)
  (primop-trivial? (call-primop node)))

(define (check-call node)
  (if (> (call-exits node) (call-arg-count node))
      (assertion-violation 'check-node "call node has more exits than arguments"))
  (if (trivial-primop-call? node)
      (check-trivial-primop-call node)
      (check-nontrivial-primop-call node)))

(define (check-trivial-primop-call node)
  (walk-vector (lambda (arg)
		 (if (not (yields-value? node))
		     (assertion-violation 'check-node "argument to trivial-primop call must yield value" arg))
		 (check-node arg))
	       (call-args node)))

(define (cont-lambda? node)
  (and (lambda-node? node)
       (eq? 'cont (lambda-type node))))

(define (call-primop-id node)
  (primop-id (call-primop node)))

(define (call-primop-name node)
  (symbol->string (primop-id (call-primop node))))

; check that first argument is a continuation variable
(define (check-cont-var node)
  (if (positive? (call-exits node))
      (assertion-violation 'check-node
			   (string-append (call-primop-name node)
					  " node has non-zero exit count")
			   node))
  (if (not (and (positive? (call-arg-count node))
		(reference-node? (call-arg node 0))))
      (assertion-violation 'check-node
			   (string-append (call-primop-name node)
					  " node must have cont var as first argument"
					  (call-arg node 0)))))

; check that the call has single continuation
(define (check-cont node)
  (if (not (= 1 (call-exits node)))
      (assertion-violation 'check-node
			   (string-append (call-primop-name node)
					  " node must have single continuation")
			   node))
  (if (not (and (positive? (call-arg-count node))
		(cont-lambda? (call-arg node 0))))
      (assertion-violation 'check-node
			   (string-append (symbol->string primop-id)
					  " node must have cont lambda as first argument" (call-arg node 0)))))
  

(define (check-nontrivial-primop-call node)
  (let ((exit-count (call-exits node))
	(arg-count (call-arg-count node))
	(primop-id (call-primop-id node)))
    
    (do ((i 0 (+ 1 i)))
	((= i arg-count))
      (let ((arg (call-arg node i)))
	(cond
	 ((< i exit-count)
	  (if (not (cont-lambda? arg))
	      (assertion-violation 'check-node "exit argument must be cont lambda" arg)))
	 ((not (yields-value? arg))
	  (assertion-violation 'check-node "regular call argument must yield value" arg)))
	(check-node arg)))

    (let ((check-proc-arg
	   (lambda ()
	     (if (< arg-count 2)
		 (assertion-violation 'check-node "call node must have >=2 arguments" node)))))

      (case primop-id
	((let)
	 (check-cont node)
	 (if (not (= (length (lambda-variables (call-arg node 0)))
		     (- arg-count 1)))
	     (assertion-violation 'check-node
				  "variable and value count don't match up in let node" node)))
	((letrec1)
	 (check-cont node)
	 (if (not (= 1 arg-count))
	     (assertion-violation 'check-node
				  "letrec1 node must have exactly 1 arg" node))
	 (let* ((cont (call-arg node 0))
		(cont-args (lambda-variables cont))
		(cont-arg-count (length cont-args))
		(next (lambda-body cont)))
	   (check-cont next)
	   (if (not (eq? 'letrec2 (call-primop-id next)))
	       (assertion-violation 'check-node
				    "letrec1 node must be followed by letrec2 node" node))
	   (if (zero? cont-arg-count)
	       (assertion-violation 'check-node
				    "letrec1 cont lambda must have at least one variable" node))
	   (if (not (= cont-arg-count
		       (- (call-arg-count next) 1)))
	       (assertion-violation 'check-node
				    "letrec1 and letrec2 nodes must have matching arity" node))
	   (let ((var (car cont-args)))
	     (if (not (= 1 (length (variable-refs var))))
		 (assertion-violation 'check-node
				      "letrec id variable must have exactly one reference" node))
	     (if (or (not (eq? next (node-parent (car (variable-refs var)))))
		     (not (= 1 (node-index (car (variable-refs var))))))
		 (assertion-violation 'check-node
				      "letrec id binding invalid" node)))))
	((call unknown-call)
	 (check-proc-arg)
	 (check-cont node))
	((tail-call unknown-tail-call)
	 (check-proc-arg)
	 (check-cont-var node))
	((return unknown-return)
	 (check-cont-var node))
	((jump)
	 (check-cont-var node)		; sort of
	 (let ((jump-target (get-lambda-value (call-arg node 0))))
	   (if (not (eq? 'jump (lambda-type jump-target)))
	       (assertion-violation 'check-node
				    "jump must go to jump lambda"
				    node jump-target))))))))

(define (check-reference ref)
  (let ((var (reference-variable ref)))
    (if (and (variable-binder var)
	     (not (variable-flag var)))
	(assertion-violation 'check-node
			     "unbound variable reference" ref))))

(define (check-literal node)
  (values)) ; nothing to check

(define (yields-value? node)
  (or (lambda-node? node)
      (and (call-node? node)
	   (trivial-primop-call? node))
      (literal-node? node)
      (reference-node? node)))
