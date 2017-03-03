; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey


; Identifying values called by primops

; Is NODE the value being called by a primop?
                            
(define (procedure-node? node)
  (let ((parent (node-parent node)))
    (and (node? parent)
	 (let ((primop (call-primop parent)))
	   (and (primop-procedure? primop)
		(eq? (primop-call-index primop)
		     (node-index node)))))))

; Get the node called at CALL.

(define (called-procedure-node call)
  (cond ((and (primop-procedure? (call-primop call))
	      (primop-call-index (call-primop call)))
	 => (lambda (i)
	      (call-arg call i)))
	(else '#f)))
