; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Mike Sperber

; Placeholders (single-assignment cells for use with threads)

(define-synchronized-record-type placeholder :placeholder
  (really-make-placeholder value queue id)
  (value queue)				; synchronize on this
  placeholder?
  (queue placeholder-queue set-placeholder-queue!) ; #f means VALUE has been set
  (value placeholder-real-value set-placeholder-value!)
  (id placeholder-id))

(define-record-discloser :placeholder
  (lambda (placeholder)
    (cons 'placeholder
	  (if (placeholder-id placeholder)
	      (list (placeholder-id placeholder))
	      '()))))

(define (make-placeholder . id-option)
  (really-make-placeholder (unspecific)
			   (make-queue)
			   (if (null? id-option) #f (car id-option))))

(define (placeholder-value placeholder . maybe-deadlock?)
  (with-new-proposal (lose)
    (let ((queue (placeholder-queue placeholder)))
      (if queue
	  (or (apply maybe-commit-and-block-on-queue queue maybe-deadlock?)
	      (lose)))))
  (placeholder-real-value placeholder))

(define (placeholder-set! placeholder new-value)
  (with-new-proposal (lose)
    (let ((queue (placeholder-queue placeholder)))
      (cond (queue
	     (set-placeholder-value! placeholder new-value)
	     (set-placeholder-queue! placeholder #f)
	     (or (maybe-commit-and-make-ready queue)
		 (lose)))
	    (else
	     ;; We only read queue and value and they are set atomically,
	     ;; so there is no need to commit here.
	     (assertion-violation 'placeholder-set!
				  "placeholder is already assigned"
				  placeholder
				  (placeholder-real-value placeholder)))))))
