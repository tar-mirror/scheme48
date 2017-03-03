; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey


(define (test)
  (let* ((b (allocate-memory 16))
	 (res (read-block (current-input-port)
			  b
			  16
			  (lambda (okay? eof? got)
			    (if (or (not okay?)
				    eof?)
				-1
				(write-block (current-output-port)
					     b
					     got
					     (lambda (okay? sent)
					       (if okay? sent -1))))))))
    (deallocate-memory b)
    res))
