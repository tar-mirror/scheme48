; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Timo Harter, Mike Sperber

; Disassembler that uses the VM's data structures.

;(define (disassemble stuff . no-subtemplates)
;  (let ((template (cond ((template? stuff) stuff)
;                        ((closure? stuff) (closure-template stuff))
;                        ((and (location? stuff)
;                              (closure? (contents stuff)))
;                         (closure-template (contents stuff)))
;                        (else
;                         (error "cannot coerce to template" stuff)))))
;    (really-disassemble template
;                        0
;                        (if (null? no-subtemplates)
;                            #f
;                            (car no-subtemplates)))
;    (newline)))

(define (disassemble code-pointer)
  (really-disassemble code-pointer 0 #f))

(define (really-disassemble code level write-templates?)
  (let loop ((pc 0))
    (if (< pc (code-vector-length code))
        (loop (write-instruction code pc level write-templates?)))))

(define (newline-indent n)
  (newline)
  (do ((i n (- i 1)))
      ((= i 0))
    (display #\space)))

(define (write-pc pc)
  (if (< pc 100) (display " "))
  (if (< pc 10) (display " "))
  (write pc))

(define (write-instruction code pc level write-sub-templates?)
  (let ((opcode (code-vector-ref code pc)))
    (newline-indent (* level 3))
    (write-pc pc)
    (display " (")
    (write (enumerand->name opcode op))
    (let ((pc (cond ((= opcode (enum op computed-goto))
 		     (display-computed-goto pc code))
 		    ((or (= opcode (enum op make-flat-env))
			 (= opcode (enum op make-big-flat-env)))
 		     (display-flat-env pc code))
		    ((= opcode (enum op protocol))
		     (display-protocol pc code))
		    ((= opcode (enum op cont-data))
		     (+ pc (get-offset (+ pc 1) code)))
 		    (else
 		     (print-opcode-args opcode (+ pc 1) code
 					level write-sub-templates?)))))
      (display #\))
      pc)))

(define (display-computed-goto start-pc code)
  (display #\space)
  (let ((count (code-vector-ref code (+ start-pc 1))))
    (write count)
    (do ((pc (+ start-pc 2) (+ pc 2))
	 (count count (- count 1)))
	((= count 0) pc)
      (display #\space)
      (write `(=> ,(+ start-pc (get-offset pc code)))))))


; skip make-flat-env or make-big-flat-env
; pc must point to the opcode, returns pc of next opcode
;
(define (skip-flat-env code pc)
 (let* ((big?     (= (code-vector-ref code pc) (enum op make-big-flat-env)))
	(size     (if big? 2 1))
	(fetch    (if big? (lambda (code pc) (get-offset pc code)) code-vector-ref))
	(offset   (+ pc 1)) ; skip opcode

	(total    (fetch code offset))               ; # of values (total)
	(closures (fetch code (+ offset size)))      ; # of closures
	(offset   (+ offset size (* size closures))) ; skip template offset + offsets of templates in template

	(vars     (fetch code offset))      ; # of vars in frame
	(offset   (+ offset (* size vars))) ; skip offsets of vars in frame

	(envs     (- total (+ closures vars))))  ; # of envs in frame
    (let loop ((offset offset)                   ; skip env information
               (env envs))
      (if (= env 0)
        offset  ; position of next opcode
        (let ((env-vars (fetch code (+ offset size)))) ; # of vars in env
          (loop 
            (+ offset size size (* size env-vars)) ; skip env offset, # of vars, offsets of vars in env
            (- env 1)))))))
        

(define (display-flat-env pc code)
  (let ((total-count (code-vector-ref code (+ pc 1))))
    (display #\space) (write total-count) (display " ...")
    (skip-flat-env code pc)))

;    (let loop ((pc (+ pc 2)) (count 0) (old-back 0))
;      (if (= count total-count)
;          pc
;          (let ((back (+ (code-vector-ref code pc)
;                         old-back))
;                (limit (+ pc 2 (code-vector-ref code (+ pc 1)))))
;            (do ((pc (+ pc 2) (+ pc 1))
;                 (count count (+ count 1))
;                 (offsets '() (cons (code-vector-ref code pc) offsets)))
;                ((= pc limit)
;                 (display #\space)
;                 (write `(,back ,(reverse offsets)))
;		 (loop pc count back))))))))

(define (display-protocol pc code)
  (let ((protocol (code-vector-ref code (+ pc 1))))
    (display #\space)
    (+ pc (cond ((<= protocol maximum-stack-args)
		 (display protocol)
		 (if (= pc 0) 3 2))
		((= protocol two-byte-nargs-protocol)
		 (display (get-offset (+ pc 2) code))
		 (if (= pc 0) 5 4))
		((= protocol two-byte-nargs+list-protocol)
		 (display (get-offset (+ pc 2) code))
		 (display "+")
		 (if (= pc 0) 5 4))
		((= protocol args+nargs-protocol)
		 (display "args+nargs")
		 2)
		((= protocol ignore-values-protocol)
		 (display "discard all values")
		 2)
		((= protocol call-with-values-protocol)
		 (display "call-with-values ")
		 (write `(=> ,(+ pc (get-offset (+ pc 2) code))))
		 4)
		((= protocol nary-dispatch-protocol)
		 (display "nary-dispatch")
		 (do ((i 0 (+ i 1)))
		     ((= i 4))
		   (let ((offset (code-vector-ref code (+ pc 2 i))))
		     (if (not (= offset 0))
			 (begin
			   (display #\space)
			   (display (list (if (= i 3) "3+" i)
					  '=>
					  (+ pc offset)))))))
		 5)
		(else
		 (error "unknown protocol" protocol))))))

(define (print-opcode-args op pc code level write-templates?)
  (let ((specs (vector-ref opcode-arg-specs op)))
    (let loop ((specs specs) (pc pc))
      (cond ((or (null? specs)
		 (= 0 (arg-spec-size (car specs))))
	     pc)
	    (else
	     (display #\space)
	     (print-opcode-arg specs pc code level write-templates?)
	     (loop (cdr specs) (+ pc (arg-spec-size (car specs)))))))))

(define (arg-spec-size spec)
  (case spec
    ((nargs byte stob literal index stack-index) 1)
    ((offset two-bytes two-byte-index two-byte-stack-index) 2)
    (else 0)))

(define (print-opcode-arg specs pc code level write-templates?)
  (case (car specs)
    ((nargs byte stack-index)
     (write (code-vector-ref code pc)))
    ((literal)
     (write (- (code-vector-ref code pc) 128)))
    ((two-bytes two-byte-stack-index)
     (write (get-offset pc code)))
    ((index)
     (write (code-vector-ref code pc)))
;     (let ((thing (template-ref template (code-vector-ref code pc))))
;       (write-literal-thing thing level write-templates?))
    ((two-byte-index)
     (write (code-vector-ref code pc)))
;     (let ((thing (template-ref template (get-offset pc code))))
;       (write-literal-thing thing level write-templates?))
    ((offset)
     (write `(=> ,(+ pc -1 (get-offset pc code)))))  ; -1 to back up over opcode
    ((stob)
     (write (enumerand->name (code-vector-ref code pc) stob)))))

(define (get-offset pc code)
  (+ (* (code-vector-ref code pc)
	byte-limit)
     (code-vector-ref code (+ pc 1))))

(define (write-literal-thing thing level write-templates?)
  (cond ((location? thing)
	 (write `(location ,thing ,(location-id thing))))
	((not (template? thing))
	 (display #\')
	 (write thing))
	(write-templates?
	 (really-disassemble thing (+ level 1) #t))
	(else
	 (display "..."))))


