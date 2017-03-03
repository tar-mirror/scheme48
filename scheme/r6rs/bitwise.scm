; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Harald Glab-Phlak, Mike Sperber

; R6RS bitwise operations

; Taken from the R6RS document.

(define (bitwise-if ei1 ei2 ei3)
  (bitwise-ior (bitwise-and ei1 ei2) 
	       (bitwise-and (bitwise-not ei1) ei3)))
			    

(define bitwise-arithmetic-shift arithmetic-shift)

(define bitwise-arithmetic-shift-left  bitwise-arithmetic-shift)
 
(define (bitwise-arithmetic-shift-right ei1 ei2)
  (bitwise-arithmetic-shift ei1 (- ei2)))
  
(define bitwise-bit-count bit-count)
  
(define (bitwise-bit-set? ei1 ei2) 
  (not (zero?
	(bitwise-and
	 (bitwise-arithmetic-shift-left 1 ei2)
	 ei1))))
  
(define (bitwise-bit-field ei1 ei2 ei3)
  (let ((mask
	 (bitwise-not
	  (bitwise-arithmetic-shift-left -1 ei3))))
    (bitwise-arithmetic-shift-right
     (bitwise-and ei1 mask)
     ei2)))

(define (bitwise-copy-bit ei1 ei2 ei3)
  (bitwise-if (bitwise-arithmetic-shift-left 1 ei2) 
	      (bitwise-arithmetic-shift-left ei3 ei2)
	      ei1))
  
(define (bitwise-copy-bit-field ei1 ei2 ei3 ei4)
  (bitwise-if (bitwise-and (bitwise-arithmetic-shift-left -1 ei2)
			   (bitwise-not 
			    (bitwise-arithmetic-shift-left -1 ei3)))
	      (bitwise-arithmetic-shift-left ei4 ei2)
	      ei1))
  
(define (bitwise-rotate-bit-field ei1 ei2 ei3 ei4)
  (let* ((n     ei1)
	 (start ei2)
	 (end   ei3)
	 (count ei4)
	 (width (- end start)))
    (if (positive? width)
	(let* ((count (remainder count width))
	       (field0
		(bitwise-bit-field n start end))
	       (field1 (bitwise-arithmetic-shift-left
			field0 count))
	       (field2 (bitwise-arithmetic-shift-right
			field0
			(- width count)))
	       (field (bitwise-ior field1 field2)))
	  (bitwise-copy-bit-field n start end field))
	n)))

(define (bitwise-reverse-bit-field ei1 ei2 ei3)
  (letrec* ((reverse-bit-field-recur 
	     (lambda (n1 n2 len)
	       (if (> len 0)
		   (reverse-bit-field-recur
		    (bitwise-arithmetic-shift-right n1 1) 
		    (bitwise-copy-bit (bitwise-arithmetic-shift-left n2 1) 0 n1)
		    (- len 1))
		   n2))))
	   (let ((width (- ei3 ei2)))
	     (if (positive? width)
		 (let ((field (bitwise-bit-field ei1 ei2 ei3)))
		   (bitwise-copy-bit-field
		    ei1 ei2 ei3 (reverse-bit-field-recur field 0 width)))
		 ei1))))

(define (bitwise-length ei)
  (do ((result 0 (+ result 1))
     (bits (if (negative? ei)
               (bitwise-not ei)
               ei)
           (bitwise-arithmetic-shift bits -1)))
    ((zero? bits)
     result)))

(define (bitwise-first-bit-set ei)
  (cond ((eq? ei 0) -1)
	((eq? (remainder ei 2) 1) 0)
	((eq? (remainder ei 2) 0) 
	 (let loop ((num ei) 
		    (count 0))
	   (if (or (eq? num 1) 
		   (eq? (remainder num 2) 1))
	       count
	       (loop (bitwise-arithmetic-shift-right num 1) (+ count 1)))))))
					    
	
      
