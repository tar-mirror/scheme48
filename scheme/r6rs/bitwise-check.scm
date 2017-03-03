; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Harald Glab-Phlak, Mike Sperber

; Rudimentary tests

(define-test-suite r6rs-bitwise-tests)

(define procs (list bitwise-not
		    bitwise-and
		    bitwise-ior
		    bitwise-xor
		    bitwise-if
		    bitwise-bit-count
		    bitwise-length
		    bitwise-first-bit-set
		    bitwise-bit-set?
		    bitwise-copy-bit
		    bitwise-bit-field
		    bitwise-copy-bit-field
		    bitwise-arithmetic-shift
		    bitwise-arithmetic-shift-left
		    bitwise-arithmetic-shift-right
		    bitwise-rotate-bit-field
		    bitwise-reverse-bit-field))
(define args (list '(7) ;not 
		   '(#b11110000 #b01010101) ;and
		   '(#b10101010 #b01010101) ;ior
		   '(#b11110000 #b01010101) ;xor
		   '(#b00110011 0 #b00110011) ;if
		   '(28) ;count of bits set
		   '(461) ;length needed to represent the value
		   '(8) ;first-bit-set
		   '(13 1) ;bit is set
		   '(#b00001100  6 1) ;copy-bit
		   '( #b00001100 #b00000010 #b00000101) ;bit-field
		   '( #b11001101 #b00000111  #b01110000 #b00000011 ) ;copy-bit-field
		   '(2 1) ;shift (left)
		   '(2 1) ;shift-left
		   '(4 1) ;shift-right
		   '(#b00001100 #b00000010 #b00000101 #b00000100) ;rotate
		   '( #b1010010 1 4))) ;reverse-bit-field
(define reslist  '(-8 80 255 165 0 3 9 3 #f 76 3 461 4 4 2 24 88))

(define-test-case smoke-tests r6rs-bitwise-tests
  (define do-it-all 
    (lambda (l1 l2 l3 )
      (if (pair? l1)
	  (check (apply (car l1) (car l2)) => (car l3))
	  (do-it-all (cdr l1) (cdr l2) (cdr l3)))))
  (do-it-all procs args reslist))

(define-test-case basic-checks r6rs-bitwise-tests
  (check (bitwise-first-bit-set 0) => -1)
  (check (bitwise-first-bit-set 1) => 0)
  (check (bitwise-first-bit-set 2) => 1)
  (check (bitwise-first-bit-set 4) => 2)
  (check (bitwise-first-bit-set 6) => 1)
  (check (bitwise-first-bit-set 8) => 3)
  (check (bitwise-arithmetic-shift-left 2 1) => 4)
  (check (bitwise-arithmetic-shift-right 4 1) => 2)
  (check (bitwise-bit-set? 13 0) => #t)
  (check (bitwise-bit-set? 13 1) => #f)
  (check (bitwise-bit-set? 13 2) => #t)
  (check (bitwise-bit-set? 13 3) => #t)
  (check (bitwise-bit-field #b00001100 #b00000010 #b00000101) => 3)
  (check (bitwise-reverse-bit-field #b1010010 1 4) => 88) ; #b1011000
  (check (bitwise-not 7) => -8)
  (check (bitwise-ior #b10101010 #b01010101) => #b11111111)
  (check (bitwise-xor #b11110000 #b01010101) => #b10100101)
  (check (bitwise-and #b11110000 #b01010101) => #b01010000)
  (check (apply (car procs) '(7)) => -8))

(define-test-case bw-if/bw-rotf/copy r6rs-bitwise-tests
  (letrec* ((do-the-if (lambda(element includes excludes)
			(if (not (eq? (bitwise-if element includes excludes) 0)) 
			    #t 
			    #f)))
	    (if-proc (lambda (if-do if-else parm)
		   (lambda(element includes excludes)
		     (if (do-the-if element includes excludes)
			 (if-do parm)
			 (if-else parm)))))
	(do-proc (lambda(parm) (cons 'if-done parm)))
	(else-proc (lambda(parm) (cons 'else-done parm))))
	   (check ((if-proc do-proc else-proc 5) 
		   #b00110011 0 #b00110011) 
		  =>(cons 'else-done 5))
	   
	   (check ((if-proc do-proc else-proc 5) 
		   #b00110010 0 #b00110011) 
		  =>(cons 'if-done 5))
	   
	   (check ((if-proc do-proc else-proc 5) 
		   #b00110011 #b00110011 #b00110011) 
		  =>(cons 'if-done 5))
	   
	   (check ((if-proc do-proc else-proc 5) 
		   #b00110011 1 0) 
		  =>(cons 'if-done 5))
	   
	   (check ((if-proc do-proc else-proc 5) 
		   #b00110011 0 1) 
		  =>(cons 'else-done 5)))
  (check (bitwise-rotate-bit-field  #b00001100 #b00000010 #b00000101 #b00000100) => #b00011000)
  (check (bitwise-rotate-bit-field  #b00001100 #b00000010 #b00000101 #b00000101) => #b00010100)
  (check (bitwise-rotate-bit-field  #b00001100 #b00000010 #b00000101 #b00010101) => #b00001100)
  (check (bitwise-copy-bit-field #b11001101 #b00000111  #b01110000 #b00000011 ) =>  #b111001101))
