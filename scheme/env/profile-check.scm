; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Marcel Turino, Manuel Dietrich

; Profiler tests

(define-test-suite profiler-tests)

(define (a-loop y)
  (+ 1
     (let loop ((i y))
       (if (> i 0)
	   (loop (- i 1))
	   0))))

(define (main-simple-loop x)
  (+ 1
     (a-loop x)
     (a-loop x)))


(define (a-rec y)
  (+ 1
     (if (> y 0)
	 (a-rec (- y 1))
	 0)))

(define (main-rec x)
  (+ 1
     (a-rec x)
     (a-rec x)))


(define (c-mutual x)
  (+ 1 x))

(define (b-mutual x)
  (let ((y (- x 1)))
    (if (> y 0)
	(begin
	  (a-mutual y)
	  (c-mutual (* 2 y))
	  (+ 1 (a-mutual y)))
	0)))

(define (a-mutual x)
  (let ((y (- x 1)))
    (if (> y 0)
	(begin
	  (b-mutual y)
	  (c-mutual y)
	  (+ 1 (b-mutual y)))
	0)))

(define (main-mutual x)
  (+ 1
     (a-mutual x)
     (a-mutual x)))


(define (a-exitcont cont x)
  (let ((y (- x 1)))
    (if (> y 0)
	(begin
	  (check-exception
	   (profile-thunk (make-empty-profile-data) (lambda () (main-exitcont 10))))
	  (cont 0))
	0)))

(define (main-exitcont cont x)
  (+ 1 (a-exitcont cont x)))


(define-test-case simple-loop profiler-tests
  
  (let ((prof-data (make-empty-profile-data))
	(blackhole (make-string-output-port)))

    ;; let it run
    (profile-thunk prof-data (lambda () (main-simple-loop 5000)) 1)

    (test-stability prof-data "a-loop")
    
    (check (profile-function-calls prof-data '("a-loop"))
	   => 2)
    (check (profile-function-calls prof-data '("loop" "a-loop"))
	   => 10002)
    (check (profile-function-reccalls prof-data '("loop"))
	   => 0) ; tail calls, this could fail when profiler enhances :(
    (check (profile-function-nonreccalls prof-data '("a-loop"))
	   => 2)
    ))



(define-test-case recursive profiler-tests
  
  (let ((prof-data (make-empty-profile-data))
	(blackhole (make-string-output-port)))

    ;; let it run
    (profile-thunk prof-data (lambda () (main-rec 500)) 50)

    (test-stability prof-data "a-rec")
    
    (check (profile-function-calls prof-data '("a-rec"))
	   => 1002)
    (check (profile-function-calls prof-data '("main-rec"))
	   => 1)
    (check (profile-function-reccalls prof-data '("a-rec" "profiler-test"))
	   => 1000)
    (check (profile-function-nonreccalls prof-data '("a-rec"))
	   => 2)
    ))


(define-test-case mutual profiler-tests
  
  (let ((prof-data (make-empty-profile-data))
	(blackhole (make-string-output-port)))

    ;; let it run
    (profile-thunk prof-data (lambda () (main-mutual 15)) 50 #f)

    (test-stability prof-data "a-mutual")
    
    (check (profile-function-calls prof-data '("a-mutual"))
	   => 43690)
    (check (profile-function-calls prof-data '("main-mutual"))
	   => 1)
    (check (profile-function-reccalls prof-data '("a-mutual" "profiler-test"))
	   => 0)
    (check (profile-function-nonreccalls prof-data '("a-mutual"))
	   => 43690)
    ))


(define-test-case exitcont profiler-tests
  
  (let ((prof-data (make-empty-profile-data))
	(blackhole (make-string-output-port)))

    ;; let it run
    (call-with-current-continuation
     (lambda (cont)
       (profile-thunk prof-data (lambda () (main-exitcont cont 22)) 50 #t)))
    
    (test-stability prof-data "a-exitcont")
    
    (check (profile-function-calls prof-data '("a-exitcont"))
	   => 1)
    (check (profile-function-calls prof-data '("main-exitcont"))
	   => 1)
    (check (profile-function-reccalls prof-data '("a-exitcont" "profiler-test"))
	   => 0)
    (check (profile-function-nonreccalls prof-data '("a-exitcont"))
	   => 1)
    ))

;; non-deterministic, but should at least not crash...
(define (test-stability prof-data funcname)
  (let ((blackhole (make-string-output-port)))
    
    (profile-display prof-data blackhole)
    (profile-display-overview prof-data blackhole)
    (profile-display-flat prof-data blackhole)
    (profile-display-tree prof-data blackhole)
    
    (profile-data-starttime prof-data)
    (profile-data-endtime prof-data)
    (profile-data-memoryuse prof-data)
    (profile-data-gcruns prof-data)
    (profile-data-runtime prof-data)
    (profile-data-samples prof-data)
    (profile-data-templates prof-data)
    (profile-data-cycles prof-data)
    (profile-data-root prof-data)

    (profile-display-function-flat prof-data '(funcname) blackhole)
    (profile-display-function-tree prof-data '(funcname "profiler-test" blackhole))
    (profile-display-function-tree prof-data "profiler-test" blackhole)

    (profile-display-function-cycle prof-data '(funcname) blackhole)	  
    ))
