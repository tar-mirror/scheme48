;;; SRFI 45, which this code is part of, bears the following
;;; copyright/license notice:

;;; Copyright (C) André van Tonder (2003). All Rights Reserved.
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(define-record-type promise :promise
  (make-promise pair)
  promise?
  ;; This pair has as a car either 'EAGER (in which case the cdr is
  ;; the value) or 'LAZY (in which case the cdr is a thunk).
  (pair promise-ref set-promise!))

(define-record-discloser :promise
  (lambda (r)
    (case (car (promise-ref r))
      ((eager)
       (list 'promise 'eager (cdr (promise-ref r))))
      ((lazy)
       (list 'promise 'lazy)))))

;;;=========================================================================
;;; Primitives for lazy evaluation:

(define-syntax lazy
  (syntax-rules ()
    ((lazy exp)
     (make-promise (cons 'lazy (lambda () exp))))))

(define (eager x)
  (make-promise (cons 'eager x)))

(define-syntax delay
  (syntax-rules ()
    ((delay exp) (lazy (eager exp)))))

(define (force promise)
  (let ((content (promise-ref promise)))
    (case (car content)
      ((eager) (cdr content))
      ((lazy)  (let* ((promise* ((cdr content)))        
                      (content  (promise-ref promise)))                ; * 
                 (if (not (eqv? (car content) 'eager))                 ; *
                     (begin (set-car! content (car (promise-ref promise*)))
                            (set-cdr! content (cdr (promise-ref promise*)))
                            (set-promise! promise* content)))
                 (force promise))))))

; (*) These two lines re-fetch and check the original promise in case 
;     the first line of the let* caused it to be forced.  For an example  
;     where this happens, see reentrancy test 3 below.

