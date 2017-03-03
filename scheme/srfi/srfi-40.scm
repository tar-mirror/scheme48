;;; STREAM -- LIBRARY OF SYNTAX AND FUNCTIONS TO MANIPULATE STREAMS

;;; A stream is a new data type, disjoint from all other data types, that
;;; contains a promise that, when forced, is either nil (a single object
;;; distinguishable from all other objects) or consists of an object
;;; (the stream element) followed by a stream.  Each stream element is
;;; evaluated exactly once, when it is first retrieved (not when it is
;;; created); once evaluated its value is saved to be returned by
;;; subsequent retrievals without being evaluated again.

;;; Copyright (C) 2003 by Philip L. Bewig of Saint Louis, Missouri,
;;; United States of America. All rights reserved.
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

;;; PROMISES A LA SRFI-45:

;;; A separate implementation is necessary to
;;; have promises that answer #t to stream?
;;; This requires lots of complicated type conversions.

(define-record-type stream-promise :stream-promise
  (make-s:promise kind content)
  s:promise?
  (kind    s:promise-kind    set-s:promise-kind!)
  (content s:promise-content set-s:promise-content!))

(define-syntax lazy
  (syntax-rules ()
    ((lazy exp)
     (make-cell (make-s:promise 'lazy (lambda () exp))))))

(define (eager x)
  (make-stream (make-cell (make-s:promise 'eager x))))

(define-syntax delay
  (syntax-rules ()
    ((delay exp) (lazy (eager exp)))))

(define (force promise)
  (let ((content (cell-ref promise)))
    (case (s:promise-kind content)
      ((eager) (s:promise-content content))
      ((lazy)
       (let* ((promise* (stream-promise ((s:promise-content content))))
              (content  (cell-ref promise)))
         (if (not (eqv? 'eager (s:promise-kind content)))
             (begin
               (set-s:promise-kind! content (s:promise-kind (cell-ref promise*)))
               (set-s:promise-content! content (s:promise-content (cell-ref promise*)))
               (cell-set! promise* content)))
         (force promise))))))

;; STREAM-TYPE -- type of streams
;; STREAM? object -- #t if object is a stream, #f otherwise
(define-record-type stream :stream
  (make-stream promise)
  stream?
  (promise stream-promise))

;;; UTILITY FUNCTIONS

;; STREAM-ERROR message -- print message then abort execution
;  replace this with a call to the native error handler
;  if stream-error returns, so will the stream library function that called it
(define stream-error error)

;;; STREAM SYNTAX AND FUNCTIONS

;; STREAM-NULL -- the distinguished nil stream
(define stream-null (make-stream (delay '())))

;; STREAM-CONS object stream -- primitive constructor of streams
(define-syntax stream-cons
  (syntax-rules ()
    ((stream-cons obj strm)
     (make-stream
      (delay
       (if (not (stream? strm))
           (stream-error "attempt to stream-cons onto non-stream")
           (cons obj strm)))))))

;; STREAM-NULL? object -- #t if object is the null stream, #f otherwise
(define (stream-null? obj)
  (and (stream? obj) (null? (force (stream-promise obj)))))

;; STREAM-PAIR? object -- #t if object is a non-null stream, #f otherwise
(define (stream-pair? obj)
  (and (stream? obj) (not (null? (force (stream-promise obj))))))

;; STREAM-CAR stream -- first element of stream
(define (stream-car strm)
  (cond ((not (stream? strm)) (stream-error "attempt to take stream-car of non-stream"))
        ((stream-null? strm)  (stream-error "attempt to take stream-car of null stream"))
        (else (car (force (stream-promise strm))))))

;; STREAM-CDR stream -- remaining elements of stream after first
(define (stream-cdr strm)
  (cond ((not (stream? strm)) (stream-error "attempt to take stream-cdr of non-stream"))
        ((stream-null? strm)  (stream-error "attempt to take stream-cdr of null stream"))
        (else (cdr (force (stream-promise strm))))))

;; STREAM-DELAY object -- the essential stream mechanism
(define-syntax stream-delay
  (syntax-rules ()
    ((stream-delay expr)
      (make-stream
        (lazy expr)))))

;; STREAM object ... -- new stream whose elements are object ...
(define (stream . objs)
  (let loop ((objs objs))
    (stream-delay
      (if (null? objs)
          stream-null
          (stream-cons (car objs) (loop (cdr objs)))))))

;; STREAM-UNFOLDN generator seed n -- n+1 streams from (generator seed)
(define (stream-unfoldn gen seed n)
  (define (unfold-result-stream gen seed)
    (let loop ((seed seed))
      (stream-delay
        (call-with-values
          (lambda () (gen seed))
          (lambda (next . results)
            (stream-cons results (loop next)))))))
  (define (result-stream->output-stream result-stream i)
    (stream-delay
      (let ((result (list-ref (stream-car result-stream) i)))
        (cond ((pair? result)
                (stream-cons (car result)
                             (result-stream->output-stream
                               (stream-cdr result-stream) i)))
              ((not result)
                (result-stream->output-stream (stream-cdr result-stream) i))
              ((null? result) stream-null)
              (else (stream-error "can't happen"))))))
  (define (result-stream->output-streams result-stream n)
    (let loop ((i 0) (outputs '()))
      (if (= i n)
        (apply values (reverse outputs))
        (loop (+ i 1)
              (cons (result-stream->output-stream result-stream i)
                    outputs)))))
  (result-stream->output-streams (unfold-result-stream gen seed) n))

;; STREAM-MAP func stream ... -- stream produced by applying func element-wise
(define (stream-map func . strms)
  (cond ((not (procedure? func)) (stream-error "non-functional argument to stream-map"))
        ((null? strms) (stream-error "no stream arguments to stream-map"))
        ((not (every stream? strms)) (stream-error "non-stream argument to stream-map"))
        (else (let loop ((strms strms))
                (stream-delay
                  (if (any stream-null? strms)
                      stream-null
                      (stream-cons (apply func (map stream-car strms))
                                   (loop (map stream-cdr strms)))))))))

;; STREAM-FOR-EACH proc stream ... -- apply proc element-wise for side-effects
(define (stream-for-each proc . strms)
  (cond ((not (procedure? proc)) (stream-error "non-functional argument to stream-for-each"))
        ((null? strms) (stream-error "no stream arguments to stream-for-each"))
        ((not (every stream? strms)) (stream-error "non-stream argument to stream-for-each"))
        (else (let loop ((strms strms))
                (if (not (any stream-null? strms))
                    (begin (apply proc (map stream-car strms))
                           (loop (map stream-cdr strms))))))))
    
;; STREAM-FILTER pred? stream -- new stream including only items passing pred?
(define (stream-filter pred? strm)
  (cond ((not (procedure? pred?)) (stream-error "non-functional argument to stream-filter"))
        ((not (stream? strm)) (stream-error "attempt to apply stream-filter to non-stream"))
        (else (stream-unfoldn
                (lambda (s)
                  (values
                    (stream-cdr s)
                    (cond ((stream-null? s) '())
                          ((pred? (stream-car s)) (list (stream-car s)))
                          (else #f))))
                strm
                1))))
