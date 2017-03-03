; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Harald Glab-Phlak, Mike Sperber

(define (bytevector:nan? x)
  (and (real? x)
       (not (= x x))))

(define (bytevector:infinite? x)
  (and (real? x)
       (not (bytevector:nan? x))
       (bytevector:nan? (- x x))))

;exported stuff
(define (bytevector-ieee-single-native-ref bytevector k)
  (r6rs-bytevect->float bytevector k))

(define (bytevector-ieee-double-native-ref bytevector k)
  (r6rs-bytevect->double bytevector k))


(define (bytevector-ieee-single-ref bytevector k endness)
  (if (eq? endness (native-endianness))
      (if (= 0 (remainder k 4))
          (bytevector-ieee-single-native-ref bytevector k)
          (let ((b (make-bytevector 4)))
            (bytevector-copy! bytevector k b 0 4)
            (bytevector-ieee-single-native-ref b 0)))
      (let ((b (make-bytevector 4)))
        (bytevector-u8-set! b 0 (bytevector-u8-ref bytevector (+ k 3)))
        (bytevector-u8-set! b 1 (bytevector-u8-ref bytevector (+ k 2)))
        (bytevector-u8-set! b 2 (bytevector-u8-ref bytevector (+ k 1)))
        (bytevector-u8-set! b 3 (bytevector-u8-ref bytevector k))
        (bytevector-ieee-single-native-ref b 0))))

(define (bytevector-ieee-double-ref bytevector k endness)
  (if (eq? endness (native-endianness))
      (if (= 0 (remainder k 8))
          (bytevector-ieee-double-native-ref bytevector k)
          (let ((b (make-bytevector 8)))
            (bytevector-copy! bytevector k b 0 8)
            (bytevector-ieee-double-native-ref b 0)))
      (let ((b (make-bytevector 8)))
        (bytevector-u8-set! b 0 (bytevector-u8-ref bytevector (+ k 7)))
        (bytevector-u8-set! b 1 (bytevector-u8-ref bytevector (+ k 6)))
        (bytevector-u8-set! b 2 (bytevector-u8-ref bytevector (+ k 5)))
        (bytevector-u8-set! b 3 (bytevector-u8-ref bytevector (+ k 4)))
        (bytevector-u8-set! b 4 (bytevector-u8-ref bytevector (+ k 3)))
        (bytevector-u8-set! b 5 (bytevector-u8-ref bytevector (+ k 2)))
        (bytevector-u8-set! b 6 (bytevector-u8-ref bytevector (+ k 1)))
        (bytevector-u8-set! b 7 (bytevector-u8-ref bytevector k))
        (bytevector-ieee-double-native-ref b 0))))

(define (bytevector-ieee-single-native-set! bytevector k x)
  (r6rs-float->bytevect!  x bytevector k))

(define (bytevector-ieee-double-native-set! bytevector k x)
  (r6rs-double->bytevect!  x bytevector k))

(define (bytevector-ieee-single-set! bytevector k x endness)
  (if (eq? endness (native-endianness))
      (if (= 0 (remainder k 4))
          (bytevector-ieee-single-native-set! bytevector k x)
          (let ((b (make-bytevector 4)))
            (bytevector-ieee-single-native-set! b 0 x)
            (bytevector-copy! b 0 bytevector k 4)))
      (let ((b (make-bytevector 4)))
        (bytevector-ieee-single-native-set! b 0 x)
        (bytevector-u8-set! bytevector k (bytevector-u8-ref b 3))
        (bytevector-u8-set! bytevector (+ k 1) (bytevector-u8-ref b 2))
        (bytevector-u8-set! bytevector (+ k 2) (bytevector-u8-ref b 1))
        (bytevector-u8-set! bytevector (+ k 3) (bytevector-u8-ref b 0)))))

(define (bytevector-ieee-double-set! bytevector k x endness)
  (if (eq? endness (native-endianness))
      (if (= 0 (remainder k 8))
          (bytevector-ieee-double-native-set! bytevector k x)
          (let ((b (make-bytevector 8)))
            (bytevector-ieee-double-native-set! b 0 x)
            (bytevector-copy! b 0 bytevector k 8)))
      (let ((b (make-bytevector 8)))
        (bytevector-ieee-double-native-set! b 0 x)
        (bytevector-u8-set! bytevector k (bytevector-u8-ref b 7))
        (bytevector-u8-set! bytevector (+ k 1) (bytevector-u8-ref b 6))
        (bytevector-u8-set! bytevector (+ k 2) (bytevector-u8-ref b 5))
        (bytevector-u8-set! bytevector (+ k 3) (bytevector-u8-ref b 4))
        (bytevector-u8-set! bytevector (+ k 4) (bytevector-u8-ref b 3))
        (bytevector-u8-set! bytevector (+ k 5) (bytevector-u8-ref b 2))
        (bytevector-u8-set! bytevector (+ k 6) (bytevector-u8-ref b 1))
        (bytevector-u8-set! bytevector (+ k 7) (bytevector-u8-ref b 0)))))


(define (r6rs-float->bytevect!  float bytevect index)
  (external-r6rs-float->bytevect! float bytevect index))

(define (r6rs-bytevect->float bytevect index)
  (external-r6rs-bytevect->float bytevect index))

(define (r6rs-double->bytevect!  double bytevect index)
  (external-r6rs-double->bytevect! double bytevect index))

(define (r6rs-bytevect->double bytevect index)
  (external-r6rs-bytevect->double bytevect index))


;; external fun definition

(import-lambda-definition-2 external-r6rs-float->bytevect!
			    (double bytevect index)
			    "r6rs_float_to_bytevect")

(import-lambda-definition-2 external-r6rs-bytevect->float
			    (bytevect index)
			    "r6rs_bytevect_to_float")

(import-lambda-definition-2 external-r6rs-double->bytevect!
			    (double bytevect index)
			    "r6rs_double_to_bytevect")

(import-lambda-definition-2 external-r6rs-bytevect->double
			    (bytevect index)
			    "r6rs_bytevect_to_double")

