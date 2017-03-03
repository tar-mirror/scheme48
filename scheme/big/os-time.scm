; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Michael Zabka


(import-lambda-definition-2 current-utc-time () "s48_get_current_time")
(import-lambda-definition-2 timezone-offset () "s48_get_timezone")

(define-record-type time :time
  (make-time seconds microseconds)
  time?
  (seconds time-seconds)
  (microseconds time-microseconds))

(define-exported-binding "os-time-type" :time)

(define (time=? time1 time2)
  (and 
   (= (time-seconds time1)
      (time-seconds time2))
   (= (time-microseconds time1)
      (time-microseconds time2))))

(define (time<? time1 time2)
  (if (< (time-seconds time1)
         (time-seconds time2))
      (< (time-microseconds time1)
         (time-microseconds time2))))

(define (time<=? time1 time2)
  (not (time<? time2 time1)))
      
(define (time>? time1 time2)
  (time<? time2 time1))

(define (time>=? time1 time2)
  (not (time<? time1 time2)))
