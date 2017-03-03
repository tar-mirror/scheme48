; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber

; Time

(import-dynamic-externals "=scheme48external/posix")

;----------------
; Time - seconds since the epoch.

(define-record-type time :time
  (make-time seconds)
  time?
  (seconds time-seconds))

(define-record-discloser :time
  (lambda (time)
    (let ((string (time->string time)))
      (list 'time (substring string 0 (- (string-length string) 1))))))

; We need to make these in the outside world.
(define-exported-binding "posix-time-type" :time)

(define (time=? time1 time2)
  (= (time-seconds time1)
     (time-seconds time2)))

(define (time<? time1 time2)
  (< (time-seconds time1)
     (time-seconds time2)))

(define (time<=? time1 time2)
  (not (time<? time2 time1)))
      
(define (time>? time1 time2)
  (time<? time2 time1))

(define (time>=? time1 time2)
  (not (time<? time1 time2)))

(import-lambda-definition-2 current-time () "posix_time")
(import-lambda-definition-2 posix-time->string (time) "posix_ctime")

(define (time->string t)
  (os-string->string
   (byte-vector->os-string
    (posix-time->string t))))

;----------------
; Dates - what a mess.

(define-record-type date :date
  (make-date second minute hour month-day month year week-day year-day dst)
  date?
  (second    date-second)
  (minute    date-minute)
  (hour      date-hour)
  (month-day date-month-day)
  (month     date-month)
  (year      date-year)		; Since 1900 (why?)
  (week-day  date-week-day)
  (year-day  date-year-day)
  (dst	     date-dst) ; #t, #f or unspecific
  ; (time-zone date-time-zone) ; maybe later
  )

(define-record-discloser :date
  (lambda (r)
    (list 'date
	  (let ((s (date->string r)))
	    (substring s 0 (- (string-length s) 1))))))

; the C interface sees date objects as vectors
(define (vector->date v)
  (apply make-date (vector->list v)))

(define (date->vector d)
  (vector (date-second d)
	  (date-minute d)
	  (date-hour d)
	  (date-month-day d)
	  (date-month d)
	  (date-year d)
	  (date-week-day d)
	  (date-year-day d)
	  (date-dst d)))

(import-lambda-definition-2 posix-date->string (date) "posix_asctime")
(import-lambda-definition-2 posix-time->utc-date (time) "posix_gmtime")
(import-lambda-definition-2 posix-time->local-date (time) "posix_localtime")
(import-lambda-definition-2 posix-date->time (date) "posix_mktime")
(import-lambda-definition-2 posix-strftime (format date) "posix_strftime")

(define (date->string d)
  (os-string->string
   (byte-vector->os-string
    (posix-date->string (date->vector d)))))

(define (time->utc-date t)
  (vector->date (posix-time->utc-date t)))

(define (time->local-date t)
  (vector->date (posix-time->local-date t)))

(define (date->time d)
  (posix-date->time (date->vector d)))

(define (format-date format d)
  (os-string->string 
   (byte-vector->os-string
    (posix-strftime (x->os-byte-vector format) (date->vector d)))))

