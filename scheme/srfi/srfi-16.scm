; Copyright (C) Lars T Hansen (1999). All Rights Reserved.

; Permission is hereby granted, free of charge, to any person
; obtaining a copy of this software and associated documentation files
; (the "Software"), to deal in the Software without restriction,
; including without limitation the rights to use, copy, modify, merge,
; publish, distribute, sublicense, and/or sell copies of the Software,
; and to permit persons to whom the Software is furnished to do so,
; subject to the following conditions:

; The above copyright notice and this permission notice shall be
; included in all copies or substantial portions of the Software.

; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

(define-syntax case-lambda
  (syntax-rules ()
    ((case-lambda 
      (?a1 ?e1 ...) 
      ?clause1 ...)
     (lambda args
       (let ((l (length args)))
         (case-lambda "CLAUSE" args l 
           (?a1 ?e1 ...)
           ?clause1 ...))))
    ((case-lambda "CLAUSE" ?args ?l 
      ((?a1 ...) ?e1 ...) 
      ?clause1 ...)
     (if (= ?l (length '(?a1 ...)))
         (apply (lambda (?a1 ...) ?e1 ...) ?args)
         (case-lambda "CLAUSE" ?args ?l 
           ?clause1 ...)))
    ((case-lambda "CLAUSE" ?args ?l
      ((?a1 . ?ar) ?e1 ...) 
      ?clause1 ...)
     (case-lambda "IMPROPER" ?args ?l 1 (?a1 . ?ar) (?ar ?e1 ...) 
       ?clause1 ...))
    ((case-lambda "CLAUSE" ?args ?l 
      (?a1 ?e1 ...)
      ?clause1 ...)
     (let ((?a1 ?args))
       ?e1 ...))
    ((case-lambda "CLAUSE" ?args ?l)
     (assertion-violation 'case-lambda "Wrong number of arguments to CASE-LAMBDA."))
    ((case-lambda "IMPROPER" ?args ?l ?k ?al ((?a1 . ?ar) ?e1 ...)
      ?clause1 ...)
     (case-lambda "IMPROPER" ?args ?l (+ ?k 1) ?al (?ar ?e1 ...) 
      ?clause1 ...))
    ((case-lambda "IMPROPER" ?args ?l ?k ?al (?ar ?e1 ...) 
      ?clause1 ...)
     (if (>= ?l ?k)
         (apply (lambda ?al ?e1 ...) ?args)
         (case-lambda "CLAUSE" ?args ?l 
           ?clause1 ...)))))
