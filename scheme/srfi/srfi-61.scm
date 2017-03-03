; SRFI 61 reference implementation

; Copyright (C) 2004 Taylor Campbell. All rights reserved.
; 
; Permission is hereby granted, free of charge, to any person
; obtaining a copy of this software and associated documentation files
; (the "Software"), to deal in the Software without restriction,
; including without limitation the rights to use, copy, modify, merge,
; publish, distribute, sublicense, and/or sell copies of the Software,
; and to permit persons to whom the Software is furnished to do so,
; subject to the following conditions:
; 
; The above copyright notice and this permission notice shall be
; included in all copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

(define-syntax cond
  (syntax-rules (=> ELSE)

    ((COND (ELSE else1 else2 ...))
     ;; The (IF #T (BEGIN ...)) wrapper ensures that there may be no
     ;; internal definitions in the body of the clause.  R5RS mandates
     ;; this in text (by referring to each subform of the clauses as
     ;; <expression>) but not in its reference implementation of COND,
     ;; which just expands to (BEGIN ...) with no (IF #T ...) wrapper.
     (IF #T (BEGIN else1 else2 ...)))

    ((COND (test => receiver) more-clause ...)
     (LET ((T test))
       (COND/MAYBE-MORE T
                        (receiver T)
                        more-clause ...)))

    ((COND (generator guard => receiver) more-clause ...)
     (CALL-WITH-VALUES (LAMBDA () generator)
       (LAMBDA T
         (COND/MAYBE-MORE (APPLY guard    T)
                          (APPLY receiver T)
                          more-clause ...))))

    ((COND (test) more-clause ...)
     (LET ((T test))
       (COND/MAYBE-MORE T T more-clause ...)))

    ((COND (test body1 body2 ...) more-clause ...)
     (COND/MAYBE-MORE test
                      (BEGIN body1 body2 ...)
                      more-clause ...))))

(define-syntax cond/maybe-more
  (syntax-rules ()
    ((COND/MAYBE-MORE test consequent)
     (IF test
         consequent))
    ((COND/MAYBE-MORE test consequent clause ...)
     (IF test
         consequent
         (COND clause ...)))))
