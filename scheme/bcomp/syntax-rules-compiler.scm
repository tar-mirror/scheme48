; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Mike Sperber

; Returns the a list of compiled rules and a list of the names that are
; free in the templates.  'ellipsis?' is a predicate that matches ellipses.
; Both values are #F if an error is found.

(define (compile-rules form ellipsis?)
  (let ((subkeywords (cadr form)))
    (let loop ((rules (cddr form)) (compiled '()) (inserted '()))
      (if (null? rules)
          (values (reverse compiled) inserted)
          (receive (c inserted)
              (compile-rule (car rules) subkeywords ellipsis? inserted)
            (if c
                (loop (cdr rules) (cons c compiled) inserted)
                (values #f #f)))))))

(define (compile-rule rule subkeywords ellipsis? inserted)
  (let ((pattern (cdar rule))
        (template (cadr rule)))
    (receive (pattern vars)
        (compile-pattern pattern subkeywords ellipsis?)
      (if pattern
          (receive (template inserted referenced)
              (compile-template template vars ellipsis? inserted)
            (if inserted ; template may legitimately be #f
                (values (cons pattern template) inserted)
                (values #f #f)))
          (values #f #f)))))
                        
(define (compile-pattern pattern subkeywords ellipsis?)
  (let label ((pattern pattern) (vars '()) (rank 0))
    (cond ((name? pattern)
           (if (memq pattern subkeywords)
               (values pattern vars)
               (let ((var (make-pattern-variable pattern rank)))
                 (values var (cons var vars)))))
          ((vector? pattern)
           (receive (patterns vars)
               (label (vector->list pattern) vars rank)
             (values (make-vector-marker patterns)
                     vars)))
          ((not (pair? pattern))
           (values pattern vars))
          ((not (and (pair? (cdr pattern))
                     (ellipsis? (cadr pattern))))
           (receive (head vars)
               (label (car pattern) vars rank)
             (receive (tail vars)
                 (label (cdr pattern) vars rank)
               (values (cons head tail) vars))))
          ((null? (cddr pattern))
           (receive (compiled ellipsis-vars)
               (label (car pattern)
                      '()
                      (+ rank 1))
             (values (make-ellipsis-form compiled ellipsis-vars)
                     (union ellipsis-vars vars))))
          (else
           (values #f '())))))

(define (compile-template template vars ellipsis? inserted)
  (call-with-current-continuation
    (lambda (quit)
      (let label ((template template)
                  (rank 0)
                  (inserted inserted)       ; free identifiers
                  (referenced '()))         ; pattern variables referenced
        (cond ((name? template)
               (let ((x (lookup-pattern-variable template vars)))
                 (cond ((not x)
                        (values template
                                (if (memq x inserted)
                                    inserted
                                    (cons template inserted))
                                referenced))
                       ((<= (pattern-variable-rank x)
                            rank)
                        (values x inserted (cons x referenced)))
                       (else
                        (quit #f #f #f)))))
              ((vector? template)
               (receive (templates inserted referenced)
                   (label (vector->list template) rank inserted referenced)
                 (values (make-vector-marker templates)
                         inserted
                         referenced)))
              ((not (pair? template))
               (values template inserted referenced))
              (else
               (let ((count (count-ellipsis (cdr template) ellipsis?)))
                 (receive (head inserted head-referenced)
                     (label (car template)
                            (+ rank count)
                            inserted
                            '())
                   (receive (tail inserted referenced)
                       (label (list-tail (cdr template) count)
                              rank
                              inserted
                              (union head-referenced referenced))
                     (values (cons (make-ellipsis-template head
                                                           count
                                                           head-referenced
                                                           rank
                                                           quit)
                                   tail)
                             inserted
                             referenced))))))))))

; Utilities

(define (union x y)
  (cond ((null? x)
         y)
        ((member (car x) y)
         (union (cdr x) y))
        (else
         (union (cdr x) (cons (car x) y)))))

(define (filter p l)
  (let label ((l l))
    (cond ((null? l)
           '())
          ((p (car l))
           (cons (car l) (label (cdr l))))
          (else
           (label (cdr l))))))

(define (lookup-pattern-variable v vars)
  (cond ((null? vars)
         #f)
        ((eq? v (pattern-variable-name (car vars)))
         (car vars))
        (else
         (lookup-pattern-variable v (cdr vars)))))

(define (count-ellipsis template ellipsis?)
  (let loop ((template template) (count 0))
    (if (and (pair? template)
             (ellipsis? (car template)))
        (loop (cdr template) (+ count 1))
        count)))

(define (make-ellipsis-template template count referenced rank quit)
  (if (= count 0)
      template
      (let ((ellipsis-vars (filter (lambda (var)
                                     (< rank
                                        (pattern-variable-rank var)))
                                   referenced)))
        (if (null? ellipsis-vars)
            (quit #f #f #f)
            (do ((template template (make-ellipsis-form template ellipsis-vars))
                 (count count (- count 1)))
                ((= count 0)
                 template))))))

