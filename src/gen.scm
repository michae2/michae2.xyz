#!r6rs

(library (gen)
  (export gen-rule
          gen-start)
  (import (rnrs base)
          (rnrs control)
          (rnrs syntax-case)
          (only (chezscheme) random random-seed))

  (define-syntax gen-rule
    (lambda (x)
      (syntax-case x ()
        [(_ nonterm expr0 expr ...)
         (let each ([exprs #'(expr ...)]
                    [cases #'([(0) expr0])]
                    [i 1])
           (if (null? exprs)
               #`(define (nonterm)
                   (case (random #,i)
                     #,@cases))
               (each (cdr exprs)
                     (cons #`[(#,i) #,(car exprs)] cases)
                     (+ i 1))))])))

  (define-syntax gen-start
    (syntax-rules ()
      [(_ name nonterm)
       (define name
         (case-lambda
           [() (nonterm)]
           [(seed) (random-seed seed) (nonterm)]))])))
