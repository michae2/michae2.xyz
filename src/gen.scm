#!r6rs

;;; Macros to make random object generators defined as context-free grammars.

(library (gen)
  (export gen-rule
          gen-start)
  (import (rnrs base)
          (rnrs control)
          (rnrs syntax-case)
          (only (chezscheme) random random-seed))

  ;; Define a nonterminal symbol and a set of production rules to rewrite the
  ;; symbol.  Each rule starts with a positive real number to indicate the
  ;; likelihood of it being chosen.
  (define-syntax gen-rule
    (lambda (x)
      (syntax-case x ()
        [(_ nonterm [n0 expr0] [n1 expr1] ...)
         (let each ([sum 0.0]
                    [rules #'([n0 expr0] [n1 expr1] ...)]
                    [clauses '()])
           (if (null? rules)
               #`(define (nonterm)
                   (let ([r (random #,sum)])
                     (cond
                       #,@clauses)))
               (let ([rule (car rules)])
                 (each (+ sum (syntax->datum (car rule)))
                       (cdr rules)
                       (cons #`[(>= r #,sum) #,(cadr rule)] clauses)))))])))

  (define-syntax gen-start
    (syntax-rules ()
      [(_ name nonterm)
       (define name
         (case-lambda
           [() (nonterm)]
           [(seed) (random-seed seed) (nonterm)]))])))
