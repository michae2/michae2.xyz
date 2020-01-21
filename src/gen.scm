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
  ;; symbol.  Each rule starts with a nonzero integer to indicate the likelihood
  ;; of it being chosen.
  (define-syntax gen-rule
    (lambda (x)
      (syntax-case x ()
        [(_ nonterm [n0 expr0] [n1 expr1] ...)
         (let each ([i 0]
                    [rules #'([n0 expr0] [n1 expr1] ...)]
                    [cases '()])
           (if (null? rules)
               #`(define (nonterm)
                   (case (random #,i)
                     #,@cases))
               (let ([rule (car rules)])
                 (do ([i i (+ i 1)]
                      [n (syntax->datum (car rule)) (- n 1)]
                      [cases cases (cons #`[(#,i) #,(cadr rule)] cases)])
                     ((zero? n) (each i (cdr rules) cases))))))])))

  (define-syntax gen-start
    (syntax-rules ()
      [(_ name nonterm)
       (define name
         (case-lambda
           [() (nonterm)]
           [(seed) (random-seed seed) (nonterm)]))])))
