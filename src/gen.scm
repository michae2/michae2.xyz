#!r6rs

;;; Create random object generators defined as context-free grammars.

(library (gen)
  (export gen-chooser-random
          gen-grammar)
  (import (rnrs base)
          (rnrs control)
          (rnrs syntax-case)
          (only (chezscheme) random random-seed))

  (define (gen-chooser-random)
    (case-lambda
      [() #f]
      [(seed) (random-seed seed) #f]
      [(sum weights) (random sum)]))

  (define-syntax gen-grammar
    (lambda (g)
      (syntax-case g ()
        [(_ name (rule ...) body ...)
         #`(define (name chooser)
             ; Create a procedure for each production rule.
             #,@(map (lambda (rule)
                       (syntax-case rule ()
                         [(nt [w e] ...)
                          (do ([ws #'(w ...) (cdr ws)]
                               [es #'(e ...) (cdr es)]
                               [sum 0.0 (+ sum (syntax->datum (car ws)))]
                               [sums '() (cons sum sums)]
                               [choices #'([else #f])
                                        (cons #`[(>= choice #,sum) #,(car es)]
                                              choices)])
                              ((null? ws)
                               (let ([sums (reverse sums)])
                                 #`(define (nt)
                                     (let ([choice (chooser #,sum '#(#,@sums))])
                                       (cond
                                         #,@choices))))))]))
                     #'(rule ...))
             (case-lambda
               [() (chooser) body ...]
               [(seed) (chooser seed) body ...]))]))))
