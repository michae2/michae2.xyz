#!r6rs

;;; Object generators defined as weighted context-free grammars.

(library (gen)
  (export gen-chooser-random
          gen-chooser-trie
          gen-grammar)
  (import (rnrs base)
          (rnrs control)
          (rnrs syntax-case)
          (only (chezscheme) random random-seed))

  (define (gen-chooser-random)
    (case-lambda
      [() #f]
      [(seed) (random-seed seed) #f]
      [(sum partial-sums) (random sum)]))

  (define (gen-chooser-trie)
    (let ([key -1]
          [suffix -1])
    (case-lambda
      [() (set! key (+ key 1)) (set! suffix key) #f]
      [(seed) (set! key seed) (set! suffix key) #f]
      [(sum partial-sums)
       (let-values ([(d m) (div-and-mod suffix (vector-length partial-sums))])
         (set! suffix d)
         (vector-ref partial-sums m))])))

  (define-syntax gen-grammar
    (lambda (g)
      (syntax-case g ()
        [(_ name (nt ...) body ...)
         #`(define (name chooser)
             ; Create a procedure for each nonterminal symbol.
             #,@(map (lambda (nt)
                       (syntax-case nt (gen-rules)
                         [(gen-rules (name) [w e] ...)
                          (do ([ws #'(w ...) (cdr ws)]
                               [es #'(e ...) (cdr es)]
                               [sum 0.0 (+ sum (syntax->datum (car ws)))]
                               [sums '() (cons sum sums)]
                               [choices #'([else #f])
                                        (cons #`[(>= choice #,sum) #,(car es)]
                                              choices)])
                              ((null? ws)
                               (let ([sums (reverse sums)])
                                 #`(define (name)
                                     (let ([choice (chooser #,sum '#(#,@sums))])
                                       (cond
                                         #,@choices))))))]))
                     #'(nt ...))
             (case-lambda
               [() (chooser) body ...]
               [(seed) (chooser seed) body ...]))]))))
