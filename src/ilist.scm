#!r6rs

;;; An indent-list (or "ilist") is a nested list of objects, representing a
;;; hierarchical document with indents corresponding to list depth.

(library (ilist)
  (export display-ilist
          write-ilist)
  (import (rnrs base)
          (rnrs control)
          (rnrs io simple))

  (define (display-ilist-indent level out)
    (let ([indent (make-string (* level 2) #\space)])
      (display indent out)))

  (define (display-ilist-single level obj out)
    (cond
      [(vector? obj)
       (vector-for-each (lambda (o) (display-ilist-single level o out)) obj)]
      [(list? obj)
       (for-each (lambda (o) (display-ilist-single level o out)) obj)]
      [(eq? obj 'WRAP)
       (newline out)
       (display-ilist-indent level out)]
      [else
       (display obj out)]))

  (define (display-ilist-multi level objs out)
    (for-each
      (lambda (obj)
        (if (list? obj)
            (display-ilist-multi (+ level 1) obj out)
            (begin
              (display-ilist-indent level out)
              (display-ilist-single level obj out)
              (newline out))))
      objs))

  (define (display-ilist objs out)
    (display-ilist-multi 0 (if (list? objs) objs (list objs)) out))

  (define (write-ilist-single level obj out)
    (cond
      [(vector? obj)
       (display "#" out)
       (write-ilist-single level (vector->list obj) out)]
      [(list? obj)
       (display "(" out)
       (for-each
         (lambda (o)
           (display " " out)
           (write-ilist-single (+ level 1) o out))
         obj)
       (display ")" out)]
      [else
       (write obj out)
       (when (eq? obj 'WRAP)
         (newline out)
         (display-ilist-indent level out))]))

  (define (write-ilist-multi level objs out)
    (for-each
      (lambda (obj)
        (display-ilist-indent level out)
        (if (list? obj)
            (begin
              (display "(" out)
              (newline out)
              (write-ilist-multi (+ level 1) obj out)
              (display-ilist-indent level out)
              (display ")" out))
            (write-ilist-single level obj out))
        (newline out))
      objs))

  (define (write-ilist objs out)
    (write-ilist-multi 0 (list objs) out)))
