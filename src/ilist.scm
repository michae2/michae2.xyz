#!r6rs

;;; An indent-list (or "ilist") is a nested list of objects, representing a
;;; hierarchical document with indents corresponding to list depth.

(library (ilist)
  (export display-ilist)
  (import (rnrs base)
          (rnrs io simple))

  (define (ilist-indent level)
    (make-string level #\tab))

  (define (display-ilist-single level obj out)
    (cond
      [(vector? obj)
       (vector-for-each (lambda (o) (display-ilist-single level o out)) obj)]
      [(list? obj)
       (for-each (lambda (o) (display-ilist-single level o out)) obj)]
      [(eq? obj 'WRAP)
       (newline out)
       (display (ilist-indent level) out)]
      [else
       (display obj out)]))

  (define (display-ilist-multi level objs out)
    (for-each
      (lambda (obj)
        (if (list? obj)
            (display-ilist-multi (+ level 1) obj out)
            (begin
              (display (ilist-indent level) out)
              (display-ilist-single level obj out)
              (newline out))))
      objs))

  (define (display-ilist objs out)
    (display-ilist-multi 0 (if (list? objs) objs (list objs)) out)))
