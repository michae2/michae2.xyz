#!r6rs

;;; The html5 data structure is a nested list of nodes, where each node is
;;; either an element:
;;;     (name ((attr1 . val) (attr2 . val)) . children)
;;; or a bit of text:
;;;     "Four score and seven years ago..."
;;;     #\x1f604
;;;     'monkey

(library (html5)
  (export html5->ilist)
  (import (rnrs base)
          (rnrs lists))

  (define (html5-text->ilist text)
    ; TODO: inject WRAPs, replace reserved characters, etc
    text)

  (define (html5-comment->ilist children)
    (vector "<!-- " (map html5-text->ilist children) " -->"))

  (define (html5-attr->ilist attr)
    (cond
      [(null? attr)
       (vector)]
      [(null? (cdr attr))
       (vector #\space (car attr))]
      [else
       (vector #\space (car attr) #\= #\" (cdr attr) #\")]))

  (define (html5-start-tag->ilist name attrs)
    (vector #\< name (map html5-attr->ilist attrs) #\>))

  (define (html5-end-tag->ilist name)
    (vector #\< #\/ name #\>))

  (define (html5-void-element->ilist name attrs)
    (html5-start-tag->ilist name attrs))

  (define (html5-element->ilist name attrs . children)
    (case name
      [(!)
       (html5-phrasing->ilist children)]
      [(!--)
       (html5-comment->ilist children)]
      [(!DOCTYPE area base br col embed hr img input link meta param source
        track wbr)
       (html5-void-element->ilist name attrs)]
      [else
       (let ([start-tag (html5-start-tag->ilist name attrs)]
             [contents (html5->ilist children)]
             [end-tag (html5-end-tag->ilist name)])
         ; Condense list-like trees into a single line.
         (if (or (memp html5-node-wraps? children)
                 (memp list? contents)
                 (> (length contents) 1))
             (list start-tag contents end-tag)
             (vector start-tag contents end-tag)))]))

  (define (html5-node-wraps? node)
    (cond
      [(list? node)
       (memp html5-node-wraps? (cddr node))]
      [(string? node)
       (memv #\newline (string->list node))]
      [else
       (eqv? #\newline node)]))

  (define (html5-node->ilist node)
    (cond
      [(list? node)
       (apply html5-element->ilist node)]
      [(string? node)
       (html5-text->ilist node)]
      [else
       node]))

  (define (html5->ilist nodes)
    (if (null? nodes)
        '()
        (let ([node (html5-node->ilist (car nodes))]
              [rest (html5->ilist (cdr nodes))])
          (if (list? node)
              (append node rest)
              (cons node rest)))))

  ;; Phrasing elements group child nodes that should all appear on one line.
  (define (html5-phrasing->ilist children)
    (vector (html5->ilist children))))
