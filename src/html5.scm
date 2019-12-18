#!r6rs

;;; The html5 data structure is a nested list of nodes, where each node is
;;; either an element:
;;;     (name ((attr . val) ...) . children)
;;; or a bit of text:
;;;     "Four score and seven years ago"
;;;     #\x1f604
;;;     'xyz

(library (html5)
  (export html5-invalid-chars?
          html5-invalid-chars-value
          html5-invalid-text?
          html5-invalid-text-value
          html5-invalid-node?
          html5-invalid-node-value
          html5->ilist)
  (import (rnrs base)
          (rnrs conditions)
          (rnrs control)
          (rnrs exceptions)
          (rnrs lists))

  (define (html5-text-wrap wrap chars)
    ; This currently looks for newlines, but maybe it should also count columns.
    (let scan-line ([seen '()] [rest chars])
      (cond
        [(null? rest)
         (list (list->string (reverse seen)))]
        [(char=? (car rest) #\newline)
         (cons (list->string (reverse seen))
               (append wrap (scan-line '() (cdr rest))))]
        [else
         (scan-line (cons (car rest) seen) (cdr rest))])))

  (define (html5-text-escape chars)
    (if (null? chars)
        '()
        (let ([char (car chars)]
              [rest (html5-text-escape (cdr chars))])
          (case char
            [(#\<) (cons* #\& #\l #\t #\; rest)]
            [(#\>) (cons* #\& #\g #\t #\; rest)]
            [(#\&) (cons* #\& #\a #\m #\p #\; rest)]
            [(#\") (cons* #\& #\q #\u #\o #\t #\; rest)]
            [(#\') (cons* #\& #\a #\p #\o #\s #\; rest)]
            [(#\=) (cons* #\& #\e #\q #\u #\a #\l #\s #\; rest)]
            [(#\x00A0) (cons* #\& #\n #\b #\s #\p #\; rest)]
            [else (cons char rest)]))))

  (define-condition-type &html5-invalid-chars &error
    make-html5-invalid-chars html5-invalid-chars?
    (value html5-invalid-chars-value))

  (define (html5-text-validate checks chars)
    (if (null? chars)
        '()
        (let each-check ([tocheck checks])
          (if (null? tocheck)
              (cons (car chars)
                    (html5-text-validate checks (cdr chars)))
              (let match ([pattern (car tocheck)]
                          [rest chars])
                (if (null? pattern)
                    ; If a check matches and we replace the offending characters
                    ; then we must also validate the replacement.
                    (let* ([invalid (car tocheck)]
                           [replace (raise-continuable
                                      (condition
                                        (make-html5-invalid-chars invalid)))])
                      (html5-text-validate checks (append replace rest)))
                    (if (or (null? rest)
                            (not (char=? (car pattern) (car rest))))
                        (each-check (cdr tocheck))
                        (match (cdr pattern) (cdr rest)))))))))

  (define-condition-type &html5-invalid-text &error
    make-html5-invalid-text html5-invalid-text?
    (value html5-invalid-text-value))

  (define (html5-text-common->ilist text checks esc? . wrap)
    (if (not (string? text))
        (let ([replace (raise-continuable
                         (condition (make-html5-invalid-text text)))])
          (apply html5-text-common->ilist replace checks esc? wrap))
        (let* ([chars (string->list text)]
               [chars (html5-text-validate checks chars)]
               [chars (if esc? (html5-text-escape chars) chars)])
          (html5-text-wrap wrap chars))))

  (define control-checks
    (do ([i 0 (+ i 1)]
         [cchars '() (cons (list (integer->char i)) cchars)])
        ((= i 32)
         ; Perhaps we should also allow some of the other whitespace characters.
         (remp (lambda (cc) (memv (car cc) '(#\tab #\newline))) cchars))))

  (define comment-checks
    (append (map string->list '("<!--" "--!>" "-->")) control-checks))

  (define raw-checks
    ; This is a bit over-restrictive, but easier than adding checks for all
    ; capitalizations of "</script>" and "</style>".
    (append (map string->list '("</S" "</s")) control-checks))

  (define (html5-text->ilist text-mode text)
    (case text-mode
      [(cmnt)
       (html5-text-common->ilist text comment-checks #f 'WRAP "     ")]
      [(attr)
       (html5-text-common->ilist text control-checks #t "&NewLine;")]
      [(raw)
       (html5-text-common->ilist text raw-checks #f 'WRAP)]
      [(pre)
       (html5-text-common->ilist text control-checks #t #\newline)]
      [(esc)
       (html5-text-common->ilist text control-checks #t 'WRAP)]
      [else
       (assertion-violation 'html5-text->ilist "unrecognized mode" text-mode)]))

  (define (html5-text-mode text-mode name)
    (case name
      [(script style) 'raw]
      [(pre) 'pre]
      [else text-mode]))

  (define (html5-attr->ilist attr)
    (cond
      [(null? attr)
       (vector)]
      [(null? (cdr attr))
       (vector #\space (car attr))]
      [else
       (vector #\space (car attr) #\=
               #\" (html5-text->ilist 'attr (cdr attr)) #\")]))

  (define (html5-end-tag->ilist name)
    (vector #\< #\/ name #\>))

  (define (html5-start-tag->ilist name attrs)
    (vector #\< name (map html5-attr->ilist attrs) #\>))

  (define (html5-void-element->ilist name attrs)
    (html5-start-tag->ilist name attrs))

  (define (html5-comment->ilist children)
    (vector "<!-- "
            (map (lambda (c) (html5-text->ilist 'cmnt c)) children)
            " -->"))

  ;; Phrasing elements group child nodes that should all appear on one line.
  (define (html5-phrasing->ilist text-mode children)
    (vector (html5-nodes->ilist text-mode children)))

  (define (html5-element->ilist text-mode name attrs . children)
    (case name
      [(!)
       (html5-phrasing->ilist text-mode children)]
      [(!--)
       (html5-comment->ilist children)]
      [(!DOCTYPE area base br col embed hr img input link meta param source
        track wbr)
       (html5-void-element->ilist name attrs)]
      [else
       (let* ([text-mode (html5-text-mode text-mode name)]
              [start-tag (html5-start-tag->ilist name attrs)]
              [contents (html5-nodes->ilist text-mode children)]
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
      [(char? node)
       (char=? #\newline node)]
      [(symbol? node)
       (memv #\newline (string->list (symbol->string node)))]
      [else
       #f]))

  (define-condition-type &html5-invalid-node &error
    make-html5-invalid-node html5-invalid-node?
    (value html5-invalid-node-value))

  (define (html5-node->ilist text-mode node)
    (cond
      [(list? node)
       (apply html5-element->ilist text-mode node)]
      [(string? node)
       (html5-text->ilist text-mode node)]
      [(char? node)
       (html5-text->ilist text-mode (string node))]
      [(symbol? node)
       (html5-text->ilist text-mode (symbol->string node))]
      [(number? node)
       (html5-text->ilist text-mode (number->string node))]
      [else
       (let ([replace (raise-continuable
                        (condition (make-html5-invalid-node node)))])
         (html5-nodes->ilist text-mode replace))]))

  (define (html5-nodes->ilist text-mode nodes)
    (if (null? nodes)
        '()
        (let ([node (html5-node->ilist text-mode (car nodes))]
              [rest (html5-nodes->ilist text-mode (cdr nodes))])
          (if (list? node)
              (append node rest)
              (cons node rest)))))

  (define (html5->ilist nodes)
    (html5-nodes->ilist 'esc nodes)))
