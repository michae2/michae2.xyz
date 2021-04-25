#!r6rs

(library (expand)
  (export expand-rule
          expand)
  (import (rnrs base)
          (rnrs lists))

;  (define-syntax and-let*
;    (syntax-rules )
;      [(_ ([var expr] ...) body)

  (define (pattern-matcher vars pattern form)
    (cond
      [(pair? pattern)
       (and (pair? form)
            ; TODO: make this short-circuit
            (let ([mcar (pattern-matcher vars (car pattern) (car form))]
                  [mcdr (pattern-matcher vars (cdr pattern) (cdr form))])
              (and mcar mcdr (append mcar mcdr))))]
      [(vector? pattern)
       (and (vector? pattern)
            (pattern-matcher vars (vector->list pattern) (vector->list form)))]
      [(and (symbol? pattern) (memq pattern vars))
       (list (cons pattern form))]
      [else
       (and (equal? pattern form) '())]))

  (define-syntax expand-rule
    (syntax-rules ()
      [(_ (var ...) pattern template)
       (cons (lambda (form)
               (pattern-matcher (quote (var ...)) pattern form))
             (lambda (bindings)
               (let ([var (cdr (assq (quote var) bindings))] ...)
                 template)))]))

  ;(define-syntax define-expand-rule
  ;  (syntax-rules ()
  ;    [(_

  (define (matches form rule)
    (let* ([matcher (car rule)]
           [transformer (cdr rule)]
           [bindings (matcher form)])
      (and bindings (lambda () (transformer bindings)))))

  (define (expand-once form rules)
    (let ([match (exists (lambda (rule) (matches form rule)) rules)])
      (if match (match) form)))

  (define (expand form rules)
    (let ([expansion (expand-once form rules)])
      (cond
        [(not (equal? expansion form))
         (expand expansion rules)]
        [(list? expansion)
         (map (lambda (subform) (expand subform rules)) expansion)]
        [(vector? expansion)
         (vector-map (lambda (subform) (expand subform rules)) expansion)]
        [else
         expansion]))))
