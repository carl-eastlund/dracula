#lang racket

(require (for-syntax syntax/parse))

(provide define-theorems)

(define-syntax (define-theorems stx)
  (syntax-parse stx
    [(_ type:str . ps)
     (and (list? (syntax->list #'ps))
          (andmap identifier? (syntax->list #'ps)))
     #'(define-syntaxes ps
         (apply values
           (for/list ([name (in-list 'ps)])
             (lambda (stx*)
               (raise-syntax-error name
                 (format "cannot use ~a as an expression" 'type) stx*)))))]))
