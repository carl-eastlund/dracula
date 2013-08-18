#lang racket

(define-syntax-rule (unchecked-arity e) e)

(define-for-syntax (check-arity-transformer arity f) f)

(define-syntax-rule (define-below-marker-for name form) (begin))

(define-syntax-rule (begin-below . body) (begin . body))

(define-for-syntax (check-below-transformer marker f) f)

(define-syntax-rule (rename-below [above below] ...)
  (define-syntaxes [ below ... ]
    (values (make-rename-transformer #'above) ...)))

(define-syntax-rule (define-values-below . body) (define-values . body))

(define-syntax-rule (define-below . body) (define . body))

(define-syntax-rule (require-below . body) (require . body))

(provide
 (for-syntax check-arity-transformer) unchecked-arity
 (for-syntax check-below-transformer) begin-below define-below-marker-for
 rename-below
 define-below
 define-values-below
 require-below
 )
