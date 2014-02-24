#lang racket/base

(require
  (for-syntax
    racket/base
    syntax/parse
    (submod "require.rkt" syntax))
  racket/require-syntax)

(define-require-syntax (cce stx)
  (syntax-parse stx
    [(_ suffix:id)
     (make-dracula-require-syntax #'suffix "private" "scheme")]))

(define-require-syntax (fasttest stx)
  (syntax-parse stx
    [(_ suffix:id)
     (make-dracula-require-syntax #'suffix "private" "fasttest")]))

(provide cce fasttest)
