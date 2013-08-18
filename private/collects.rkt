#lang racket/base

(require
  (for-syntax
    racket/base
    syntax/parse
    racket/syntax)
  racket/require-syntax)

(define-require-syntax (cce stx)
  (syntax-parse stx
    [(_ suffix:id)
     (format-id (attribute suffix) #:source (attribute suffix)
       "dracula/private/scheme/~a" (attribute suffix))]))

(define-require-syntax (fasttest stx)
  (syntax-parse stx
    [(_ suffix:id)
     (format-id (attribute suffix) #:source (attribute suffix)
       "dracula/private/fasttest/~a" (attribute suffix))]))

(provide cce fasttest)
