#lang racket/base

(require
  (for-syntax
    racket/base
    syntax/parse
    racket/syntax)
  racket/require-syntax
  planet/version)

(define-require-syntax (cce stx)
  (syntax-parse stx
    [(_ suffix:id)
     (define/syntax-parse path:id
       (format-id (attribute suffix) #:source (attribute suffix)
         "private/scheme/~a" (attribute suffix)))
     (datum->syntax
       stx
       (list #'this-package-in (attribute path))
       stx)]))

(define-require-syntax (fasttest stx)
  (syntax-parse stx
    [(_ suffix:id)
     (define/syntax-parse path:id
       (format-id (attribute suffix) #:source (attribute suffix)
         "private/fasttest/~a" (attribute suffix)))
     (datum->syntax
       stx
       (list #'this-package-in (attribute path))
       stx)]))

(provide cce fasttest)
