#lang racket

(require
  (for-syntax
    racket/base
    syntax/parse
    racket/syntax)
  racket/require-syntax
  "../../collects.rkt")

(define-require-syntax (random stx)
  (syntax-parse stx
    [(_ suffix:id)
     (format-id (attribute suffix) #:source (attribute suffix)
       "schematics/random1/~a" (attribute suffix))]))

(provide cce random)
