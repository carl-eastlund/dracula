#lang racket
(provide declare)

(define-syntax (declare stx)
  (raise-syntax-error #f
    "declare may not be used as an expression" stx))
