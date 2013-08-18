#lang racket/base
(require (for-syntax racket/base)
         scribble/eval
         scribble/manual
         scribble/decode
         "../reference/evaluator.rkt")

(provide show eval show/eval)

(define-syntax (show stx)
  (syntax-case stx ()
    [(_ def ...)
     (syntax/loc stx
       (racketblock def ...))]))

(define-syntax (eval stx)
  (syntax-case stx ()
    [(_ (def ...) (int ...))
     (syntax/loc stx
       (interaction #:eval (modular-evaluator 'def ...) int ...))]))

(define-syntax (show/eval stx)
  (syntax-case stx ()
    [(_ (def ...) (int ...))
     (syntax/loc stx
       (make-splice
        (list
         (show def ...)
         (eval (def ...) (int ...)))))]))
