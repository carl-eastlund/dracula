#lang racket/base

(require
  racket/sandbox
  racket/require
  (path-up "self/require.rkt")
  (path-up "self/module-path.rkt")
  (cce-in sandbox))

(provide the-evaluator evaluator modular-evaluator)

(define (evaluator . definitions)
  (apply make-scribble-evaluator module-path:acl2 definitions))

(define (modular-evaluator . definitions)
  (apply make-scribble-evaluator module-path:modular-acl2 definitions))

(define the-evaluator (evaluator))
