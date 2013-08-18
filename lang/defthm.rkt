#lang racket

(require "theorems.rkt")

(provide defthm defthmd defaxiom)

(define-syntax (defthm stx)
  (syntax-case stx ()
    [(_ name body . hints)
     #'(define-theorems "theorem" name)]))

;; To a runtime system, defthm and defaxiom are the same no-op.
(define-syntax (defaxiom stx)
  (syntax-case stx ()
    [(_ name body . hints)
     #'(define-theorems "axiom" name)]))
(define-syntax (defthmd stx)
  (syntax-case stx ()
    [(_ name body . hints)
     #'(define-theorems "theorem" name)]))

