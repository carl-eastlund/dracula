#lang dracula/kernel

(provide
  declare-values
  declare-syntaxes
  declare-synonyms
  where)

(require
  dracula/prelude/core/imported)

(begin-for-syntax
  (define (declaration-transformer stx)
    (parameterize {[current-syntax-context stx]}
      (wrong-syntax stx
        "illegal use of form; must be at the top level of a description body")))
  (define (description-transformer stx)
    (parameterize {[current-syntax-context stx]}
      (wrong-syntax stx
        "illegal use of form; must be in a description specification"))))

(define-syntax declare-values declaration-transformer)
(define-syntax declare-syntaxes declaration-transformer)

(define-syntax where description-transformer)

(define-shorthand
  (declare-synonyms [stx-name:id val-name:id] ...)
  (begin))
