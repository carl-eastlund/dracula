#lang racket

(provide (rename-out [acl2-app #%app]))

(require (for-syntax "syntax-checks.rkt")
         (for-template racket/base))

(define-syntax (acl2-app stx)
  (syntax-case stx (unbox)
    [(_) (syntax/loc stx '())]
    [(_ (unbox id) . args)
     (syntax/loc stx (#%app (unbox id) . args))]
    [(_ id . args)
     (and (identifier? #'id) (not (legal-constant-name? #'id)))
     (syntax/loc stx (#%app id . args))]
    [(_ id . args)
     (legal-constant-name? #'id)
     (raise-syntax-error #f
       "This is a constant name, but we expected a function name." stx #'id)]
    [(_ something . args)
     (raise-syntax-error #f
       "Expected the name of a function here" stx #'something)]))

