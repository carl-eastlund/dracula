#lang racket


(require (for-syntax "t-or-nil.rkt"))
(provide (rename-out [acl2-let let] [acl2-let* let*]))

(define-for-syntax (identifier!? stx i)
  (or (identifier? i)
    (raise-syntax-error #f "Left-hand side of each binding must be an identifier" stx i)))

(define-for-syntax (check-let let-stx params)
  (and (andmap (lambda (i) (identifier!? let-stx i)) params)
    (for-each (lambda (i) 
                (when (t-or-nil? i)
                  (raise-syntax-error #f "t and nil may not be rebound" let-stx i)))
      params)
    #t))

(define-syntax (acl2-let stx)
  (syntax-case stx ()
    [(_ ([var expr] ...) body)
     (check-let stx (syntax->list #'(var ...)))
     #'(let ([var expr] ...) body)]))

(define-syntax (acl2-let* stx)
  (syntax-case stx ()
    [(_ ([var expr] ...) body)
     (check-let stx (syntax->list #'(var ...)))
     #'(let* ([var expr] ...) body)]))
