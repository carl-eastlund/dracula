#lang racket

(require "defun.rkt")

(provide deflist)

(define-for-syntax (identifier=? a b)
  (eq? (syntax-e a) (syntax-e b)))

(define-syntax (deflist stx)
  (syntax-case stx (l)
    [(form name (l) fn)
     (identifier? (syntax name))
     (quasisyntax/loc stx
       (defun name (l)
         (cond
           [(null? l) 't]
           [(pair? l)
            (if (not
                  (member
                    (unsyntax
                      (syntax-case* (syntax fn) (lambda) identifier=?
                        [(lambda (formal) body)
                         (syntax/loc (syntax fn)
                           (let ((formal (car l)))
                             body))]
                        [_
                         (identifier? (syntax fn))
                         (syntax/loc (syntax fn)
                           (fn (car l)))]
                        [_
                         (raise-syntax-error
                           #f "invalid predicate" stx (syntax fn))]))
                    '(nil ())))
              (name (cdr l))
              '())]
           [else '()])))]))


