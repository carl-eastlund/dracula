#lang racket

(require (prefix-in acl2- "conditionals.rkt")
         "../teachpacks/testing.rkt")

(define-syntax (assert-event stx)
  (syntax-case stx ()
    [(_ e)
     (syntax/loc stx (check-expect (acl2-if e 't '()) 't))]))

(define-syntax (thm stx)
  (syntax-case stx ()
    [(_ e) #'(begin)]))

(define-syntax (progn stx)
  (syntax-case stx ()
    [(_ . ds) #'(begin . ds)]))

(define-syntax (disable stx)
  (syntax-case stx ()
    [(_ . stuff) #'(begin)]))
(define-syntax (enable stx)
  (syntax-case stx ()
    [(_ . stuff) #'(begin)]))
(define-syntax (in-theory stx)
  (syntax-case stx ()
    [(_ spec)
     #'(begin)]))

(define-syntax-rule (set-ruler-extenders e)
  (define-values [] (begin e (values))))

(define-syntax (theory-invariant stx)
  #'(begin))

(define-syntax (defpkg stx) #'(begin))
(define-syntax (deflabel stx) #'(begin))

(define-syntax (assert$ stx)
  (syntax-case stx ()
    [(_ test form)
     #'(acl2-if test
                form
                (error 'assert$ "Assertion failed!~n~a" 'test))]))

(define-syntax (mv stx)
  (syntax-case stx ()
    [(_ expr1 expr2 . exprs)
     #'(list expr1 expr2 . exprs)]))

(define-syntax (mv-let stx)
  (syntax-case stx ()
    [(_ ids expr body)
     #'(match-let ([(list . ids) expr]) body)]))


(define-syntax (deftheory stx)
  (syntax-case stx (:doc)
    [(_ name expr :doc doc-string) #'(define name '(theory: expr))]
    [(_ name expr) #'(deftheory name expr :doc "")]))

(define-syntax (defequiv stx)
  (syntax-case stx ()
    [(_ name)
     (identifier? #'name)
     (syntax/loc stx (begin))]))

(provide (all-defined-out)
         (rename-out [time time$]))
