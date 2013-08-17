#lang dracula/kernel

(provide
  let
  let*)

(define-syntax (let stx)
  (syntax-parse stx
    [(_ {[lhs:id rhs:expr] ...} body:expr)
     #'(let-values {[(lhs) rhs] ...} body)]))

(define-syntax (let* stx)
  (syntax-parse stx
    [(_ {[lhs:id rhs:expr] ...} body:expr)
     (foldr
       (lambda (id expr stx)
         #`(let {[#,id #,expr]} #,stx))
       #'body
       (@ lhs)
       (@ rhs))]))
