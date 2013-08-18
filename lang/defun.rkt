#|
defun generates macros that check that the defined function is used only in 
operator position (or acl2-provide/contract).  Also does arity checking.
|#
#lang racket

(require "constants.rkt"
         "declare.rkt"
         "../private/collects.rkt"
         "check.rkt"
         racket/stxparam)

(require (for-syntax "t-or-nil.rkt"
                     racket/match
                     (cce function)
                     (cce syntax)))

(provide defun
         defund
         defstub
         mutual-recursion)

(define-for-syntax (enforce-valid-name! stx v)
  (unless (identifier? v)
    (raise-syntax-error #f "expected an identifier" stx v))
  (when (t-or-nil? v)
    (raise-syntax-error #f "cannot bind reserved name" stx v)))

(define-for-syntax (expand-funs orig-stx funs-stx)
  (match (syntax-local-context)
    ['expression
     (raise-syntax-error #f
       "may not be used as an expression" orig-stx)]
    [_ (void)])
  (syntax-case funs-stx ()
    [((function args body) ...)
     (andmap list? (syntax->datum #'(args ...)))
     (begin
       (for* ([ids (in-list (syntax->list #'((function ...) args ...)))]
              [id (in-list (syntax->list ids))])
         (enforce-valid-name! orig-stx id))
       (with-syntax ([(checked ...) (generate-temporaries #'(function ...))]
                     [(arity ...) (map length (syntax->datum #'(args ...)))])
         (quasisyntax/loc orig-stx
           (begin
             (define-below-marker-for HERE #,orig-stx)
             (define-syntaxes (function ...)
               (values (check-below-transformer #'HERE
                         (check-arity-transformer 'arity
                           (redirect-transformer #'checked)))
                       ...))
             (define-values (checked ...)
               (let ([function (lambda args body)] ...)
                 (values function ...)))))))]))

(define-syntax (defstub stx)
  (syntax-case stx ()
    [(ds function (arg ...) dummy)
     (expand-funs
      stx
      #'((function (arg ...)
                   (raise-user-error 'function "cannot execute a stub"))))]))

(define-syntax (defun stx)
  (syntax-case stx ()
    [(df function (arg ...) doc/declare ... body)
     (expand-funs stx #'((function (arg ...) body)))]))

(define-syntax (defund stx)
  (syntax-case stx ()
    [(df function (arg ...) doc/declare ... body)
     (expand-funs stx #'((function (arg ...) body)))]))

(define-syntax (mutual-recursion stx)
  (syntax-case stx ()
    [(mr (defun function (arg ...) doc/declare ... body) ...)
     (expand-funs stx #'((function (arg ...) body) ...))]))
