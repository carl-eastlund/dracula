#lang racket/base

(require racket/stxparam
         "keywords.rkt"
         "dynamic-rep.rkt"
         "../private/planet.rkt")

(require (for-syntax racket/base
                     (cce syntax)
                     "static-rep.rkt"
                     "syntax-meta.rkt"))

(provide interface-macro)

(define-for-syntax (spec->static stx)
  (syntax-case stx (fun sig con mv include)
    [(fun f xs decl ... body)
     (syntax/loc stx
       (list
         (make-ind/static #'f
                          (syntax->list #'xs)
                          (syntax->list #'(decl ...))
                          #'body)))]
    [(sig f xs ys)
     (syntax/loc stx
       (list (make-sig/static #'f (syntax->list #'xs) (syntax->list #'ys))))]
    [(sig f xs)
     (syntax/loc stx
       (list (make-sig/static #'f (syntax->list #'xs) #f)))]
    [(con f e . options)
     (syntax/loc stx
       (list (make-con/static #'f #'e #'options)))]
    [(include . is)
     (syntax/loc stx
       (for/list ([i (in-list (syntax->list #'is))])
         (make-include/static
          (syntax->meta #:message "not an interface" i))))]))

(define-for-syntax (expand-interface stx)
  (parameterize ([current-syntax stx])
    (syntax-case stx ()
      [(_ name . specs)
       (with-syntax ([specs/static
                      (map spec->static (syntax->list #'specs))])
         (syntax/loc stx
           (define-syntax name
             (make-syntax-meta
              (make-interface/static #'name (append . specs/static))
              (expand-keyword "cannot be used as an expression")))))])))

(define-syntax interface-macro expand-interface)
