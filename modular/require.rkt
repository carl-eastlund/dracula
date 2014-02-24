#lang racket/base

(require
  racket/require
  (path-up "self/require.rkt")
  (for-syntax
    racket/base
    (cce-in syntax)))

(provide require-macro)

(define-for-syntax (expand-require stx)
  (parameterize ([current-syntax stx])
    (syntax-case stx ()
      [(_ . mods)
       (begin
         (for ([m (syntax->list #'mods)])
           (unless (string? (syntax-e m))
             (syntax-error m "expected a filename (string literal)")))
         (syntax/loc stx
           (begin
             (require . mods)
             (provide (all-from-out . mods)))))])))

(define-syntax require-macro expand-require)
