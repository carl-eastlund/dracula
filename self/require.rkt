#lang racket/base

(provide
  cce-in
  fasttest-in
  dracula-in
  teachpack-in)

(require
  (for-syntax
    racket/base
    syntax/parse
    (path-up "self/module-path.rkt"))
  racket/require-syntax)

(begin-for-syntax

  (define (make-module-requirer module-transformer)
    (lambda (stx)
      (syntax-parse stx
        [(_ mod:id ...)
         (define/syntax-parse [spec ...]
           (for/list {[mod-id (in-list (attribute mod))]}
             (module-transformer #:stx mod-id
               (syntax-e mod-id))))
         #'(combine-in spec ...)]))))

(define-require-syntax cce-in
  (make-module-requirer dracula-cce-syntax))

(define-require-syntax fasttest-in
  (make-module-requirer dracula-fasttest-syntax))

(define-require-syntax dracula-in
  (make-module-requirer dracula-module-syntax))

(define-require-syntax teachpack-in
  (make-module-requirer dracula-teachpack-syntax))
