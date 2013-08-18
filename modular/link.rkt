#lang racket/base

(require
  "../private/planet.rkt"
  (for-syntax
    racket/base
    racket/block
    syntax/parse
    (cce syntax)
    "static-rep.rkt"
    "syntax-meta.rkt"
    "list-set.rkt")
  "dynamic-rep.rkt"
  "keywords.rkt")

(provide link-macro)
(provide restrict-macro)

(define-for-syntax (restrict-module/static name stx mod imp-ids exp-ids)
  (define imps
    (for/list ([imp-id (in-list imp-ids)])
      (make-import/static
        imp-id
        (syntax->meta #:message "not an interface" imp-id)
        null
        null)))
  (for {[real-imp (in-list (module/static-imports mod))]}
    (unless (for/or {[spec-imp (in-list imps)]}
              (port/static-external=? spec-imp real-imp))
      (syntax-error stx
        "module ~s does not import required interface ~s"
        (syntax-e name)
        (syntax-e
          (interface/static-name
            (port/static-interface real-imp))))))
  (define exps
    (for/list ([exp-id (in-list exp-ids)])
      (make-export/static
        exp-id
        (syntax->meta #:message "not an interface" exp-id)
        null
        null)))
  (for {[spec-exp (in-list exps)]}
    (unless (for/or {[real-exp (in-list (module/static-exports mod))]}
              (port/static-external=? real-exp spec-exp))
      (syntax-error stx
        "module ~s exports unimplemented interface ~s"
        (syntax-e name)
        (syntax-e
          (interface/static-name
            (port/static-interface spec-exp))))))
  (make-module/static
    name
    (module/static-dynamic mod)
    stx
    (append imps exps)))

(define-syntax (restrict/derived stx)
  (syntax-parse stx
    [(_ original name full (import i ...) (export e ...))
     #'(define-syntax name
         (make-syntax-meta
           (restrict-module/static
             #'name
             (quote-syntax original)
             (syntax->meta #:message "not a module" #'full)
             (list #'i ...)
             (list #'e ...))
           (expand-keyword "cannot be used as an expression")))]))

(define-syntax (restrict-macro stx)
  (syntax-parse stx
    [(_ name full (import i ...) (export e ...))
     #`(restrict/derived #,stx name full (import i ...) (export e ...))]))

(define-for-syntax (expand-link stx)
  (parameterize ([current-syntax stx])
    (syntax-parse stx
      #:literals (import export)
      [(form name (import i ...) (export e ...) (mod ...))
       (with-syntax ([original stx])
         #'(begin
             (form full (mod ...))
             (restrict/derived original name full
               (import i ...) (export e ...))))]
      [(_ name ())
       (syntax-error stx "must link at least one module")]
      [(_ name (mod))
       (syntax/loc stx
         (define-syntax name (syntax-local-value #'mod)))]
      [(form name (one two . mods))
       (with-syntax ([original stx])
         (syntax/loc stx
           (begin
             (define-syntaxes (one-two one/impl two/impl)
               (block
                 (define one/static
                   (syntax->meta #:message "not a module" #'one))
                 (define one/dynamic (module/static-dynamic one/static))
                 (define one/imports (module/static-imports one/static))
                 (define one/exports (module/static-exports one/static))
                 (define two/static
                   (syntax->meta #:message "not a module" #'two))
                 (define two/dynamic (module/static-dynamic two/static))
                 (define two/imports (module/static-imports two/static))
                 (define two/exports (module/static-exports two/static))
                 (for* {[one/import (in-list one/imports)]
                        [two/export (in-list two/exports)]
                        #:when (port/static-external=? one/import two/export)}
                   (syntax-error (quote-syntax original)
                     (string-append
                       "constituents of link must be reordered; "
                       "import of interface ~s comes before "
                       "export of interface ~s")
                     (syntax-e
                       (interface/static-name
                         (port/static-interface one/import)))
                     (syntax-e
                       (interface/static-name
                         (port/static-interface two/export)))))
                 (define one-two/exports
                   (list-union #:compare port/static-external=?
                     one/exports two/exports))
                 (define one-two/imports
                   (list-union #:compare port/static-external=?
                     one/imports
                     (list-minus #:compare port/static-external=?
                       two/imports one/exports)))
                 (values
                   (make-syntax-meta
                     (make-module/static
                       #'one-two
                       #'dynamic
                       #'original
                       (append one-two/imports one-two/exports))
                     (expand-keyword "cannot be used as an expression"))
                   (make-rename-transformer one/dynamic)
                   (make-rename-transformer two/dynamic))))
             (define dynamic
               (make-module/dynamic
                (lambda (imp/dynamic)
                  (let* ([one/func (module/dynamic-implementation one/impl)]
                         [exp-one/dynamic (one/func imp/dynamic)]
                         [imp-two/dynamic
                          (interface/dynamic-join imp/dynamic exp-one/dynamic)]
                         [two/func (module/dynamic-implementation two/impl)]
                         [exp-two/dynamic (two/func imp-two/dynamic)]
                         [exp/dynamic
                          (interface/dynamic-join imp-two/dynamic
                                                  exp-two/dynamic)])
                    exp/dynamic))))
             (form name (one-two . mods)))))]
      [_
       (syntax-error
        stx
        "expected a name followed by a parenthesized list of one or more module names")])))

(define-syntax link-macro expand-link)
