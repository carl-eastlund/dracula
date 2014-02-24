#lang racket/base

(require
  racket/require
  (path-up "self/require.rkt")
  (dracula-in lang/do-check)
  "module.rkt"
  (for-syntax
    racket/base
    racket/list
    (cce-in syntax)
    "static-rep.rkt"
    "syntax-meta.rkt"
    "../proof/proof.rkt"
    "../proof/syntax.rkt"))

(provide top-interaction-macro module-begin-macro)

(define-for-syntax (expand-top-interaction stx)
  (parameterize ([current-syntax stx])
    (syntax-case stx ()
      [(_ . body) (syntax/loc stx (#%top-interaction . body))])))

(define-for-syntax (get-module-name stx)
  (syntax-case stx (module-macro)
    [(module-macro name . _) (identifier? #'name) #'name]
    [_ #f]))

(define-for-syntax (expand-module-begin stx)
  (parameterize ([current-syntax stx])
    (syntax-case stx ()
      [(_ . body)
       (with-syntax ([exports (datum->syntax stx `(,#'all-defined-out))]
                     [names (filter-map get-module-name (syntax->list #'body))])
         (syntax/loc stx
           (#%module-begin
            (provide exports)
            (begin-below . body)
            (define-values ()
              (annotate-modules names (values))))))])))

(define-for-syntax (body->syntax body)
  (cond
    [(import/static? body) (import/static-abstract body)]
    [(export/static? body) (export/static-concrete body)]
    [else body]))

(define-for-syntax (module/static->part id mod)
  (make-part
   (syntax-e id)
   (syntax->loc (module/static-source mod))
   (map syntax->term
     (map body->syntax
       (module/static-body mod)))))

(define-for-syntax (identifier->part id)
  (module/static->part id (syntax->meta #:message "not a module" id)))

(define-syntax (annotate-modules stx)
  (syntax-case stx ()
    [(_ names expr)
     (annotate-proof
      (apply make-proof (map identifier->part (syntax->list #'names)))
      #'expr)]))

(define-syntax top-interaction-macro expand-top-interaction)
(define-syntax module-begin-macro expand-module-begin)
