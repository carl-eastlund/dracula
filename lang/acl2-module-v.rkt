#lang racket

(require syntax/moddep
         racket/runtime-path
         (for-template racket/base))

(provide
  (contract-out
    [acl2-module-v module-path?]
    [modular-acl2-module-v module-path?]
    [teachpack-path path-string?]
    [make-teachpack-require-syntax
     (->
       syntax?
       (or/c path-string? (syntax/c path-string?))
       module-path-syntax?)]
    [make-dracula-spec
     (->*
         {string?}
         {#:version? boolean?}
       #:rest (listof string?)
       module-path?)]))

(define (module-path-syntax? x)
  (and (syntax? x)
    (module-path? (syntax->datum x))))

(define make-dracula-spec make-dracula-require-sexp)

(define (make-teachpack-require-syntax stx file)
  (make-dracula-require-syntax #:stx stx file "teachpacks"))

(define acl2-module-v (make-dracula-spec 'lang/dracula))
(define modular-acl2-module-v (make-dracula-spec 'modular/main))

(define-runtime-path teachpack-v "../teachpacks")
(define backslash-pattern #rx"\\\\")
(define (backslashes->forward-slashes str)
  (regexp-replace* backslash-pattern str "/"))
(define teachpack-path
  (string-append
    (backslashes->forward-slashes
      (path->string teachpack-v))
    "/"))
