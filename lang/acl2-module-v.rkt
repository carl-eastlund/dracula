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
     (-> syntax? path-string? module-path-syntax?)]
    [make-dracula-spec
     (->*
         {string?}
         {#:version? boolean?}
       #:rest (listof string?)
       module-path?)]))

(define (module-path-syntax? x)
  (and (syntax? x)
    (module-path? (syntax->datum x))))

(define (make-dracula-spec #:version? [version? #t] file . dirs)
  `(lib ,file "dracula" ,@dirs))

(define (make-teachpack-require-syntax stx file)
  (datum->syntax stx (list #'lib file "dracula" "teachpacks") stx))

(define backslash-pattern #rx"\\\\")

(define (backslashes->forward-slashes str)
  (regexp-replace* backslash-pattern str "/"))

(define-runtime-path teachpack-v "../teachpacks")

(define teachpack-path
  (string-append (backslashes->forward-slashes
                  (path->string teachpack-v))
                 "/"))

(define acl2-module-v (make-dracula-spec "dracula.rkt" "lang"))

(define modular-acl2-module-v (make-dracula-spec "main.rkt" "modular"))
