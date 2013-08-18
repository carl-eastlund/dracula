#lang racket

(require syntax/moddep
         racket/runtime-path
         (for-template racket/base))

(provide acl2-module-v
         modular-acl2-module-v
         teachpack-path
         make-teachpack-require-syntax
         make-dracula-spec)

(define (make-dracula-spec #:version? [version? #t] file . dirs)
  `(lib "dracula" ,@dirs ,file))

(define (make-teachpack-require-syntax file)
  (list #'lib "dracula" "teachpacks" file))

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
