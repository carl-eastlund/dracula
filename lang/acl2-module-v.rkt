#lang racket

(require syntax/moddep
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

(define teachpack-v (list 'lib "dracula" "teachpacks"))

(define backslash-pattern #rx"\\\\")

(define (backslashes->forward-slashes str)
  (regexp-replace* backslash-pattern str "/"))

(define teachpack-path
  (string-append (backslashes->forward-slashes
                  (path->string 
                   (resolve-module-path teachpack-v #f)))
                 "/"))

(define acl2-module-v (make-dracula-spec "dracula.rkt" "lang"))

(define modular-acl2-module-v (make-dracula-spec "main.rkt" "modular"))
