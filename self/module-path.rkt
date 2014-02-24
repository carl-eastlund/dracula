#lang racket/base

(provide
  dracula-module-path
  dracula-module-syntax

  module-path:acl2
  module-path:modular-acl2

  dracula-cce-syntax
  dracula-fasttest-syntax
  dracula-teachpack-syntax
  dracula-teachpack-dir)

(require
  racket/match
  racket/string
  racket/syntax
  planet/version)

(define planet-version (this-package-version))

(define (make-symbol-suffix file dirs)
  (string-join (append dirs (list (format "~a" file))) "/"))

(define (dracula-module-id stx version? file dirs)
  (match planet-version
    [(list owner (regexp #px"^(.*)[.]plt$" (list _ pkg)) major minor)
     (format-id stx #:source stx "~a/~a~a/~a"
       owner
       pkg
       (if version? (format ":~a:~a" major minor) "")
       (make-symbol-suffix file dirs))]
    [#false
     (format-id stx #:source stx "dracula/~a"
       (make-symbol-suffix file dirs))]))

(define (dracula-compound-module stx version? file dirs)
  (define datum
    (match planet-version
      [(list owner package major minor)
       `(,#'planet ,file
          ,(if version?
             (list owner package major minor)
             (list owner package))
          ,@dirs)]
      [#false
       `(,#'lib ,file "dracula" dirs)]))
  (datum->syntax stx datum stx))

(define (dracula-module-syntax
          #:stx [stx #false]
          #:version? [version? #true]
          file . dirs)
  (match file
    [(? symbol?) (dracula-module-id stx version? file dirs)]
    [(? string? (regexp #px"^(.*)[.]rkt$" (list _ prefix)))
     (dracula-module-id stx version? (string->symbol prefix) dirs)]
    [(? string?) (dracula-compound-module stx version? file dirs)]))

(define (dracula-module-path #:version? [version? #true] file . dirs)
  (syntax->datum
    (apply dracula-module-syntax #:version? version? file dirs)))

(define module-path:acl2 (dracula-module-path 'main))
(define module-path:modular-acl2 (dracula-module-path 'modular))

(define (dracula-teachpack-syntax
          #:stx [stx #false]
          #:version? [version? #true]
          file . dirs)
  (apply dracula-module-syntax
    #:stx stx
    #:version? version?
    file "teachpacks" dirs))

(define-runtime-path teachpack-path "../teachpacks")

(define dracula-teachpack-dir
  (string-append
    (regexp-replace*
      (regexp-quote "\\")
      (path->string teachpack-path)
      "/")
    "/"))
