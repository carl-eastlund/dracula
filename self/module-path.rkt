#lang racket/base

(provide
  dracula-module-path
  dracula-module-syntax

  module-path:acl2
  module-path:modular-acl2

  dracula-cce-syntax
  dracula-random-syntax
  dracula-fasttest-syntax
  dracula-teachpack-syntax

  dracula-teachpack-dir)

(require
  racket/match
  racket/string
  racket/syntax
  racket/runtime-path
  planet/version)

(define (make-symbol-suffix file dirs)
  (string-join (append dirs (list (format "~a" file))) "/"))

(define (package-module-id planet package stx version? file dirs)
  (match planet
    [(list owner (regexp #px"^(.*)[.]plt$" (list _ pkg)) major minor)
     (format-id stx #:source stx "~a/~a~a/~a"
       owner
       pkg
       (if version? (format ":~a:~a" major minor) "")
       (make-symbol-suffix file dirs))]
    [#false
     (format-id stx #:source stx "~a"
       (make-symbol-suffix file (append package dirs)))]))

(define (package-compound-module planet package stx version? file dirs)
  (define datum
    (match planet
      [(list owner package major minor)
       `(,#'planet ,file
          ,(if version?
             (list owner package major minor)
             (list owner package))
          ,@dirs)]
      [#false
       `(,#'lib ,file ,@package ,@dirs)]))
  (datum->syntax stx datum stx))

(define (package-module-syntax
          #:planet planet
          #:package package
          #:stx [stx #false]
          #:version? [version? #true]
          file . dirs)
  (match file
    [(? symbol?)
     (package-module-id planet package stx version? file dirs)]
    [(? string? (regexp #px"^(.*)[.]rkt$" (list _ prefix)))
     (define file (string->symbol prefix))
     (package-module-id planet package stx version? file dirs)]
    [(? string?)
     (package-compound-module planet package stx version? file dirs)]))

(define dracula-planet-version (this-package-version))
(define dracula-package-dirs '["dracula"])

(define (dracula-module-syntax
          #:stx [stx #false]
          #:version? [version? #true]
          file . dirs)
  (apply package-module-syntax
    #:planet dracula-planet-version
    #:package dracula-package-dirs
    #:stx stx
    #:version? version?
    file dirs))

(define random-planet-version
  (and dracula-planet-version
    (list "schematics" "random.plt" 1 0)))

(define random-package-dirs
  '["schematics" "random1"])

(define (dracula-random-syntax
          #:stx [stx #false]
          #:version? [version? #true]
          file . dirs)
  (apply package-module-syntax
    #:planet random-planet-version
    #:package random-package-dirs
    #:stx stx
    #:version? version?
    file dirs))

(define (dracula-syntax-maker base)
  (lambda {#:stx [stx #false]
           #:version? [version? #true]
           file . dirs}
    (apply dracula-module-syntax
      #:stx stx
      #:version? version?
      file base dirs)))

(define dracula-teachpack-syntax (dracula-syntax-maker "teachpacks"))
(define dracula-fasttest-syntax (dracula-syntax-maker "private/fasttest"))
(define dracula-cce-syntax (dracula-syntax-maker "private/scheme"))

(define (dracula-module-path #:version? [version? #true] file . dirs)
  (syntax->datum
    (apply dracula-module-syntax #:version? version? file dirs)))

(define module-path:acl2 (dracula-module-path 'main))
(define module-path:modular-acl2 (dracula-module-path 'modular))

(define-runtime-path teachpack-path "../teachpacks")

(define dracula-teachpack-dir
  (string-append
    (regexp-replace*
      (regexp-quote "\\")
      (path->string teachpack-path)
      "/")
    "/"))
