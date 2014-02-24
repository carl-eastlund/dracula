#lang racket/base

(module spec racket/base

  (provide
    make-dracula-require-syntax
    make-dracula-require-sexp)

  (require
    racket/match
    racket/string
    racket/syntax
    planet/version)

  (define planet-version (this-package-version))

  (define (make-symbol-suffix file dirs)
    (string-join (append dirs (list (format "~a" file))) "/"))

  (define (make-dracula-require-id stx version? file dirs)
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

  (define (make-dracula-require-syntax/long stx version? file dirs)
    (match planet-version
      [(list owner package major minor)
       `(,#'planet ,file
          ,(if version?
             (list owner package major minor)
             (list owner package))
          ,@dirs)]
      [#false
       `(,#'lib ,file "dracula" dirs)]))

  (define (make-dracula-require-syntax
            #:stx [stx #false]
            #:version? [version? #true]
            file . dirs)
    (match file
      [(? symbol?) (make-dracula-require-id stx version? file dirs)]
      [(? string? (regexp #px"^(.*)[.]rkt$" (list _ prefix)))
       (make-dracula-require-id stx version? (string->symbol prefix) dirs)]
      [(? string?) (make-dracula-require-syntax/long stx version? file dirs)]))

  (define (make-dracula-require-sexp #:version? [version? #true] file . dirs)
    (syntax->datum
      (apply make-dracula-require-syntax #:version? version? file dirs))))

(module macros racket/base

  (provide
    dracula-in)

  (require
    (for-syntax
      racket/base
      syntax/parse
      (submod ".." spec))
    racket/require-syntax)

  (define-require-syntax (dracula-in stx)
    (syntax-parse stx
      [(_ mod:id)
       (make-dracula-require-syntax #:stx stx
         (syntax-e (attribute mod)))])))

(module scribble racket/base)
