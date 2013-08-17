#lang mischief

(provide
  module-path->proof-obligation
  write-certification-script
  write-program-obligations-to-file
  proof-dependencies)

(require
  (for-syntax
    mischief
    #;(prefix-in static:
      dracula/proof/static)))

(define (proof-dependencies . mod-paths)
  (for {[mod-path (in-list mod-paths)]}
    (dynamic-require mod-path (void)))
  (dynamic-require 'dracula/proof/static (void))
  (eval
    #`(block
        (define-syntax (the-macro stx)
          (define static:all-modules-with-proofs
            (dynamic-require 'dracula/proof/static
              'all-modules-with-proofs))
          (define static:proof-dependencies
            (dynamic-require 'dracula/proof/static
              'proof-dependencies))
          (define/syntax-parse deps
            (quote-transformer
              (static:all-modules-with-proofs)))
          (define/syntax-parse mod~>deps
            (quote-transformer
              (static:proof-dependencies)))
          #'(values deps mod~>deps))
        (the-macro))))

(define (module-path->proof-obligation mod-path)
  (dynamic-require mod-path (void))
  (eval
    #`(block
        (define-syntax (the-macro stx)
          (define static:module-path->proof-obligation
            (dynamic-require 'dracula/proof/static
              'module-path->proof-obligation))
          (define/syntax-parse pf
            (static:module-path->proof-obligation '#,mod-path))
          #'(quote pf))
        (the-macro))))

(define (write-program-obligations-to-file . mod-paths)
  (for {[mod-path (in-list mod-paths)]}
    (dynamic-require mod-path (void)))
  (dynamic-require 'dracula/proof/static (void))
  (define/with-syntax [mod ...] (map quote-transformer mod-paths))
  (eval
    #'(block
        (define-syntax (the-macro stx)

          ;; workaround for sequencing bug:
          (define static:module-path->proof-obligation
            (dynamic-require 'dracula/proof/static
              'module-path->proof-obligation))
          (define static:write-program-obligations-to-file
            (dynamic-require 'dracula/proof/static
              'write-program-obligations-to-file))

          (for {[path (in-list (list mod ...))]}
            (static:write-program-obligations-to-file path
              (static:module-path->proof-obligation path)))
          #'(void))
        (the-macro))))

(define (write-certification-script . mod-paths)
  (for {[mod-path (in-list mod-paths)]}
    (dynamic-require mod-path (void)))
  (define/syntax-parse [mod ...]
    (map quote-transformer mod-paths))
  (eval
    #'(block
        (define-syntax (the-macro stx)
          (define static:write-certification-script
            (dynamic-require 'dracula/proof/static
              'write-certification-script))
          (static:write-certification-script mod ...)
          #'(void))
        (the-macro))))
