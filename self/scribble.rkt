#lang racket/base

(require
  (for-syntax
    racket/base
    syntax/parse
    (path-up "self/module-path.rkt")))

(begin-for-syntax

  (define-syntax-class dracula-mod
    #:attributes {module-path}
    (pattern mod:id
      #:attr module-path
      (dracula-module-path #:stx (attribute mod)
        (syntax-e (attribute mod)))))

  (define-splicing-syntax-class use-dracula-sources
    #:attributes {[splice 1]}
    (pattern (~seq #:use-sources ~! [source:dracula-mod ...])
      #:attr [splice 1]
      (list #'#:use-sources
        (datum->syntax #false
          (attribute source.module-path))))
    (pattern (~seq)
      #:attr [splice 1] '[])))

(define-syntax (defmodule/dracula stx)
  (syntax-parse stx
    [(_ mod:dracula-mod use-sources:use-dracula-sources)
     #'(defmodule mod.module-path use-sources.splice ...)]))
