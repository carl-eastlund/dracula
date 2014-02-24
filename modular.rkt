#lang racket
(module reader syntax/module-reader
  #:language (make-dracula-require-sexp 'modular))
(require "private/scheme/require-provide.rkt")
(require/provide (dracula-in modular/main))
