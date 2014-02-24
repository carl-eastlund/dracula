#lang racket

(module reader syntax/module-reader
  #:language (make-dracula-require-sexp 'lang/dracula)
  #:read read
  #:read-syntax read-syntax
  (require "private/require.rkt")
  (require (dracula-in lang/reader)))

(require "private/require.rkt")
(require (dracula-in private/scheme/require-provide))
(require/provide (dracula-in lang/dracula))
