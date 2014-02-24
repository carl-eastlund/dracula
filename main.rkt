#lang racket

(module reader syntax/module-reader
  #:language module-path:acl2
  #:read read
  #:read-syntax read-syntax
  (require
    racket/require
    (path-up "self/module-path.rkt")
    (path-up "self/require.rkt")
    (dracula-in lang/reader)))

(require
  racket/require
  (path-up "self/require.rkt")
  (cce-in require-provide))

(require/provide (dracula-in lang/dracula))
