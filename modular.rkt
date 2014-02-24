#lang racket

(module reader syntax/module-reader
  #:language module-path:modular-acl2
  #:read read
  #:read-syntax read-syntax
  (require
    racket/require
    (path-up "self/require.rkt")
    (path-up "self/module-path.rkt")
    (dracula-in lang/acl2-reader)))

(require
  racket/require
  (path-up "self/require.rkt")
  (cce-in require-provide))

(require/provide (dracula-in modular/main))
