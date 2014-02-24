#|
Gathers all the modules together and provides the core forms (more or less).
A few more forms are defined at provided toward the end.
|#
#lang racket/base

(require
  racket/require
  (path-up "self/require.rkt")
  (cce-in require-provide))

(require/provide
  "constants.rkt"
  "conditionals.rkt"
  "let.rkt"
  "quote.rkt"
  "acl2-top.rkt"
  "acl2-app.rkt"
  "declare.rkt"
  "defun.rkt"
  "defconst.rkt"
  "include-book.rkt"
  "in-package.rkt"
  "parameters.rkt"
  "with-prover-time-limit.rkt"
  "case-match.rkt"
  "defthm.rkt"
  "acl2-io.rkt"
  "prover.rkt")
