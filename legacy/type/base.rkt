#lang mischief

(provide
  (struct-out type)
  ref?)

(require
  dracula/proof/term)

(struct type [ref] #:transparent)

(define (ref? x)
  (or
    (syntax? x)
    (false? x)))
