#lang racket
(require "fresh.rkt")
(define-syntax (macro stx)
  (with-syntax {[name (fresh)]}
    #'(begin-for-syntax
        (define original (quote-syntax name))
        (provide original))))
(macro)
