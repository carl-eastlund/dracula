#lang racket
(require "original.rkt")
(define-syntax (macro stx)
  (with-syntax {[orig original]}
    #'(begin-for-syntax
        (define identical (quote-syntax orig))
        (provide identical))))
(macro)
