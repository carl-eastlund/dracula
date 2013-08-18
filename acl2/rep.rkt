#lang racket/base
(require racket/contract)

;; A Sexp is (Or Symbol String Char ExactNumber (cons Sexp Sexp))
(define sexp/c
  (flat-rec-contract sexp
    null? ;; Scheme/ACL2 mismatch
    symbol?
    string?
    char?
    (and/c number? exact?)
    (cons/c sexp sexp)))

(provide/contract
 [sexp/c flat-contract?])
