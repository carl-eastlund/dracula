#lang racket/base

(provide (all-defined-out))

;; Constants must be surrounded with asterisks.
(define (legal-constant-name? x)
  (and (identifier? x)
       (regexp-match-exact?
        (regexp "[*].+[*]")
        (symbol->string (syntax-e x)))))

;; is stx an identifier whose name starts with a colon?
(define (keyword-syntax? stx)
  (and (identifier? stx)
       (let ([str (symbol->string (syntax-e stx))])
         (eq? (string-ref str 0) #\:))))

