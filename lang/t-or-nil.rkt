#lang racket

(provide/contract [t-or-nil? [identifier? . -> . boolean?]])

(define symbol-downcase
  (compose string->symbol string-downcase symbol->string))

;; Determine if id is t or nil, case insensitive, symbolic-identifier=?
(define (t-or-nil? id)
  (let ([id (symbol-downcase (syntax-e id))])
    (or (eq? id 't) (eq? id 'nil))))
