#lang racket/base

(require
  racket/contract
  racket/require
  (path-up "self/require.rkt")
  (cce-in syntax))

(define ((expand-keyword message) stx)
  (syntax-error stx message))

(define refresh-identifier
  (compose syntax-local-introduce syntax-local-get-shadower))

(define-struct syntax-meta (value transformer)
  #:property prop:procedure (struct-field-index transformer))

(define (syntax->meta id #:message [msg "not a syntactic meta-value"])

  (define (err)
    (syntax-error id (format "~a: ~s" msg (syntax->datum id))))

  (let* ([meta (syntax-local-value id err)])
    (if (syntax-meta? meta)
        (syntax-meta-value meta)
        (err))))

(define (reloc stx #:src src)
  (datum->syntax stx (syntax-e stx) src stx))

(define (reloc* stxs #:src src)
  (for/list {[stx (in-list stxs)]}
    (reloc stx #:src src)))

(provide/contract
 [reloc (-> syntax? #:src syntax? syntax?)]
 [reloc* (-> (listof syntax?) #:src syntax? (listof syntax?))]
 [expand-keyword (-> string? (-> syntax? syntax?))]
 [refresh-identifier (-> identifier? identifier?)]
 [syntax-meta? (-> any/c boolean?)]
 [make-syntax-meta (-> any/c (-> syntax? syntax?) syntax-meta?)]
 [syntax-meta-transformer (-> syntax-meta? (-> syntax? syntax?))]
 [syntax-meta-value (-> syntax-meta? any/c)]
 [syntax->meta
  (->* [identifier?]
       [#:message string?]
       any/c)])
