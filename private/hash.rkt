#lang racket

(define (hash-contains? table key)
  (let/ec return
    (hash-ref table key (lambda () (return #f)))
    #t))

(define (hash-ref/check table key)
  (hash-ref table key))

(define (hash-ref/identity table key)
  (hash-ref table key (lambda () key)))

(define (hash-ref/default table key default)
  (hash-ref table key (lambda () default)))

(define (hash-domain table)
  (for/list ([i (in-hash-keys table)]) i))

(define (hash-range table)
  (for/list ([i (in-hash-values table)]) i))

(provide/contract
 [hash-contains? (-> hash? any/c boolean?)]
 [hash-ref/identity (-> hash? any/c any/c)]
 [hash-ref/default (-> hash? any/c any/c any/c)]
 [hash-ref/check
  (->d ([table hash?] [key any/c]) ()
       #:pre-cond (hash-contains? table key)
       [_ any/c])]
 [hash-domain (-> hash? list?)]
 [hash-range (-> hash? list?)])
