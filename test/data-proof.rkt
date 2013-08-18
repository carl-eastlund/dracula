#lang racket

(require "../proof/proof.rkt")

(provide (all-defined-out))

(define stx-1+2 (datum->syntax #f '(+ 1 2) (list "here" 1 0 1 7)))
(define stx-3*4 (datum->syntax #f '(* 3 4) (list "here" 1 0 9 7)))
(define stx-1234
  (datum->syntax #f (list stx-1+2 stx-3*4) (list "here" 1 0 1 15)))

(define stx-list-a-b (datum->syntax #f '(list 'a 'b) (list "here" 1 0 17 12)))
(define stx-quote-c-d (datum->syntax #f ''(c d) (list "here" 1 0 30 6)))
(define stx-abcd
  (datum->syntax #f (list stx-list-a-b stx-quote-c-d) (list "here" 1 0 17 19)))

(define loc-0:7 (syntax->loc stx-1+2))
(define loc-8:15 (syntax->loc stx-3*4))
(define loc-0:15 (syntax->loc stx-1234))
(define loc-16:28 (syntax->loc stx-list-a-b))
(define loc-29:35 (syntax->loc stx-quote-c-d))
(define loc-16:35 (syntax->loc stx-abcd))

(define term-1+2 (syntax->term stx-1+2))
(define term-3*4 (syntax->term stx-3*4))
(define term-list-a-b (syntax->term stx-list-a-b))
(define term-quote-c-d (syntax->term stx-quote-c-d))

(define part-abcd (syntax->part stx-abcd #:name 'Helper))
(define part-1234 (syntax->part stx-1234 #:name 'Main))
(define part-1234/renamed (syntax->part stx-1234 #:name 'Main-Renamed))

(define proof-main+helper (make-proof part-abcd part-1234))
(define proof-main+helper/reordered (make-proof part-1234 part-abcd))
(define proof-helper (make-proof part-abcd))
(define proof-main (make-proof part-1234))
(define proof-main/renamed (make-proof part-1234/renamed))