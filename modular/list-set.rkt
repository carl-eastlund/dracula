#lang racket/base

(provide list-union
         list-union*
         list-inter
         list-inter*
         list-minus)

(define (list-minus #:compare [compare equal?] one two)
  (remove* two one compare))

(define (list-union #:compare [compare equal?] . sets)
  (list-union* #:compare compare sets))

(define (list-union* #:compare [compare equal?] sets)
  (if (null? sets)
      null
      (let* ([tail (list-union* #:compare compare (cdr sets))]
             [head (list-minus #:compare compare (car sets) tail)])
        (append tail head))))

(define (list-inter #:compare [compare equal?] set . sets)
  (list-inter* #:compare compare (cons set sets)))

(define (list-inter* #:compare [compare equal?] sets)
  (let* ([head (car sets)]
         [rest (cdr sets)])
    (if (null? rest)
        head
        (let* ([tail (list-inter* #:compare compare rest)]
               [over (list-minus #:compare compare tail head)]
               [gone (list-minus #:compare compare tail over)])
          gone))))
