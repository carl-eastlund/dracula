#lang racket

(provide acl2-equal?)

(define (acl2-equal? x y)
  (match* [x y]
    ([(cons xa xb) (cons ya yb)]
     (and (acl2-equal? xa ya)
          (acl2-equal? xb yb)))
    ([(or '() 'nil) (or '() 'nil)] #t)
    ([_ _] (equal? x y))))
