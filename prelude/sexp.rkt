#lang dracula/base

(provide (all-defined-out))

(define empty '())

(define (empty? x)
  (equal? x '()))

(define (list? x)
  (cond
    [(cons? x) (list? (rest x))]
    [#:else (empty? x)]))

(define (binary-append xs ys)
  (cond
    [(cons? xs)
     (cons (first xs)
       (binary-append (rest xs) ys))]
    [#:else ys]))

(fold-shorthand (append e:expr ... e0:expr) (binary-append e e0))

(define (fix-natural x)
  (if (natural? x) x 0))

(define (natural-positive? x)
  (and (natural? x) (not (zero? x))))

(define (natural-zero? x)
  (not (natural-positive? x)))
