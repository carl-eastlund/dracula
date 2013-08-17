#lang dracula/base

(provide
  (all-defined-out))

(require
  dracula/prelude/sexp
  dracula/prelude/type)

(define (set? xs)
  (cond
    [(empty? xs) #true]
    [(cons? xs)
     (let {[y (first xs)]
           [ys (rest xs)]}
       (and
         (not (member? y ys))
         (set? ys)))]
    [#:else #false]))

(define (set-add x xs)
  (cond
    [(member? x xs) xs]
    [#:else (cons x xs)]))

(define (member? x xs)
  (cond
    [(empty? xs) #false]
    [(cons? xs)
     (or (equal? x (first xs))
       (member? x (rest xs)))]))

(define-alias empty-set empty)
(define-alias set-empty? empty?)

(fold-shorthand (set* e:expr ... e0:expr) (set-add e e0))
(define-shorthand (set e:expr ...) (set* e ... '()))

(fold-shorthand (set-union e:expr ... e0:expr)
  (binary-set-union e e0)
  (set))

(define (binary-set-union xs ys)
  (match xs
    ['() ys]
    [(cons x xs)
     (set-add x
       (binary-set-union xs ys))]))

(fold-shorthand (set-intersect e:expr ... e0:expr)
  (binary-set-intersect e e0))

(define (binary-set-intersect xs ys)
  (match xs
    ['() '()]
    [(cons x xs)
     (let* {[zs (binary-set-intersect xs ys)]}
       (cond
         [(member? x ys) (cons x zs)]
         [#:else zs]))]))

(define (set-subtract xs ys)
  (match ys
    ['() xs]
    [(cons y ys)
     (set-remove y
       (set-subtract xs ys))]))

(define (set-remove x ys)
  (match ys
    ['() '()]
    [(cons y ys)
     (let* {[zs (set-remove x ys)]}
       (cond
         [(equal? x y) zs]
         [#:else (cons y zs)]))]))

(theorem (set-remove/set-union x ys zs)
  (implies (and (set? ys) (set? zs))
    (equal?
      (set-remove x
        (set-union ys zs))
      (set-union
        (set-remove x ys)
        (set-remove x zs)))))

(description SET-OF
  (~component Elem #:> TYPE)
  (~define (is? xs)
    (cond
      [(empty? xs) #true]
      [(cons? xs)
       (let {[y (first xs)]
             [ys (rest xs)]}
         (and
           ((dot Elem is?) y)
           (not (member? y ys))
           (is? (rest ys))))]))
  (~component Type #:> TYPE
    #:where {[is? #:= is?]}))

(generic (Set-of T #:> TYPE) #:> SET-OF #:where {[Elem #:= T]}
  (define (is? xs)
    (cond
      [(empty? xs) #true]
      [(cons? xs)
       (let {[y (first xs)]
             [ys (rest xs)]}
         (and
           ((dot Elem is?) y)
           (not (member? y ys))
           (is? (rest ys))))]))
  (component Type #:> TYPE
    #:where {[is? #:= is?]}))

(define-types Set-of
  [Boolean
   Number
   Rational
   Integer
   Natural
   Symbol
   Keyword
   String
   Char
   List])
