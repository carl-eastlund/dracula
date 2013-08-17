#lang mischief

(provide
  diff)

(require
  srfi/67)

(define (diff a b)
  (diff-at a b '() '()))

(define (diff-at a b path tail)
  (cond
    [(and (list? a) (list? b)) (list-diff-at a b path tail)]
    [(and (pair? a) (pair? b)) (pair-diff-at a b path tail)]
    [(and (vector? a) (vector? b)) (vector-diff-at a b path tail)]
    [(and (hash? a) (hash? b)) (hash-diff-at a b path tail)]
    [(and (box? a) (box? b)) (box-diff-at a b path tail)]
    [(and (prefab? a) (prefab? b)) (prefab-diff-at a b path tail)]
    [(equal? a b) tail]
    [else (cons (at path a b 'incomparable) tail)]))

(define (list-diff-at a b path tail)
  (define na (length a))
  (define nb (length b))
  (cond
    [(= na nb)
     (diff-at/ref a b path tail list-ref
       (for/list {[i (in-range (length a))]} i))]
    [else (cons (at path na nb 'length) tail)]))

(define (pair-diff-at a b path tail)
  (diff-at (car a) (car b) (extend path 'car)
    (diff-at (cdr a) (cdr b) (extend path 'cdr) tail)))

(define (vector-diff-at a b path tail)
  (cond
    [(= (vector-length a) (vector-length b))
     (diff-at/ref a b path tail vector-ref
       (for/list {[i (in-range (vector-length a))]} i))]
    [else (cons (at path a b 'vector-length) tail)]))

(define (hash-diff-at a b path tail)
  (cond
    [(not (hash-type=? a b)) (cons (at path a b 'hash-type) tail)]
    [(not (hash-compare=? a b)) (cons (at path a b 'hash-compare) tail)]
    [(not (hash-keys=? a b)) (cons (at path a b 'hash-keys) tail)]
    [else
     (diff-at/ref a b path tail hash-ref
       (sort (hash-keys a) (<? default-compare)))]))

(define (box-diff-at a b path tail)
  (cond
    [(box-type=? a b)
     (diff-at (unbox a) (unbox b) (extend path 'unbox) tail)]
    [else (cons (at path a b 'box-type) tail)]))

(define (prefab-diff-at a b path tail)
  (cond
    [(not (equal? (prefab-key a) (prefab-key b)))
     (cons (at path a b 'prefab-key) tail)]
    [(not (= (length (prefab-fields a)) (length (prefab-fields b))))
     (cons (at path a b 'prefab-fields-length) tail)]
    [else
     (diff-at/ref a b path tail prefab-ref
       (for/list {[i (in-range (length (prefab-fields a)))]} i))]))

(define (diff-at/ref a b path tail ref indices)
  (cond
    [(empty? indices) tail]
    [else (diff-at
            (ref a (first indices))
            (ref b (first indices))
            (extend path (first indices))
            (diff-at/ref a b path tail ref (rest indices)))]))

(define (box-type=? a b)
  (iff (immutable? a) (immutable? b)))

(define (hash-type=? a b)
  (and
    (iff (immutable? a) (immutable? b))
    (iff (hash-weak? a) (hash-weak? b))))

(define (hash-compare=? a b)
  (and
    (iff (hash-eq? a) (hash-eq? b))
    (iff (hash-eqv? a) (hash-eqv? b))
    (iff (hash-equal? a) (hash-equal? b))))

(define (hash-keys=? a b)
  (and (hash-subset? a b) (hash-subset? b a)))

(define (hash-subset? a b)
  (for {[k (in-hash-keys a)]}
    (hash-has-key? b k)))

(define (at path expected actual reason)
  (prefab 'at path '#:reason reason '#:expected expected '#:actual actual))
(define (extend path step) (append path (list step)))
