#lang racket

(require rackunit rackunit/text-ui "random.ss")

(define-syntax (check-dist stx)
  (syntax-case stx ()
    [(_ c p e)
     (syntax/loc stx
       (let* ([count c]
              [pred p]
              [dist (lambda () e)])
         (with-check-info
          (['count count]
           ['predicate 'p]
           ['distribution 'e])
          (let* ([vals (for/list ([i (in-range count)]) (dist))]
                 [fails (filter (negate pred) vals)])
            (unless (null? fails)
              (with-check-info
               (['failures fails])
               (fail-check)))))))]))

(define-syntax (test-dist stx)
  (syntax-case stx ()
    [(_ c p e)
     (syntax/loc stx
       (test-case (format "~s" 'e) (check-dist c p e)))]))

(run-tests
 (test-suite "FastTest"
   (test-suite "distributions"
     (test-suite "booleans"
       (test-suite "random-boolean/fair"
         (test-dist 10 boolean? (random-boolean/fair)))
       (test-suite "random-boolean/bernoulli"
         (test-dist 10 boolean? (random-boolean/bernoulli 2/3))
         (test-dist 10 (lambda (x) (eq? x #t)) (random-boolean/bernoulli 1))
         (test-dist 10 (lambda (x) (eq? x #f)) (random-boolean/bernoulli 0)))
       (test-suite "random-boolean"
         (test-dist 10 boolean? (random-boolean))
         (test-dist 10 boolean? (random-boolean 2/3))
         (test-dist 10 (lambda (x) (eq? x #t)) (random-boolean 1))
         (test-dist 10 (lambda (x) (eq? x #f)) (random-boolean 0))))
     (test-suite "numbers"
       (test-suite "random-natural/binomial"
         (test-dist 100
                    (lambda (x) (and (exact-integer? x) (<= 0 x 10)))
                    (random-natural/binomial 10 2/3)))
       (test-suite "random-natural/geometric"
         (test-dist 1000
                    exact-nonnegative-integer?
                    (random-natural/geometric 1/50 0))
         (test-dist 1000
                    exact-positive-integer?
                    (random-natural/geometric 1/50 1)))
       (test-suite "random-natural/pascal"
         (test-dist 1000
                    exact-nonnegative-integer?
                    (random-natural/pascal 10 1/50)))
       (test-suite "random-natural/poisson"
         (test-dist 1000
                    exact-nonnegative-integer?
                    (random-natural/poisson 22/7)))
       (test-suite "random-integer/skellam"
         (test-dist 1000 exact-integer? (random-integer/skellam 50))
         (test-dist 1000 exact-integer? (random-integer/skellam 50 75)))
       (test-suite "random-integer/uniform"
         (test-dist 1000
                    (lambda (x) (and (exact-integer? x) (<= -100 x 100)))
                    (random-integer/uniform -100 100))
         (test-dist 10
                    (lambda (x) (and (exact-integer? x) (= x 10)))
                    (random-integer/uniform 10 10)))
       (test-suite "random-natural"
         (test-dist 10000 exact-nonnegative-integer? (random-natural)))
       (test-suite "random-integer"
         (test-dist 10000 exact-integer? (random-integer)))
       (test-suite "random-rational"
         (test-dist 10000
                    (lambda (x) (and (rational? x) (exact? x)))
                    (random-rational)))
       (test-suite "random-exact"
         (test-dist 10000
                    (lambda (x) (and (number? x) (exact? x)))
                    (random-exact)))
       (test-suite "random-real"
         (test-dist 10000 inexact-real? (random-real)))
       (test-suite "random-inexact"
         (test-dist 10000
                    (lambda (x) (and (number? x) (inexact? x)))
                    (random-real)))
       (test-suite "random-number"
         (test-dist 10000 number? (random-number))))
     (test-suite "lists"
       (test-suite "random-list"
         (test-dist 1000
                    (lambda (x) (and (list? x) (andmap exact-integer? x)))
                    (random-list random-integer))
         (test-dist 1000
                    (lambda (x)
                      (and (list? x)
                           (<= (length x) 10)
                           (andmap exact-integer? x)))
                    (random-list random-integer
                                 #:len (random-natural/binomial 10 1/2)))))
     (test-suite "text"
       (test-suite "random-char"
         (test-dist 100 char? (random-char)))
       (test-suite "random-string"
         (test-dist 1000 string? (random-string))))
     #|
     (test-suite "vectors")
     (test-suite "custom"
       (let* ()

         (define (custom-sexp? v)
           (or (number? v)
               (string? v)
               (symbol? v)
               (and (list? v)
                    (andmap custom-sexp? v))))

         (define (random-custom-sexp :size (random-natural/poisson 4))
           (if (<= size 0)
               (random-number)
               (let* ([len (random-integer/uniform 1 size)]
                      [sizes (random-naturals/split len (- size len))])
                 (map (lambda (size) (random-custom-sexp #:size size)) sizes))))

         (test-dist 1000 custom-sexp? (random-custom-sexp))))
     |#
     )))
