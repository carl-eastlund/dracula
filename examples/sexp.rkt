#lang dracula

(mutual-recursion sexp

  (define (sexp? x) #:measure (add1 (* 2 (size x)))
    (or (symbol? x)
      (sexp-list? x)))

  (define (sexp-list? x) #:measure (* 2 (size x))
    (cond
      [(empty? x) #true]
      [(cons? x) (and (sexp? (first x))
                   (sexp-list? (rest x)))]
      [#:else #false])))

(mutual-recursion subst

  (define (subst/sexp s a b) #:measure (add1 (* 2 (size s)))
    (cond
      [(symbol? s) (if (equal? s a) b s)]
      [#:else (subst/list s a b)]))

  (define (subst/list s a b) #:measure (* 2 (size s))
    (cond
      [(empty? s) '()]
      [(cons? s) (cons (subst/sexp (first s) a b)
                   (subst/list (rest s) a b))])))

(mutual-induction subst-same subst

  (theorem (subst/sexp-same s a b)
    (implies (and (sexp? s) (equal? a b))
      (equal? (subst/sexp s a b) s)))

  (theorem (subst/list-same s a b)
    (implies (and (sexp-list? s) (equal? a b))
      (equal? (subst/list s a b) s))))
