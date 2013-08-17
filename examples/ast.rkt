#lang dracula

(datatype ast
  [expr
   (var [name symbol?])
   (fun [formals set-of-symbol?] [body expr?])
   (app [fun expr?] [args expr-list?])]
  [expr-list #:list-of expr?])

(include-book "arithmetic-5/top" #:dir #:system)

(mutual-recursion subst

  (define (subst/one e x v) #:measure (size e)
    (match e
      [(var y) (if (equal? x y) v (var y))]
      [(fun ys e) (if (member? x ys) (fun ys e) (fun ys (subst/one e x v)))]
      [(app f es) (app (subst/one f x v) (subst/many es x v))]))

  (define (subst/many es x v) #:measure (size es)
    (match es
      ['() '()]
      [(cons e es) (cons (subst/one e x v) (subst/many es x v))])))

(mutual-recursion free?

  (define (free?/one x e) #:measure (size e)
    (match e
      [(var y) (equal? x y)]
      [(fun ys e) (if (member? x ys) #false (free?/one x e))]
      [(app f es) (or (free?/one x f) (free?/many x es))]))

  (define (free?/many x es) #:measure (size es)
    (match es
      ['() #false]
      [(cons e es) (or (free?/one x e) (free?/many x es))])))

(mutual-induction subst/free? subst

  (theorem (subst/free?/one e x v)
    (implies (and (expr? e) (symbol? x) (expr? v) (not (free?/one x e)))
      (equal? (subst/one e x v) e)))

  (theorem (subst/free?/many es x v)
    (implies (and (expr-list? es) (symbol? x) (expr? v) (not (free?/many x es)))
      (equal? (subst/many es x v) es))))
