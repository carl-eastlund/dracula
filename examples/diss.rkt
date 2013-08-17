#lang dracula

(description TYPE
  (~define (is? x))
  (~theorem (is?/boolean? x)
    (boolean? (is? x))))

(description LISTOF
  (~component Elem #:> TYPE)
  (~define (filter xs))
  (~theorem (filter/member x xs)
    (implies (and (list? xs) (member? x (filter xs))) ((dot Elem is?) x)))
  (define-syntax (add stx)
    (syntax-parse stx
      [(_ e1 e2)
       #'(let {[x e1] [xs e2]}
           (if ((dot Elem is?) x) (cons x xs) xs))]))
  (define-syntax (make stx)
    (syntax-parse stx
      [(_) #'empty]
      [(_ e . es) #'(add e (make . es))])))

(generic (Listof E #:> TYPE) #:> LISTOF
  #:where {[Elem #:= E]}
  (define (filter xs)
    (cond
      [(empty? xs) xs]
      [(cons? xs) (add (first xs) (filter (rest xs)))]))
  (theorem (filter/member x xs)
    (implies (and (list? xs) (member? x (filter xs))) ((dot E is?) x))))

(component Integer #:> TYPE
  #:where {[is? #:= integer?]}
  (theorem (is?/boolean? x)
    (boolean? (integer? x))))

(instance Listof-Integer (Listof Integer))
(use Listof-Integer)
(Listof-Integer.make 1 "two")
