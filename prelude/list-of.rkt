#lang dracula/base

(provide
  (all-defined-out))

(require
  dracula/prelude/sexp
  dracula/prelude/type)

(description LIST-OF
  (~component Elem #:> TYPE)
  (~define (is? x)
    (cond
      [(empty? x) #true]
      [(cons? x)
       (and ((dot Elem is?) (first x))
         (is? (rest x)))]
      [#:else #false])))

(generic (List-of T #:> TYPE) #:> LIST-OF #:where {[Elem #:= T]}
  (define (is? x)
    (cond
      [(empty? x) #true]
      [(cons? x)
       (and ((dot Elem is?) (first x))
         (is? (rest x)))]
      [#:else #false]))
  (component Type #:> TYPE
    #:where {[is? #:= is?]}))

(define-types List-of
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

(define-types List-of
  [List-of-Boolean
   List-of-Number
   List-of-Rational
   List-of-Integer
   List-of-Natural
   List-of-Symbol
   List-of-Keyword
   List-of-String
   List-of-Char
   List-of-List])

(define-match-conversion
  [list->string #:-> string?]
  [string->list #:-> list-of-char?])
