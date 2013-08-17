#lang dracula/base

(provide
  (all-defined-out))

(require
  (for-syntax mischief)
  dracula/prelude/sexp)

(description TYPE
  (~define (is? x)))

(component Boolean #:> TYPE #:where {[is? #:= boolean?]})
(component Number #:> TYPE #:where {[is? #:= number?]})
(component Rational #:> TYPE #:where {[is? #:= rational?]})
(component Integer #:> TYPE #:where {[is? #:= integer?]})
(component Natural #:> TYPE #:where {[is? #:= natural?]})
(component Symbol #:> TYPE #:where {[is? #:= symbol?]})
(component Keyword #:> TYPE #:where {[is? #:= keyword?]})
(component String #:> TYPE #:where {[is? #:= string?]})
(component Char #:> TYPE #:where {[is? #:= char?]})
(component List #:> TYPE #:where {[is? #:= list?]})

(define-syntax (define-type stx)
  (syntax-parse stx
    [(_ Generic:id Arg:id)
     (define/syntax-parse Generic-of-Arg
       (format-id (@ Arg) "~a-~a" (@ Generic) (@ Arg)))
     (define/syntax-parse Generic-of-Arg.is?
       (format-id (@ Arg) "~a.is?" (@ Generic-of-Arg)))
     (define/syntax-parse generic-of-arg?
       (format-id (@ Arg) "~a?"
         (identifier-downcase (@ Generic-of-Arg))))
     #'(begin
         (instance Generic-of-Arg (Generic Arg))
         (define-id-shorthand generic-of-arg? (dot Generic-of-Arg is?)))]))

(define-shorthand (define-types Generic:id [Arg:id ...])
  (begin (define-type Generic Arg) ...))
