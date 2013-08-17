#lang mischief

(provide
  (rename-out
    [booleans boolean-inputs]
    [naturals natural-inputs]
    [integers integer-inputs]
    [rationals rational-inputs]
    [non-zero-rationals non-zero-rational-inputs]
    [numbers number-inputs]
    [non-zero-numbers non-zero-number-inputs]
    [strings string-inputs]
    [symbols symbol-inputs]
    [keywords keyword-inputs]
    [chars char-inputs]
    [char-codes char-code-inputs]
    [atoms atom-inputs]
    [sexps sexp-inputs]
    [cons-stream cons-inputs]
    [list-stream list-of-inputs]
    [lists list-inputs]
    [stream-interleave combine-inputs]
    [eq?-stream eq-inputs]
    [eqv?-stream eqv-inputs]
    [equal?-stream equal-inputs]
    [constant-stream constant-inputs]))

(require dracula/proof/term)

(define (constant-stream x)
  (define-stream the-stream
    (stream-cons x the-stream))
  the-stream)

(define (naturals-from n)
  (stream-cons n
    (naturals-from (add1 n))))

(define naturals
  (naturals-from 0))

(define integers
  (stream-cons 0
    (stream-interleave
      (naturals-from 1)
      (stream-map -
        (naturals-from 1)))))

(define (fibonacci-rationals n1 d1 n2 d2)
  (define n3 (+ n1 n2))
  (define d3 (+ d1 d2))
  (stream-cons (/ n3 d3)
    (stream-interleave
      (fibonacci-rationals n3 d3 n1 d1)
      (fibonacci-rationals n3 d3 n2 d2))))

(define positive-rationals (fibonacci-rationals 0 1 +1 0))
(define negative-rationals (fibonacci-rationals 0 1 -1 0))
(define positive-imaginaries (fibonacci-rationals 0 1 0+1i 0))
(define negative-imaginaries (fibonacci-rationals 0 1 0-1i 0))

(define non-zero-rationals
  (stream-interleave
    positive-rationals
    negative-rationals))

(define rationals
  (stream-cons 0
    non-zero-rationals))

(define complex-basis
  (stream
    positive-rationals
    negative-rationals
    positive-imaginaries
    negative-imaginaries))

(define complexes
  (stream-cross-product make-rectangular
    non-zero-rationals
    non-zero-rationals))

(define non-zero-numbers
  (stream-interleave*
    (stream-append complex-basis
      (stream complexes))))

(define numbers
  (stream-cons 0
    non-zero-numbers))

(define ALPHAS (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(define alphas (string->list "abcdefghijklmnopqrstuvwxyz"))
(define digits (string->list "0123456789"))

(define symbol-punct-set
  (set-subtract symbol-char-set
    (list->seteqv ALPHAS)
    (list->seteqv alphas)
    (list->seteqv digits)))

(define punct (sort (set->list symbol-punct-set) char<?))
(define special (sort (set->list special-char-set) char<?))

(define chars
  (stream-interleave* (list alphas ALPHAS digits punct special)))

(define char-codes
  (stream-map char->integer chars))

(define symbol-chars
  (stream-interleave* (list ALPHAS punct)))

(define (list-stream elems)
  (define-stream lists
    (stream-cons '()
      (stream-cross-product cons elems lists)))
  lists)

(define (string-stream chars)
  (stream-map list->string
    (list-stream chars)))

(define strings (string-stream chars))

(define symbols
  (stream-map string->symbol
    (stream-rest (string-stream symbol-chars))))

(define keywords
  (stream-map string->keyword
    (stream-rest (string-stream symbol-chars))))

(define booleans
  (list #f #t))

(define singletons
  (list '() #f #t))

(define atoms
  (stream-interleave singletons
    (stream-interleave numbers
      (stream-interleave strings
        (stream-interleave symbols chars)))))

(define (sexp-stream atom-stream)
  (define-stream sexps
    (stream-interleave*
      (stream-cons atom-stream
        (stream-cross-product* cons sexps sexps))))
  sexps)

(define sexps
  (sexp-stream atoms))

(define lists
  (list-stream sexps))

(define (cons-stream car-stream cdr-stream)
  (stream-cross-product cons car-stream cdr-stream))

(define eq?-stream (stream-interleave singletons symbols))
(define eqv?-stream (stream-interleave singletons symbols numbers chars))
(define equal?-stream sexps)

(module+ main
  (define the-count (make-parameter 20))
  (define the-stream (make-parameter sexps))
  (command-line
    #:once-each
    [{"-c" "--count"} number "specify the number of iterations"
     (the-count
       (or (string->number number)
         (error '--count "expected a natural number, got ~s" number)))]
    #:once-any
    [{"--sexp"} "generate s-expressions" (the-stream sexps)]
    [{"--atom"} "generate atoms" (the-stream atoms)]
    [{"--symbol"} "generate symbols" (the-stream symbols)]
    [{"--string"} "generate strings" (the-stream strings)]
    [{"--char"} "generate characters" (the-stream chars)]
    [{"--complex"} "generate complex numbers" (the-stream numbers)]
    [{"--rational"} "generate rational numbers" (the-stream rationals)]
    [{"--integer"} "generate integers" (the-stream integers)]
    [{"--natural"} "generate natural numbers" (the-stream naturals)]
    #:args {}
    (for {[x (in-list (stream-take (the-stream) (the-count)))]}
      (printf "~s\n" x))))
