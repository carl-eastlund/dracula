#lang dracula/core

(require/provide

  (validated-primitive-in racket

    ;; Equality comparisons:
    [(equal? equal-inputs equal-inputs) #:symbol EQUAL]
    [(eqv? eqv-inputs eqv-inputs) #:symbol EQL]
    [(eq? eq-inputs eq-inputs) #:symbol EQ]

    ;; Booleans:
    [(boolean? sexp-inputs) #:symbol BOOLEANP]
    [(not sexp-inputs)]

    ;; Pairs:
    [(cons? sexp-inputs)]
    [(cons sexp-inputs sexp-inputs)]
    [(car (cons-inputs sexp-inputs sexp-inputs))]
    [(cdr (cons-inputs sexp-inputs sexp-inputs))]

    ;; Lists:
    [(first (cons-inputs sexp-inputs list-inputs))]
    [(rest (cons-inputs sexp-inputs list-inputs))]

    ;; Symbols:
    [(symbol? sexp-inputs) #:symbol SYMBOL?]
    [(string->symbol string-inputs)]
    [(symbol->string symbol-inputs) #:symbol SYMBOL-NAME]

    ;; Keywords:
    [(keyword? sexp-inputs)]
    [(string->keyword string-inputs)]
    [(keyword->string keyword-inputs) #:symbol SYMBOL-NAME]

    ;; Strings:
    [(string? sexp-inputs)]
    [(string->list string-inputs)]
    [(list->string (list-of-inputs char-inputs))]
    [(string-length string-inputs) #:symbol LENGTH]
    [(string-append string-inputs string-inputs)
     #:as binary-string-append
     #:symbol STRING-APPEND]

    ;; Characters:
    [(char? sexp-inputs) #:symbol CHARACTERP]
    [(char->integer char-inputs) #:symbol CHAR-CODE]
    [(integer->char char-code-inputs) #:symbol CODE-CHAR]

    ;; Numbers:
    [(number? sexp-inputs) #:symbol ACL2-NUMBERP]
    [(rational? sexp-inputs)]
    [(integer? sexp-inputs)]
    [(exact-nonnegative-integer? sexp-inputs) #:as natural? #:symbol NATP]
    [(zero? number-inputs)]
    [(add1 number-inputs) #:symbol 1+]
    [(sub1 number-inputs) #:symbol 1-]
    [(make-rectangular rational-inputs rational-inputs) #:as complex]
    [(imag-part number-inputs) #:symbol IMAGPART]
    [(real-part number-inputs) #:symbol REALPART]
    [(numerator rational-inputs)]
    [(denominator rational-inputs)]
    [(+ number-inputs number-inputs) #:as binary-+]
    [(- number-inputs) #:as unary--]
    [(* number-inputs number-inputs) #:as binary-*]
    [(/ non-zero-number-inputs) #:as unary-/]
    [(= number-inputs number-inputs)]
    [(< rational-inputs rational-inputs)])

  (validated-primitive-in dracula/primitive

    ;; Special ACL2 functions:
    [(size sexp-inputs) #:symbol ACL2-COUNT]
    [(implies sexp-inputs sexp-inputs)])

  (primitive-in dracula/primitive

    ;; Error function, nothing to validate:
    [illegal 3]))
