#lang dracula/kernel

(provide
  ? _ ...
  match match*
  match/implies match*/implies
  define-derived-match match*/derived
  define-match-type
  define-match-conversion)

(require
  (for-syntax mischief macro-debugger/emit)
  dracula/prelude/core
  dracula/prelude/base/primitive
  dracula/prelude/base/shorthand)

(begin-for-syntax

  (define-syntax-class pat

    #:attributes {[test 1] [bind 1] [value 1]}

    #:literals {?
                [_blank _]
                [...dots ...]
                quote
                quasiquote
                unquote
                unquote-splicing}

    (pattern (? ~! x:id e:expr)
      #:attr [test 1] (list #'(let {[x pattern-input]} e))
      #:attr [bind 1] (list (@ x))
      #:attr [value 1] (list #'pattern-input))

    (pattern (quote ~! datum)
      #:attr [test 1] (list #'(equal? pattern-input (quote datum)))
      #:attr [bind 1] '()
      #:attr [value 1] '())

    (pattern (quasiquote ~! :quasi))

    (pattern (~and (~or quote quasiquote unquote unquote-splicing) ~!)
      #:fail-when #true "misuse of pattern keyword"
      #:attr [test 1] '()
      #:attr [bind 1] '()
      #:attr [value 1] '())

    (pattern (ty:type-id ~! arg:pat ...)
      #:do {(define expected-arity (length (type-selectors (@ ty.value))))
            (define actual-arity (length (@ arg)))}
      #:fail-unless (= actual-arity expected-arity)
      (format "~s expects ~a arguments, but got ~a arguments"
        (syntax-e (@ ty))
        expected-arity
        actual-arity)

      #:do
      [(define-values {tests binds vals}
         (type-pattern (@ ty.value) (@ arg.test) (@ arg.bind) (@ arg.value)))]

      #:attr [test 1] tests
      #:attr [bind 1] binds
      #:attr [value 1] vals)

    (pattern (~and term (macro:macro-id ~! . _))
      #:do
      [(define stx
         (apply-macro-in-scope (@ macro) (@ macro.value) (@ term)))
       (define-values {tests binds vals}
         (syntax-parse stx
           [p:pat (values (@ p.test) (@ p.bind) (@ p.value))]))]
      #:attr [test 1] tests
      #:attr [bind 1] binds
      #:attr [value 1] vals)

    (pattern (bad-macro:id . tail)
      #:when (not (dict-has-key? macro-registry (@ bad-macro)))
      #:when (for/or {[macro (in-dict-keys macro-registry)]}
               (free-identifier=? (@ bad-macro) macro))
      #:do {(emit-remark "bad macro:" (@ bad-macro) (dict-keys macro-registry))
            (wrong-syntax #'bad-macro "identifier mismatch in match macros")}
      #:attr [test 1] '()
      #:attr [bind 1] '()
      #:attr [value 1] '())

    (pattern (bad-type:id . tail)
      #:when (not (dict-has-key? type-registry (@ bad-type)))
      #:when (for/or {[type (in-dict-keys type-registry)]}
               (free-identifier=? (@ bad-type) type))
      #:do {(emit-remark "bad type:" (@ bad-type) (dict-keys type-registry))
            (wrong-syntax #'bad-type "identifier mismatch in match types")}
      #:attr [test 1] '()
      #:attr [bind 1] '()
      #:attr [value 1] '())

    (pattern (head ~! . tail)
      #:fail-when (@ head)
      "expected a match pattern macro or constructor"
      #:attr [test 1] '()
      #:attr [bind 1] '()
      #:attr [value 1] '())

    (pattern _blank
      #:attr [test 1] '()
      #:attr [bind 1] '()
      #:attr [value 1] '())

    (pattern ...dots
      #:fail-when #true
      "ellipsis matching is not supported"
      #:attr [test 1] '()
      #:attr [bind 1] '()
      #:attr [value 1] '())

    (pattern name:id
      #:attr [test 1] '()
      #:attr [bind 1] (list (@ name))
      #:attr [value 1] (list #'pattern-input))

    (pattern (~and atom (~not (~or _:keyword () (_ . _))))
      #:attr [test 1] (list #'(equal? pattern-input (quote atom)))
      #:attr [bind 1] '()
      #:attr [value 1] '()))

  (define-syntax-class quasi
    #:attributes {quote? [test 1] [bind 1] [value 1]}
    #:literals {unquote unquote-splicing}

    (pattern (unquote ~! :pat)
      #:attr quote? #false)

    (pattern ((~and _ ~! left:quasi) . right:quasi)

      #:attr quote? (and (@ left.quote?) (@ right.quote?))

      #:do
      [(define-values {tests binds vals}
         (cond
           [(@ quote?)
            (define test
              #'(equal? pattern-input
                  (quote (left . right))))
            (values (list test) '() '())]
           [else
            (type-pattern
              (type #'cons? (list #'first #'rest))
              (list (@ left.test) (@ right.test))
              (list (@ left.bind) (@ right.bind))
              (list (@ left.value) (@ right.value)))]))]

      #:attr [test 1] tests
      #:attr [bind 1] binds
      #:attr [value 1] vals)

    (pattern
      (~and unquote ~!
        (~fail "misuse of unquote"))
      #:attr quote? #false
      #:attr [test 1] '()
      #:attr [bind 1] '()
      #:attr [value 1] '())

    (pattern
      (~and unquote-splicing ~!
        (~fail "unquote-splicing patterns are not supported"))
      #:attr quote? #false
      #:attr [test 1] '()
      #:attr [bind 1] '()
      #:attr [value 1] '())

    (pattern (~and atom (~not (_ . _)))
      #:attr quote? #true
      #:attr [test 1] (list #'(equal? pattern-input (quote atom)))
      #:attr [bind 1] '()
      #:attr [value 1] '()))

  (define (type-pattern ty arg-tests arg-binds arg-vals)

    (define/for/append-lists
        {tests binds vals}
        {[selector (in-list (type-selectors ty))]
         [sub-tests (in-list arg-tests)]
         [sub-binds (in-list arg-binds)]
         [sub-vals (in-list arg-vals)]}

      (define/syntax-parse select selector)
      (define/syntax-parse temp (fresh selector))

      (define tests
        (syntax-parse sub-tests
          [() '()]
          [(sub-test ...+)
           (list
             #'(let {[temp (select pattern-input)]}
                 (with-pattern-input temp
                   (and sub-test ...))))]))

      (define-values {binds vals}
        (syntax-parse (list sub-binds sub-vals)
          [{() ()} (values '() '())]
          [{(sub-bind:id ...+) (sub-val:expr ...+)}
           (values
             (syntax->list #'[temp sub-bind ...])
             (syntax->list
               #'[(select pattern-input)
                  (with-pattern-input temp sub-val)
                  ...]))]))

      (values tests binds vals))

    (define/syntax-parse type? (type-predicate ty))

    (values
      (cons #'(type? pattern-input) tests)
      binds
      vals))

  (define type-registry
    (make-free-id-table))

  (struct type [predicate selectors])

  (define-syntax-class type-id
    #:attributes {value}
    (pattern type:id
      #:attr value (dict-ref type-registry (@ type) #false)
      #:fail-unless (type? (@ value))
      "expected a match pattern constructor name"))

  (define (register-match-type constructor predicate selectors
            #:source [source #false])
    (parameterize {[current-syntax-context source]}
      (unless (identifier-binding constructor)
        (wrong-syntax constructor
          "cannot register undefined name as a match pattern constructor"))
      (dict-set! type-registry constructor
        (type predicate selectors))))

  (define macro-registry
    (make-free-id-table))

  (define-syntax-class macro-id
    #:attributes {value}
    (pattern macro:id
      #:attr value (dict-ref macro-registry (@ macro) #false)
      #:fail-unless (@ value)
      "expected a match pattern macro name"))

  (define (register-match-macro id
            #:source [source #false])
    (parameterize {[current-syntax-context source]}
      (scope-static-value id
        #:failure
        (lambda ()
          (wrong-syntax id
            "cannot register undefined name as a match pattern macro"))
        #:success
        (lambda (value)
          (unless (macro? value)
            (wrong-syntax id
              "expected ~a, but found ~a: ~v"
              "name bound as a macro"
              "a name bound as something else"
              value))
          (dict-set! macro-registry id value)))))

  (define (macro? x)
    (or
      (rename-transformer? x)
      (set!-transformer? x)
      (procedure? x)))

  (define (apply-macro-in-scope name value stx0)

    (define proc
      (cond
        [(rename-transformer? value)
         (rename-transformer
           (rename-transformer-target value))]
        [(set!-transformer? value)
         (set!-transformer-procedure value)]
        [(procedure? value) value]
        [else
         (wrong-syntax name
           "expected a macro name, but found a name bound statically to ~v"
           value)]))

    (with-new-scope

      (define/syntax-parse start (fresh name))
      (define/syntax-parse finish (fresh name))

      (scope-bind-syntax! (@ start)
        (syntax-parser
          [(_ in)
           (define stx (proc (@ in)))
           (to-syntax #:stx stx
             (list #'finish stx))]))

      (define stx
        (expand-in-scope
          (to-syntax #:stx stx0
            (list (@ start) stx0))
          #:stop-at (list #'finish)))

      (syntax-parse stx
        [(_ out)
         ;; transfer properties
         (to-syntax
           (syntax-e (@ out))
           #:stx (@ out)
           #:prop stx)])))

  (define (pattern-transformer stx)
    (wrong-syntax stx
      "cannot use pattern outside of match")))

(define-syntax ? pattern-transformer)
(define-syntax _ pattern-transformer)
(define-syntax ... pattern-transformer)

(define-syntax-parameter pattern-input
  (lambda (stx)
    (wrong-syntax stx
      "cannot be used outside of match")))

(define-syntax (with-pattern-input stx)
  (syntax-parse stx
    [(_ name:id body:expr)
     #'(syntax-parameterize
           {[pattern-input (rename-transformer #'name)]}
         body)]))

(define-syntax (let*/with-pattern stx)
  (syntax-parse stx
    [(_ {(input:id [lhs:id rhs:expr] ...) ...} body:expr)
     (foldr
       (lambda (input-stx lhs-stxs rhs-stxs body-stx)
         (define/syntax-parse input input-stx)
         (define/syntax-parse [lhs ...] lhs-stxs)
         (define/syntax-parse [rhs ...] rhs-stxs)
         (define/syntax-parse body body-stx)
         #'(with-pattern-input input
             (let* {[lhs rhs] ...} body)))
       #'body
       (@ input)
       (@ lhs)
       (@ rhs))]))

(define-syntax (match*/derived stx)
  (syntax-parse stx
    [(_ original . _)
     (syntax-parse stx #:context (@ original)
       [(_ _ dispatch:id {arg:expr ...}
           [(~and clause {lhs:pat ...}) rhs:expr]
           ...)

        #:do {(define arity (length (@ arg)))}
        #:fail-when
        (for/first {[stx (in-list (@ clause))]
                    [ids (in-list (@ lhs))]
                    #:when (not (= (length ids) arity))}
          stx)
        (format "clause has the wrong number of patterns; expected ~a" arity)

        (define/syntax-parse {temp ...}
          (for/list {[stx (in-list (@ arg))]} (fresh 'match)))

        #'(let {[temp arg] ...}
            (dispatch
              [(and
                 (with-pattern-input temp
                   (and lhs.test ...))
                 ...)
               (let*/with-pattern
                   {(temp [lhs.bind lhs.value] ...) ...}
                 rhs)]
              ...))])]))

(define-syntax (define-derived-match stx)
  (syntax-parse stx
    [(_ name:id name*:id dispatch:id)
     #'(...
         (define-syntaxes {name name*}
           (values
             (syntax-parser
               [(~and original (_ arg [lhs rhs] ...))
                #'(match*/derived original dispatch {arg}
                    [{lhs} rhs]
                    ...)])
             (syntax-parser
               [(~and original (_ {arg ...} [{lhs ...} rhs] ...))
                #'(match*/derived original dispatch {arg ...}
                    [{lhs ...} rhs]
                    ...)]))))]))

(define-derived-match match match* cond)
(define-derived-match match/implies match*/implies imply)

(begin-for-syntax
  (define (lift-syntax-effect stx)
    (define/syntax-parse effect stx)
    #'(define-syntaxes {}
        (begin0 (values)
          effect))))

(define-syntax (define-match-macro stx)
  (syntax-parse stx
    [(_ name:id)
     (define/syntax-parse orig stx)
     (lift-syntax-effect
       #'(register-match-macro #:source (quote-syntax orig)
           (quote-syntax name)))]))

(define-syntax (define-match-type stx)
  (syntax-parse stx
    [(_ (predicate:id (constructor:id selector:id ...)))
     (define/syntax-parse orig stx)
     (lift-syntax-effect
       #'(register-match-type #:source (quote-syntax orig)
           #'constructor
           #'predicate
           (list #'selector ...)))]))

(define-syntax (define-match-conversion stx)
  (syntax-parse stx
    [(_ [forward:id #:-> to:id]
        [backward:id #:-> from:id])
     #'(begin
         (define-match-type (to (forward backward)))
         (define-match-type (from (backward forward))))]))

(define-match-macro list)
(define-match-macro list*)

(define-match-type (cons? (cons first rest)))
(define-match-type (number? (complex real-part imag-part)))
(define-match-type (rational? (/ numerator denominator)))

(define-match-conversion
  [string->symbol #:-> symbol?]
  [symbol->string #:-> string?])

(define-match-conversion
  [string->keyword #:-> keyword?]
  [keyword->string #:-> string?])

(define-match-conversion
  [integer->char #:-> char?]
  [char->integer #:-> integer?])
