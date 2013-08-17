#lang dracula/core

(provide
  (all-defined-out))

(require
  (for-syntax
    dracula/proof/term)
  dracula/prelude/base/primitive)

(fold-shorthand (or e:expr ... e0:expr)
  (let {[disjunct e]} (if disjunct disjunct e0))
  #false)

(fold-shorthand (and e:expr ... e0:expr)
  (if e e0 #false)
  #true)

(begin-for-syntax
  (define-splicing-syntax-class cond-clauses
    #:attributes {body}
    (pattern (~seq [test:expr result:expr] ~! rest:cond-clauses)
      #:attr body #'(if test result rest.body))
    (pattern (~seq [disjunct:expr] ~! rest:cond-clauses)
      #:attr body #'(or disjunct rest.body))
    (pattern (~seq [#:else ~! default:expr])
      #:attr body (@ default))
    (pattern (~seq)
      #:attr body #'(error 'cond "all clauses failed"))))

(define-shorthand (cond clauses:cond-clauses) clauses.body)

(define-shorthand (imply [hyp:expr conc:expr] ...)
  (and (implies hyp conc) ...))

(fold-shorthand (+ e:expr ... e0:expr) (binary-+ e e0) 0)
(fold-shorthand (* e:expr ... e0:expr) (binary-* e e0) 1)

(define-shorthand -
  [(_ e:expr) (unary-- e)]
  [(_ e0:expr e:expr ...+) (+ e0 (- e) ...)])

(define-shorthand /
  [(_ e:expr) (unary-/ e)]
  [(_ e0:expr e:expr ...+) (* e0 (/ e) ...)])

(define-shorthand (> x:expr y:expr) (< y x))
(define-shorthand (<= x:expr y:expr) (not (> x y)))
(define-shorthand (>= x:expr y:expr) (not (< x y)))

(fold-shorthand (list* e:expr ... e0:expr) (cons e e0))
(define-shorthand (list e:expr ...) (list* e ... '()))

(fold-shorthand (string-append e:expr ... e0:expr)
  (binary-string-append e e0)
  "")

(begin-for-syntax

  (define (unquote-transformer stx)
    (wrong-syntax stx
      "cannot use outside of quasiquote"))

  (define-syntax-class quasi
    #:attributes {result quote?}
    #:literals {unquote unquote-splicing}
    (pattern (unquote ~! result:expr) #:attr quote? #false)
    (pattern ((unquote-splicing ~! left:expr) . right:quasi)
      #:attr result #'(append left right.result)
      #:attr quote? #false)
    (pattern ((~and _ ~! left:quasi) . right:quasi)
      #:attr quote? (and (@ left.quote?) (@ right.quote?))
      #:attr result
      (if (@ quote?)
        #'(quote (left . right))
        #'(cons left.result right.result)))
    (pattern (~and unquote ~! (~fail "misuse of unquote"))
      #:attr quote? #false
      #:attr result #false)
    (pattern (~and unquote-splicing ~! (~fail "misuse of unquote-splicing"))
      #:attr quote? #false
      #:attr result #false)
    (pattern (~and atom (~not (_ . _)))
      #:attr quote? #true
      #:attr result #'(quote atom))))

(define-syntax unquote unquote-transformer)
(define-syntax unquote-splicing unquote-transformer)

(define-syntax (quasiquote stx)
  (syntax-parse stx
    [(_ q:quasi) (@ q.result)]))

(begin-for-syntax

  (define (parse-format-string stx fmt)
    (define-values {parsed keys}
      (parse-format-list stx (string->list fmt)))
    (values (list->string parsed) keys))

  (define (parse-format-list stx chars [free-keys char-keys])
    (match! chars
      [(list* #\~ type others)
       (define spec
         (match! type
           [#\s #\x]
           [#\a #\f]
           [_ (wrong-syntax stx
                "invalid ~~ specifier; expected only ~~s or ~~a, but got ~~~a"
                type)]))
       (unless (cons? free-keys)
         (wrong-syntax stx
           "too many ~~ specifiers, ran out of keys"))
       (define key (first free-keys))
       (define-values {parsed keys}
         (parse-format-list stx others (rest free-keys)))
       (values (list* #\~ spec key parsed) (cons key keys))]
      [(list #\~)
       (wrong-syntax stx
         "invalid ~~ specifier; ~~ cannot come at the end of a format string")]
      [(cons c others)
       (define-values {parsed keys}
         (parse-format-list stx others free-keys))
       (values (cons c parsed) keys)]
      ['() (values '() '())]))

  (define char-keys
    (append
      (string->list "0123456789")
      (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
      (string->list "abcdefghijklmnopqrstuvwxyz")
      (filter-not (disjoin char-alphabetic? char-numeric?)
        (sort (set->list standard-char-set) char<?)))))

(define-syntax (error stx)
  (syntax-parse stx
    [(_ name:expr fmt:str arg:expr ...)
     #:do
     {(define-values {parsed-fmt-string fmt-key-chars}
        (parse-format-string #'fmt (syntax-e (@ fmt))))
      (define expected-count (length fmt-key-chars))
      (define actual-count (length (@ arg)))}
     #:fail-unless (= actual-count expected-count)
     (format "format string expects ~a, but got ~a"
       (count->phrase expected-count "argument")
       (count->phrase actual-count "argument"))
     (define/syntax-parse parsed-fmt parsed-fmt-string)
     (define/syntax-parse [fmt-key ...] fmt-key-chars)
     #'(illegal name 'parsed-fmt (list (cons 'fmt-key arg) ...))]))
