#lang mischief

(require
  (for-syntax mischief)
  (for-template racket/base))

(require/define-literals/ids runtime
  (for-template dracula/expansion/runtime))

(define-syntax (define/provide-pattern stx)
  (syntax-parse stx
    [(_ name:id . body)
     #`(begin
         (provide name)
         #,(syntax/loc stx
             (define-pattern name . body)))]))

(define-syntax (define-pattern stx)
  (syntax-parse stx
    [(_ name:id . body)
     (syntax/loc stx
       (define-syntax-class name
         #:literal-sets
           {[kernel-literals #:at name]
            [runtime-literals #:at name]}
         #:no-delimit-cut
         . body))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definitions

(define/provide-pattern $begin-for-syntax-defn
  (pattern (begin-for-syntax ~! body:expr ...)))

(define/provide-pattern $require-defn
  (pattern (#%require ~! spec:expr ...)))

(define/provide-pattern $provide-defn
  (pattern (#%provide ~! spec:expr ...)))

(define/provide-pattern $syntaxes-defn
  (pattern (define-syntaxes ~! {name:id ...} body:expr)))

(define/provide-pattern $primitive-defn
  (pattern
    ({~literal #%plain-lambda} {} ({~literal quote} #:primitive) ~!
     name:id
     ({~literal quote} arity:nat)
     ({~literal quote} symbol:id)
     ({~literal quote} package:id))))

(define/provide-pattern $include-book-defn
  (pattern
    ({~literal #%plain-lambda} {} ({~literal quote} #:include-book) ~!
     path:str
     option:expr ...)))

(define/provide-pattern $in-theory-defn
  (pattern
    ({~literal #%plain-lambda} {} ({~literal quote} #:in-theory) ~!
     theory:expr)))

(define/provide-pattern $term-defn
  (pattern (define-values {name:id} term:expr)))

(define/provide-pattern $fun-defn
  #:attributes {name [formal 1] body measure [goal 1]}
  (pattern (~delimit-cut :$term-defn) #:with :$fun-term (@ term)))

(define/provide-pattern $thm-defn
  #:attributes {name [formal 1] body [rule 1] [goal 1]}
  (pattern (~delimit-cut :$term-defn) #:with :$thm-term (@ term)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terms

(define/provide-pattern $fun-term
  #:attributes {[formal 1] body measure [goal 1]}
  (pattern
    ({~literal #%plain-lambda} {formal:id ...}
     ({~literal quote} #:function) ~!
     (~or
       (~optional
         (~describe "measure"
           ({~literal #%plain-lambda} {} ({~literal quote} #:measure) ~!
            measure:expr)))
       (~optional
         (~describe "goals"
           ({~literal #%plain-lambda} {} ({~literal quote} #:goals) ~!
            goal:expr ...))))
     ...
     body:expr)))

(define/provide-pattern $thm-term
  #:attributes {[formal 1] body [rule 1] [goal 1]}
  (pattern
    ({~literal #%plain-lambda} {formal:id ...}
     ({~literal quote} #:theorem) ~!
     (~or
       (~optional
         (~describe "rules"
           ({~literal #%plain-lambda} {}
            ({~literal quote} #:rule-classes) ~! rule:expr ...)))
       (~optional
         (~describe "goals"
           ({~literal #%plain-lambda} {}
            ({~literal quote} #:goals) ~! goal:expr ...))))
     ...
     body:expr)))

(define/provide-pattern $inst-term
  #:attributes {[label 1] [body 1]}
  (pattern
    ({~literal #%plain-app} {~literal #%instance} ~!
     ({~literal quote} [label:id ...])
     :$inst-term-body)
    #:do {(define label-count (length (@ label)))
          (define defn-count (length (@ body)))}
    #:fail-unless (= label-count defn-count)
    (format "found ~a labels and ~a definitions; totals must match"
      label-count
      defn-count)))

(define-pattern $inst-term-body
  #:attributes {[body 1]}
  (pattern
    ({~literal #%plain-lambda} {}
     (~or
       (letrec-syntaxes+values
           {[{_:id ...} _:expr] ...}
           {(~and clause [{lhs:id} rhs:expr]) ...}
         ({~literal #%plain-app} values member:id ...))
       (letrec-values
           {(~and clause [{lhs:id} rhs:expr]) ...}
         ({~literal #%plain-app} values member:id ...))
       (~and ({~literal #%plain-app} values)
         (~bind [(lhs 1) '()] [(rhs 1) '()]))))
    #:do {(define clause-count (length (@ clause)))
          (define member-count (length (@ member)))}
    #:fail-unless (= member-count clause-count)
    (format "found ~a definitions and ~a members; totals must match"
      clause-count
      member-count)
    #:fail-when (for/first {[lhs-id (in-list (@ lhs))]
                            [member-id (in-list (@ member))]
                            #:unless (free-identifier=? lhs-id member-id)}
                  member-id)
    "member name does not match corresponding defined name"
    #:attr [body 1]
    (for/list {[stx (in-list (@ clause))]
               [lhs-id (in-list (@ lhs))]
               [rhs-stx (in-list (@ rhs))]}
      (to-syntax #:source stx
        (list #'define-values (list lhs-id) rhs-stx)))))

(define/provide-pattern $seal-term
  #:attributes {type term}
  (pattern
    ({~literal #%plain-app} {~literal #%seal} ~! type:expr term:expr)))

(define/provide-pattern $gen-term
  #:attributes {[domain 1] [formal 1] body}
  (pattern
    ({~literal #%plain-app} {~literal #%generic} ~!
     ({~literal #%plain-lambda} {} domain:expr ...)
     ({~literal #%plain-lambda} {formal:id ...} body:expr))
    #:do {(define domain-count (length (@ domain)))
          (define formal-count (length (@ formal)))}
    #:fail-unless (= domain-count formal-count)
    (format "found ~a formals and ~a domain types; totals must match"
      formal-count
      domain-count)))

(define/provide-pattern $app-term
  #:attributes {gen [arg 1]}
  (pattern
    ({~literal #%plain-app} {~literal #%instantiate} ~!
     gen:expr
     arg:expr ...)))

(define/provide-pattern $type-term
  #:attributes {type}
  (pattern
    ({~literal #%plain-lambda} {} ({~literal quote} #:type) ~! type:expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declarations

(define/provide-pattern $type-decl
  (pattern
    ({~literal #%plain-lambda} {} ~! ({~literal quote} #:declare)
     name:id
     type:expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types

(define/provide-pattern $stub-type
  #:attributes {arity}
  (pattern
    ({~literal #%plain-lambda} {} ({~literal quote} #:stub) ~! arity:nat)))

(define/provide-pattern $fun-type
  #:attributes {[formal 1] body measure}
  (pattern
    ({~literal #%plain-lambda} {formal:id ...}
     ({~literal quote} #:function) ~!
     (~optional
       (~describe "measure"
         ({~literal #%plain-lambda} {} ({~literal quote} #:measure) ~!
          measure:expr)))
     body:expr)))

(define/provide-pattern $thm-type
  #:attributes {[formal 1] body [rule 1]}
  (pattern
    ({~literal #%plain-lambda} {formal:id ...}
     ({~literal quote} #:theorem) ~!
     (~optional
       (~describe "goals"
         ({~literal #%plain-lambda} {} ({~literal quote} #:rule-classes) ~!
          rule:expr ...)))
     body:expr)))

(define/provide-pattern $inst-type
  #:attributes {[label 1] [body 1]}
  (pattern
    ({~literal #%plain-lambda} {} ({~literal quote} #:instance) ~!
     ({~literal quote} [label:id ...])
     :$inst-type-body)
    #:do {(define label-count (length (@ label)))
          (define defn-count (length (@ body)))}
    #:fail-unless (= label-count defn-count)
    (format "found ~a labels and ~a definitions; totals must match"
      label-count
      defn-count)))

(define-pattern $inst-type-body
  #:attributes {[body 1]}
  (pattern
    ({~literal #%plain-lambda} {name:id ...}
     body:$type-decl ...)
    #:do {(define decl-count (length (@ body)))
          (define name-count (length (@ name)))}
    #:fail-unless (= decl-count name-count)
    (format "found ~a declarations and ~a members; totals must match"
      decl-count
      name-count)
    #:fail-when (for/first {[member-id (in-list (@ name))]
                            [define-id (in-list (@ body.name))]
                            #:unless (free-identifier=? member-id define-id)}
                  define-id)
    "defined name does not match corresponding member name"))

(define/provide-pattern $where-type
  #:attributes {type [label 2] [ref 1]}
  (pattern
    ({~literal #%plain-lambda} {} ({~literal quote} #:where) ~! type:expr
     (~seq ({~literal quote} [label:id ...]) ref:expr) ...)))

(define/provide-pattern $gen-type
  #:attributes {[domain 1] [formal 1] range}
  (pattern
    ({~literal #%plain-lambda} {} ({~literal quote} #:generic) ~!
     ({~literal #%plain-lambda} {formal:id ...} range:expr)
     domain:expr ...)
    #:do {(define domain-count (length (@ domain)))
          (define formal-count (length (@ formal)))}
    #:fail-unless (= domain-count formal-count)
    (format "found ~a formals and ~a domain types; totals must match"
      formal-count
      domain-count)))

(define/provide-pattern $type-type
  #:attributes {type}
  (pattern
    ({~literal #%plain-lambda} {} ({~literal quote} #:type) ~! type:expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; References

(define/provide-pattern $field-ref
  (pattern
    ({~literal #%plain-app} {~literal #%deref}
     comp:expr
     ({~literal quote} field:id))))

(define/provide-pattern $var-ref
  (pattern name:id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expressions

(define/provide-pattern $quote-expr
  (pattern ({~literal quote} ~! value)))

(define/provide-pattern $if-expr
  (pattern (if ~! test:expr then:expr else:expr)))

(define/provide-pattern $app-expr
  (pattern ({~literal #%plain-app} ~! fun:expr arg:expr ...)))

(define/provide-pattern $let-expr
  (pattern (let-values ~! {[{lhs:id} rhs:expr] ...} body:expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rule Classes

(define/provide-pattern $rewrite-rule
  (pattern
    ({~literal #%plain-lambda} {} ({~literal quote} #:rewrite) ~!
     (~optional
       ({~literal #%plain-lambda} {} ({~literal quote} #:corollary) ~!
        corollary:expr)))))

(define/provide-pattern $forward-chaining-rule
  (pattern
    ({~literal #%plain-lambda} {} ({~literal quote} #:forward-chaining) ~!
     (~optional
       ({~literal #%plain-lambda} {} ({~literal quote} #:corollary) ~!
        corollary:expr)))))

(define/provide-pattern $elim-rule
  (pattern
    ({~literal #%plain-lambda} {} ({~literal quote} #:elim) ~!
     (~optional
       ({~literal #%plain-lambda} {} ({~literal quote} #:corollary) ~!
        corollary:expr)))))

(define/provide-pattern $type-prescription-rule
  (pattern
    ({~literal #%plain-lambda} {} ({~literal quote} #:type-prescription) ~!
     (~optional
       ({~literal #%plain-lambda} {} ({~literal quote} #:corollary) ~!
        corollary:expr)))))

(define/provide-pattern $induction-rule
  (pattern
    ({~literal #%plain-lambda} {} ({~literal quote} #:induction) ~!
     (~or
       (~optional
         ({~literal #%plain-lambda} {} ({~literal quote} #:corollary) ~!
          corollary:expr))
       (~once
         ({~literal #%plain-lambda} {} ({~literal quote} #:pattern) ~!
          pattern:expr))
       (~optional
         ({~literal #%plain-lambda} {} ({~literal quote} #:condition) ~!
          condition:expr))
       (~once
         ({~literal #%plain-lambda} {} ({~literal quote} #:scheme) ~!
          scheme:expr)))
     ...)))

(define/provide-pattern $definition-rule
  (pattern
    ({~literal #%plain-lambda} {} ({~literal quote} #:definition) ~!
     (~or
       (~optional
         ({~literal #%plain-lambda} {} ({~literal quote} #:corollary) ~!
          corollary:expr))
       (~optional
         ({~literal #%plain-lambda} {} ({~literal quote} #:clique) ~!
          clique:expr)))
     ...)))

(define/provide-pattern $recursion-clique
  (pattern
    ({~literal #%plain-lambda} {} ({~literal quote} #:clique) ~!
     fun:expr ...)))

(define/provide-pattern $recursion-controllers
  (pattern
    ({~literal #%plain-lambda} {} ({~literal quote} #:controllers) ~!
     ({~literal #%plain-lambda} {}
      fun:expr
      ({~literal quote} control:boolean) ...)
     ...)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hints

(define/provide-pattern $goal
  (pattern
    ({~literal #%plain-lambda} {} ({~literal quote} #:goal) ~!
     name:str
     hint:expr ...)))

(define/provide-pattern $by-hint
  (pattern
    ({~literal #%plain-lambda} {} ({~literal quote} #:by) ~!
     (~optional lemma:expr))))

(define/provide-pattern $use-hint
  (pattern
    ({~literal #%plain-lambda} {} ({~literal quote} #:use) ~!
     lemma:expr ...)))

(define/provide-pattern $in-theory-hint
  (pattern
    ({~literal #%plain-lambda} {} ({~literal quote} #:in-theory) ~!
     theory:expr)))

(define/provide-pattern $induct-hint
  (pattern
    ({~literal #%plain-lambda} {} ({~literal quote} #:induct) ~!
     scheme:expr)))

(define/provide-pattern $lemma
  (pattern ($%plain-app rune:expr arg:expr ...)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theories

(define/provide-pattern $enable
  (pattern
    ({~literal #%plain-lambda} {} ({~literal quote} #:enable) ~!
     rune:expr ...)))

(define/provide-pattern $disable
  (pattern
    ({~literal #%plain-lambda} {} ({~literal quote} #:disable) ~!
     rune:expr ...)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Book Options

(define/provide-pattern $book-dir
  (pattern
    ({~literal #%plain-lambda} {} ({~literal quote} #:dir) ~!
     ({~literal quote} dir:keyword))))

(define/provide-pattern $skip-proofs-okp
  (pattern
    ({~literal #%plain-lambda} {} ({~literal quote} #:skip-proofs-ok) ~!
     ({~literal quote} ok:boolean))))
