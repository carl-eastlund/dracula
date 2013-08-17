#lang mischief

(require
  (for-syntax
    mischief)
  (for-template
    racket/base
    dracula/expansion/runtime))

(provide defn not-defn)

(define-syntax-class defn
  #:literal-sets {kernel-literals}
  (pattern (begin-for-syntax . _))
  (pattern (define-syntaxes . _))
  (pattern (define-values . _))
  (pattern (#%require . _))
  (pattern (#%provide . _)))

(define-syntax-class not-defn
  (pattern (~not _:defn)))

(define-syntax (define/provide-pattern stx)
  (syntax-parse stx
    [(_ name:id . body)
     #'(begin
         (provide name)
         (define-syntax-class name
           #:literal-sets {[kernel-literals #:at name]}
           #:no-delimit-cut
           . body))]))

(define-syntax (define/provide-maker stx)
  (syntax-parse stx
    [(_ (name:id [formal:id /c:expr] ...) body:expr ...+)
     (define/syntax-parse make:id
       (format-id (@ name) "make-~a" (@ name)))
     #'(begin
         (provide
           (contract-out
             [make
              (->* {/c ...} {#:source (or/c syntax? #false)}
                (syntax-class/c name))]))
         (define (make #:source [source #false] formal ...)
           (to-syntax #:source source
             (block body ...))))]))

(define/provide-pattern begin-for-syntax-defn
  (pattern (begin-for-syntax ~! body:expr ...)))

(define/provide-maker (begin-for-syntax-defn [body (listof syntax?)])
  (cons #'begin-for-syntax body))

(define/provide-pattern require-defn
  (pattern (#%require ~! spec:expr ...)))

(define/provide-maker (require-defn [specs (listof syntax?)])
  (cons #'#%require specs))

(define/provide-pattern provide-defn
  (pattern (#%provide ~! spec:expr ...)))

(define/provide-maker (provide-defn [specs (listof syntax?)])
  (cons #'#%provide specs))

(define/provide-pattern syntax-defn
  (pattern (define-syntaxes ~! {name:id ...} body:expr)))

(define/provide-maker (syntax-defn [name (listof identifier?)] [body syntax?])
  (list #'define-syntaxes name body))

(define/provide-pattern description-defn
  #:attributes
    {name
     decls
     defns
     [field-name 1]
     [decl-name 1]
     [defn-name 1]
     [body 1]}
  (pattern
    (define-values {name:id field-name:id ...}
      (#%plain-app {~literal make-description} ~!
        (quote decls:nat)
        (quote defns:nat)
        impl:impl-expr))
    #:do {(define decl-expected (length (@ impl.implicit-name)))
          (define defn-expected (length (@ impl.explicit-name)))
          (define expected (+ decl-expected defn-expected))
          (define actual (length (@ field-name)))}
    #:fail-unless (= actual expected)
    (string-append
      (format "expected ~a field names " expected)
      (format "(based on ~a declared names + ~a defined names), "
        defn-expected
        decl-expected)
      (format "but found ~a field names" actual))
    #:attr [decl-name 1] (@ impl.implicit-name)
    #:attr [defn-name 1] (@ impl.explicit-name)
    #:attr [body 1] (@ impl.body)))

(define/provide-maker (description-defn
                        [name identifier?]
                        [decls (syntax-class/c nat)]
                        [defns (syntax-class/c nat)]
                        [fields (listof identifier?)]
                        [decl-ids (listof identifier?)]
                        [defn-ids (listof identifier?)]
                        [body (listof syntax?)])
  (list #'define-values (list* name fields)
    (list #'#%plain-app #'make-description
      (list #'quote decls)
      (list #'quote defns)
      (make-impl-expr decl-ids defn-ids body))))

(define/provide-pattern component-defn
  (pattern (define-values {name:id} body:component-expr)))

(define/provide-maker (component-defn
                        [name identifier?]
                        [body syntax?])
  (list #'define-values (list name) body))

(define/provide-pattern generic-defn
  (pattern
    (define-values {name:id}
      (#%plain-app {~literal make-generic} ~! domain:expr
        (#%plain-lambda {formal:id}
          body:component-expr)))))

(define/provide-maker (generic-defn
                        [name identifier?]
                        [domain syntax?]
                        [formal identifier?]
                        [body syntax?])
  (list #'define-values (list name)
    (list #'#%plain-app #'make-generic domain
      (list #'#%plain-lambda (list formal)
        body))))

(define/provide-pattern component-expr
  #:attributes {desc [from-desc 1] [from-comp 1] [body 1]}
  (pattern (#%plain-app {~literal make-component} ~! desc:expr impl:impl-expr)
    #:attr [from-desc 1] (@ impl.implicit-name)
    #:attr [from-comp 1] (@ impl.explicit-name)
    #:attr [body 1] (@ impl.body)))

(define/provide-maker (component-expr
                        [desc syntax?]
                        [from-desc (listof identifier?)]
                        [from-comp (listof identifier?)]
                        [body (listof syntax?)])
  (list #'#%plain-app #'make-component desc
    (make-impl-expr from-desc from-comp body)))

(define/provide-pattern instance-defn
  (pattern
    (define-values {name:id}
      (#%plain-app {~literal apply-generic} ~! gen:expr arg:expr))))

(define/provide-maker (instance-defn
                        [name identifier?]
                        [gen syntax?]
                        [arg syntax?])
  (list #'define-values (list name)
    (list #'#%plain-app #'apply-generic gen arg)))

(define/provide-pattern component-decl
  (pattern
    (define-values {}
      (#%plain-app {~literal abstract-component} ~! name:id desc:expr))))

(define/provide-maker (component-decl [name identifier?] [desc syntax?])
  (list #'define-values (list)
    (list #'#%plain-app #'abstract-component name desc)))

(define/provide-pattern generic-decl
  (pattern
    (define-values {}
      (#%plain-app {~literal abstract-generic} ~! name:id domain:expr
        (#%plain-lambda {formal:id} range:expr)))))

(define/provide-maker (generic-decl
                        [name identifier?]
                        [domain syntax?]
                        [formal identifier?]
                        [range syntax?])
  (list #'define-values (list)
    (list #'#%plain-lambda #'abstract-generic name domain
      (list #'#%plain-lambda (list formal) range))))

(define/provide-pattern refine-expr
  (pattern
    (#%plain-app {~literal refine-description} ~!
      base:expr
      (~seq field:expr defn:expr) ...)))

(define/provide-maker (refine-expr
                        [base syntax?]
                        [fields (listof syntax?)]
                        [defns (listof syntax?)])
  (list* #'#%plain-app #'refine-description base
    (for/append {[field (in-list fields)]
                 [defn (in-list defns)]}
      (list field defn))))

(define-syntax-class impl-expr
  #:attributes {[implicit-name 1] [explicit-name 1] [body 1]}
  #:literal-sets {kernel-literals}
  #:no-delimit-cut
  (pattern
    (#%plain-lambda {implicit-name:id ...}
      (letrec-values
          {(~and clause [(lhs:id ...) rhs:expr]) ...}
        (#%plain-app {~literal values} explicit-name:id ...)))
    #:attr [body 1]
    (clauses->defns #'define-values
      (@ clause)
      (@ lhs)
      (@ rhs)))
  (pattern
    (#%plain-lambda {implicit-name:id ...}
      (#%plain-app {~literal values} explicit-name:id ...))
    #:attr [body 1] '()))

(define (clauses->defns defn-id clause-stxs lhs-stxss rhs-stxs)
  (for/list {[clause-stx (in-list clause-stxs)]
             [lhs-stxs (in-list lhs-stxss)]
             [rhs-stx (in-list rhs-stxs)]}
    (to-syntax #:source clause-stx #:stx defn-id
      (list (syntax-e defn-id) lhs-stxs rhs-stx))))

(define (make-impl-expr implicit explicit body)
  (define clauses
    (for/list {[stx (in-list body)]}
      (syntax-parse stx
        #:literal-sets {kernel-literals}
        [(define-values {x ...} e) (list (@ x) (@ e))])))
  (list #'#%plain-lambda implicit
    (list #'letrec-values clauses
      (list* #'#%plain-app #'values explicit))))

(define/provide-pattern primitive-defn
  (pattern
    (define-values {}
      (#%plain-app {~literal register-primitive} ~!
        prim-name:id
        (quote arity:nat)
        (quote sym-package:id)
        (quote sym-name:id)))))

(define/provide-maker (primitive-defn
                        [name identifier?]
                        [arity (syntax-class/c nat)]
                        [pkg identifier?]
                        [sym identifier?])
  (list #'define-values (list)
    (list #'#%plain-app #'register-primitive
      name
      (list #'quote arity)
      (list #'quote pkg)
      (list #'quote sym))))

(define/provide-pattern function-defn
  (pattern
    (define-values {name:id}
      (#%plain-lambda (formal:id ...)
        (quote #:function)
        ~!
        (~optional (#%plain-lambda () ~! measure:expr))
        body:expr))))

(define/provide-maker (function-defn
                        [name identifier?]
                        [formals (listof identifier?)]
                        [measure (or/c syntax? #false)]
                        [body syntax?])
  (list #'define-values (list name)
    (list/optional #'#%plain-lambda formals
      (list #'quote '#:function)
      (and measure
        (list #'#%plain-lambda (list) measure))
      body)))

(define/provide-pattern function-decl
  (pattern
    (define-values {}
      (#%plain-app {~literal abstract-function} ~!
        name:id
        (quote arity:nat)))))

(define/provide-maker (function-decl
                        [name identifier?]
                        [arity (syntax-class/c nat)])
  (list #'define-values (list)
    (list #'#%plain-app #'abstract-function
      name
      (list #'quote arity))))

(define/provide-pattern theorem-defn
  (pattern
    (define-values {name:id}
      (#%plain-lambda (formal:id ...)
        (quote #:theorem)
        ~!
        (~or
          (~optional
            (#%plain-lambda ()
              (#%plain-app {~literal make-rule-classes} ~!
                rule-class:expr ...)))
          (~optional
            (#%plain-lambda ()
              (#%plain-app {~literal make-hints} ~!
                goal:expr ...))))
        ...
        body:expr))))

(define/provide-maker (theorem-defn
                        [name identifier?]
                        [formals (listof identifier?)]
                        [rule-classes (or/c #false (listof syntax?))]
                        [hints (or/c #false (listof syntax?))]
                        [body syntax?])
  (list #'define-values (list name)
    (list/optional #'#%plain-lambda formals
      (list #'quote '#:theorem)
      (and rule-classes
        (list #'#%plain-lambda '()
          (list* #'#%plain-app #'make-rule-classes
            rule-classes)))
      (and hints
        (list #'#%plain-lambda '()
          (list* '#%plain-app #'make-hints
            hints)))
      body)))

(define/provide-pattern theorem-decl
  (pattern
    (define-values {}
      (#%plain-app {~literal abstract-theorem} ~!
        name:id
        (#%plain-lambda (formal:id ...)
          (~or
            (~optional
              (#%plain-lambda ()
                (#%plain-app {~literal make-rule-classes} ~!
                  rule-class:expr ...))))
          ...
          body:expr)))))

(define/provide-maker (theorem-decl
                        [name identifier?]
                        [formals (listof identifier?)]
                        [rule-classes (or/c #false (listof syntax?))]
                        [body syntax?])
  (list #'define-values '()
    (list #'#%plain-app #'abstract-theorem name
      (list/optional #'#%plain-lambda formals
        (and rule-classes
          (list #'#%plain-lambda '()
            (list* #'#%plain-app #'make-rule-classes
              rule-classes)))
        body))))

(define/provide-pattern in-theory-defn
  (pattern
    (define-values {}
      (#%plain-app {~literal register-theory} ~! theory:expr))))

(define/provide-maker (in-theory-defn [theory syntax?])
  (list #'define-values '()
    (list #'#%plain-app #'register-theory theory)))

(define/provide-pattern book-defn
  (pattern
    (define-values {}
      (#%plain-app {~literal register-book} ~!
        (quote path:str)
        option:expr
        ...))))

(define/provide-maker (book-defn
                        [path (syntax-class/c str)]
                        [options (listof syntax?)])
  (list '#'define-values '()
    (list* #'#%plain-app #'register-book
      (list #'quote path)
      options)))

(define/provide-pattern book-dir-expr
  (pattern (#%plain-app {~literal book-dir} ~! (quote dir:keyword))))
(define/provide-maker (book-dir-expr [dir (syntax-class/c keyword)])
  (list #'#%plain-app #'book-dir (list #'quote dir)))

(define/provide-pattern rule/rewrite-expr
  (pattern (#%plain-app {~literal rule/rewrite} ~! corollary:expr)))
(define/provide-maker (rule/rewrite-expr [corollary syntax?])
  (list #'#%plain-app #'rule/rewrite corollary))

(define/provide-pattern rule/forward-chaining-expr
  (pattern (#%plain-app {~literal rule/forward-chaining} ~! corollary:expr)))
(define/provide-maker (rule/forward-chaining-expr
                        [corollary syntax?])
  (list #'#%plain-app #'rule/forward-chaining corollary))

(define/provide-pattern rule/elim-expr
  (pattern (#%plain-app {~literal rule/elim} ~! corollary:expr)))
(define/provide-maker (rule/elim-expr [corollary syntax?])
  (list #'#%plain-app #'rule/elim corollary))

(define/provide-pattern rule/type-prescription-expr
  (pattern (#%plain-app {~literal rule/type-prescription} ~! corollary:expr)))
(define/provide-maker (rule/type-prescription-expr
                        [corollary syntax?])
  (list #'#%plain-app #'rule/type-prescription corollary))

(define/provide-pattern rule/definition-expr
  (pattern
    (#%plain-app {~literal rule/definition} ~!
      corollary:expr
      clique:expr)))

(define/provide-maker (rule/definition-expr
                        [corollary syntax?]
                        [clique syntax?])
  (list #'#%plain-app #'rule/definition
    corollary
    clique))

(define/provide-pattern option/clique-expr
  (pattern (#%plain-app {~literal option/clique} ~! fun:expr ...)))

(define/provide-maker (option/clique-expr [funs (listof syntax?)])
  (list* #'#%plain-app #'option/clique funs))

(define/provide-pattern option/controllers-expr
  (pattern
    (#%plain-app {~literal option/controllers} ~!
      (#%plain-app fun:expr (quote control:boolean) ...)
      ...)))

(define/provide-maker (option/controllers-expr
                        [funs (listof syntax?)]
                        [controlss (listof (listof (syntax-class/c boolean)))])
  (list* #'#%plain-app #'option/controllers
    (for/list {[fun (in-list funs)]
               [controls (in-list controlss)]}
      (list* #'#%plain-app fun
        (for/list {[control (in-list controls)]}
          (list #'quote control))))))

(define/provide-pattern rule/induction-expr
  (pattern
    (#%plain-app {~literal rule/induction} ~!
      corollary:expr
      pattern:expr
      condition:expr
      scheme:expr)))

(define/provide-maker (rule/induction-expr
                        [corollary (or/c #false syntax?)]
                        [pattern syntax?]
                        [condition (or/c #false syntax?)]
                        [scheme syntax?])
  (list #'#%plain-app #'rule/induction corollary pattern condition scheme))

(define/provide-pattern optional/some-expr
  (pattern (#%plain-app {~literal optional/some} ~! arg:expr)))
(define/provide-maker (optional/some-expr [arg syntax?])
  (list/optional #'#%plain-app #'optional/some arg))

(define/provide-pattern optional/none-expr
  (pattern (#%plain-app {~literal optional/none} ~!)))
(define/provide-maker (optional/none-expr)
  (list/optional #'#%plain-app #'optional/none))

(define/provide-pattern goal-expr
  (pattern
    (#%plain-app {~literal make-goal} ~!
      (quote name:str)
      hint:expr
      ...)))

(define/provide-maker (goal-expr [name identifier?] [hints (listof syntax?)])
  (list* #'#%plain-app #'make-goal
    (list #'quote name)
    hints))

(define/provide-pattern hint/by-expr
  (pattern (#%plain-app {~literal hint/by} ~! (~optional inst:expr))))

(define/provide-maker (hint/by-expr [inst (or/c #false syntax?)])
  (list/optional #'#%plain-app #'hint/by inst))

(define/provide-pattern hint/use-expr
  (pattern (#%plain-app {~literal hint/use} ~! inst:expr ...)))

(define/provide-maker (hint/use-expr [insts (listof syntax?)])
  (list* #'#%plain-app #'hint/use insts))

(define/provide-pattern hint/in-theory-expr
  (pattern (#%plain-app {~literal hint/in-theory} ~! theory:expr)))

(define/provide-maker (hint/in-theory-expr [theory syntax?])
  (list #'#%plain-app #'hint/in-theory theory))

(define/provide-pattern hint/induct-expr
  (pattern (#%plain-app {~literal hint/induct} ~! scheme:expr)))

(define/provide-maker (hint/induct-expr [scheme syntax?])
  (list #'#%plain-app #'hint/induct scheme))

(define/provide-pattern disable-expr
  (pattern (#%plain-app {~literal disable} ~! rune:expr ...)))

(define/provide-maker (disable-expr [runes (listof syntax?)])
  (list* #'#%plain-app #'disable runes))

(define/provide-pattern enable-expr
  (pattern (#%plain-app {~literal enable} ~! rune:expr ...)))

(define/provide-maker (enable-expr [runes (listof syntax?)])
  (list* #'#%plain-app #'enable runes))

(define/provide-pattern deref-expr
  (pattern (#%plain-app {~literal component-ref} ~! comp:expr field:expr)))

(define/provide-maker (deref-expr [comp syntax?] [field syntax?])
  (list #'#%plain-app #'component-ref comp field))

(define/provide-pattern quote-expr (pattern (quote ~! value)))
(define/provide-maker (quote-expr [value syntax?]) (list #'quote value))

(define/provide-pattern if-expr
  (pattern (if ~! test:expr then:expr else:expr)))

(define/provide-maker (if-expr [test syntax?] [then syntax?] [else syntax?])
  (list #'if test then else))

(define/provide-pattern let-expr
  (pattern (let-values ~! {[(lhs:id) rhs:expr] ...} body:expr)))

(define/provide-maker (let-expr
                        [lhs (listof identifier?)]
                        [rhs (listof syntax?)]
                        [body syntax?])
  (list #'let-values
    (for/list {[name (in-list lhs)] [value (in-list rhs)]}
      (list (list name) value))
    body))

(define/provide-pattern app-expr
  (pattern (#%plain-app ~! fun:expr arg:expr ...)))

(define/provide-maker (app-expr [fun syntax?] [args (listof syntax?)])
  (list* #'#%plain-app fun args))

(define/provide-pattern var-expr (pattern name:id))
(define/provide-maker (var-expr [name identifier?]) name)
