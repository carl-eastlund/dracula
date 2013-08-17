#lang mischief

(provide parse-program)

(require
  (for-syntax
    mischief)
  mischief/kernel-syntax
  dracula/model/data
  dracula/model/subst
  dracula/model/names
  unstable/list
  (for-template
    racket/base
    dracula/expansion/runtime))

(define (parse-program stxs)
  (program
    (sort-defns
      (filter-map parse-definition stxs))))

(define (parse-environment stxs)
  (environment
    (sort-decls
      (filter-map parse-declaration stxs))))

(define-syntax-class label-id
  (pattern name:id
    #:fail-unless (symbol-interned? (syntax-e #'name))
    (format "invalid label ~a" (label->string (syntax-e #'name)))))

(define-syntax-class a-for-syntax-block
  #:description "a for-syntax block"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern (begin-for-syntax ~! . _)))

(define-syntax-class a-require-form
  #:description "a require form"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern (#%require ~! . _)))

(define-syntax-class a-provide-form
  #:description "a provide form"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern (#%provide ~! . _)))

(define-syntax-class a-syntax-definition
  #:description "a syntax definition"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern (define-syntaxes ~! . _)))

(define-syntax-class a-module-definition
  #:description "a module definition"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern ({~literal module} ~! . _)))

(define-syntax-class a-submodule-definition
  #:description "a submodule definition"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern ({~literal module*} ~! . _)))

(define-syntax-class an-anonymous-definition
  #:description "an anonymous definition"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern
    (define-values {} ~!
      (begin0 (#%plain-app {~literal values}) a:expr))))

(define-syntax-class a-value-definition
  #:description "a value definition"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern (define-values ~! {x:id} t:expr)))

(define-syntax-class a-definition
  #:description "a definition"
  #:attributes {}
  #:literal-sets {kernel-literals}
  (pattern :a-for-syntax-block)
  (pattern :a-require-form)
  (pattern :a-provide-form)
  (pattern :a-syntax-definition)
  (pattern :a-module-definition)
  (pattern :a-submodule-definition)
  (pattern :an-anonymous-definition)
  (pattern :a-value-definition))

(define (parse-definition stx)
  (syntax-parse stx
    #:literal-sets {kernel-literals}
    [:a-for-syntax-block #false]
    [:a-require-form #false]
    [:a-provide-form #false]
    [:a-syntax-definition #false]
    [:a-module-definition #false]
    [:a-submodule-definition #false]
    [:an-anonymous-definition (parse-anonymous-definition-body (@ a))]
    [:a-value-definition (defn (src stx) (var (@ x)) (parse-term (@ t)))]
    [:a-declaration
     #:fail-when #true
     "expected a definition, but found a declaration"
     impossible]
    [e:an-expression (parse-expression (@ e))]))

(define-syntax-class an-assertion
  #:description "an assertion"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern
    (#%plain-app {~literal #%assert} ~! e:expr
      (~optional (quote-syntax original)))))

(define-syntax-class a-validation
  #:description "a validation"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern
    (#%plain-app {~literal #%validate} ~! fun:expr
      (quote {[input ...] ...})
      (quote {output ...})
      (~optional (quote-syntax original)))))

(define-syntax-class an-in-theory-specification
  #:description "an in-theory specification"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern (#%plain-lambda {} '#:in-theory ~! t:expr)))

(define-syntax-class a-primitive-registration
  #:description "a primitive registration"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern
    (#%plain-lambda {} '#:primitive ~!
      name:id 'arity:nat 'package:id 'symbol:id)))

(define-syntax-class a-book-import
  #:description "a book import"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern
    (#%plain-lambda {} '#:include-book ~! 'path:str opt:expr ...)))

(define (parse-anonymous-definition-body stx)
  (syntax-parse stx
    #:literal-sets {kernel-literals}

    [:an-assertion
     (assert-event
       (src (or (@ original) stx))
       (parse-expression (@ e)))]

    [:a-validation
     (validate
       (src (or (@ original) stx))
       (parse-addr (@ fun))
       (to-datum (@ input))
       (to-datum (@ output)))]

    [:an-in-theory-specification
     (in-theory stx (parse-theory (@ t)))]

    [:a-primitive-registration
     (primitive (src stx)
       (var (@ name))
       (syntax-e (@ arity))
       (syntax-e (@ package))
       (syntax-e (@ symbol)))]

    [:a-book-import
     (include-book (src stx)
       (syntax-e (@ path))
       (map parse-book-option (@ opt)))]))

(define-syntax-class a-value-declaration
  #:description "a value declaration"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern (define-values {name:id} ~! type:expr)))

(define-syntax-class a-declaration
  #:description "a declaration"
  #:no-delimit-cut
  #:attributes {}
  #:literal-sets {kernel-literals}
  (pattern _:a-value-declaration))

(define (parse-declaration stx)
  (syntax-parse stx
    #:literal-sets {kernel-literals}

    [:a-value-declaration
     (decl (src stx) (var (@ name)) (parse-type (@ type)))]
    [:a-definition
     #:fail-when #true
     "expected a declaration, but found a definition"
     impossible]
    [:an-expression
     #:fail-when #true
     "expected a declaration, but found an expression"
     impossible]))

(define-syntax-class a-function
  #:description "a function"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern (#%plain-lambda {x:id ...} '#:function ~! m:expr g:expr e:expr)))

(define-syntax-class a-theorem
  #:description "a theorem"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern (#%plain-lambda {x:id ...} '#:theorem ~! r:expr g:expr e:expr)))

(define-syntax-class an-instance
  #:description "an instance"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern (#%plain-app {~literal #%instance} ~! body:expr)))

(define-syntax-class a-generic
  #:description "a generic"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern
    (#%plain-lambda {formal:id ...}
      (begin0 ~! body:expr domain:expr ...))))

(define-syntax-class an-instantiated-generic
  #:description "an instantiated generic"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern (#%plain-app {~literal #%instantiate} ~! gen:expr arg:expr ...)))

(define-syntax-class a-sealed-term
  #:description "a sealed term"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern (#%plain-app {~literal #%seal} ~! type:expr term:expr)))

(define-syntax-class a-reified-type
  #:description "a reified type"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern (#%plain-lambda {} '#:type ~! type:expr)))

(define-syntax-class a-term
  #:description "a term"
  #:attributes {}
  #:literal-sets {kernel-literals}
  (pattern :a-function)
  (pattern :a-theorem)
  (pattern :an-instance)
  (pattern :a-generic)
  (pattern :an-instantiated-generic)
  (pattern :a-sealed-term)
  (pattern :a-reified-type))

(define (parse-term stx)
  (syntax-parse stx
    #:literal-sets {kernel-literals}

    [:a-function
     (fun-term (src stx)
       (map var (@ x))
       (parse-expression (@ e))
       (parse-measure (@ m))
       (parse-goals (@ g)))]

    [:a-theorem
     (thm-term (src stx)
       (map var (@ x))
       (parse-expression (@ e))
       (parse-rules (@ r))
       (parse-goals (@ g)))]

    [:an-instance
     (define-values {labels names defns}
       (parse-instance-body (@ body)))
     (inst-term (src stx)
       (map syntax-e labels)
       (map var names)
       (parse-program defns))]

    [:a-generic
     #:do {(define n-formals (length (@ formal)))
           (define n-domains (length (@ domain)))}
     #:fail-unless (= n-formals n-domains)
     (format "argument mismatch: generic has ~a and ~a"
       (count->phrase n-formals "formal argument")
       (count->phrase n-domains "domain type"))
     (gen-term (src stx)
       (map var (@ formal))
       (map parse-type (@ domain))
       (parse-term (@ body)))]

    [:an-instantiated-generic
     (app-term (src stx)
       (parse-term (@ gen))
       (map parse-addr (@ arg)))]

    [:a-sealed-term
     (seal-term (src stx)
       (parse-type (@ type))
       (parse-term (@ term)))]

    [:a-reified-type
     (type-term (src stx)
       (parse-type (@ type)))]

    [a:an-addr
     (parse-addr (@ a))]

    [_:a-type
     #:fail-when #true
     "expected a term, but found a type"
     impossible]))

(define-syntax-class a-stub-type
  #:description "a stub type"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern (#%plain-lambda {} '#:stub 'arity:nat)))

(define-syntax-class a-function-type
  #:description "a function type"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern (#%plain-lambda {x:id ...} '#:function ~! m:expr e:expr)))

(define-syntax-class a-theorem-type
  #:description "a theorem type"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern (#%plain-lambda {x:id ...} '#:theorem ~! r:expr e:expr)))

(define-syntax-class an-instance-type
  #:description "an instance type"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern (#%plain-lambda {} '#:instance ~! body:expr)))

(define-syntax-class a-generic-type
  #:description "a generic type"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern
    (#%plain-lambda {formal:id ...}
      (begin0 ~! body:expr domain:expr ...))))

(define-syntax-class a-refined-type
  #:description "a refined type"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern
    (#%plain-lambda {} '#:refine ~!
      base:expr
      '[label:label-id ...]
      addr:expr)))

(define-syntax-class a-nominal-type
  #:description "a nominal type"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern (#%plain-lambda {} '#:addr ~! addr:expr)))

(define-syntax-class a-type-type
  #:description "the type of a type"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern (#%plain-lambda {} '#:type ~! type:expr)))

(define-syntax-class a-type
  #:description "a type"
  #:attributes {}
  #:literal-sets {kernel-literals}
  (pattern :a-stub-type)
  (pattern :a-function-type)
  (pattern :a-theorem-type)
  (pattern :an-instance-type)
  (pattern :a-generic-type)
  (pattern :a-refined-type)
  (pattern :a-nominal-type)
  (pattern :a-type-type))

(define (parse-type stx)
  (syntax-parse stx
    #:literal-sets {kernel-literals}

    [:a-stub-type
     (stub-type (src stx)
       (syntax-e (@ arity)))]

    [:a-function-type
     (fun-type (src stx)
       (map var (@ x))
       (parse-expression (@ e))
       (parse-measure (@ m)))]

    [:a-theorem-type
     (thm-type (src stx)
       (map var (@ x))
       (parse-expression (@ e))
       (parse-rules (@ r)))]

    [:an-instance-type
     (define-values {labels names decls}
       (parse-instance-body (@ body)))
     (inst-type (src stx)
       (map syntax-e labels)
       (map var names)
       (parse-environment decls))]

    [:a-generic-type
     #:do {(define n-formals (length (@ formal)))
           (define n-domains (length (@ domain)))}
     #:fail-unless (= n-formals n-domains)
     (format "argument mismatch: generic type has ~a and ~a"
       (count->phrase n-formals "formal argument")
       (count->phrase n-domains "domain type"))
     (gen-type (src stx)
       (map var (@ formal))
       (map parse-type (@ domain))
       (parse-type (@ body)))]

    [:a-refined-type
     (refine-type (src stx)
       (parse-type (@ base))
       (map syntax-e (@ label))
       (parse-addr (@ addr)))]

    [:a-nominal-type
     (addr-type (src stx)
       (parse-addr (@ addr)))]

    [:a-type-type
     (type-type (src stx)
       (parse-type (@ type)))]

    [a:an-addr
     (parse-addr (@ a))]

    [_:a-term
     #:fail-when #true
     "expected a type, but found a term"
     impossible]))

(define-syntax-class an-instance-body-with-syntax-and-value-bindings
  #:description "an instance body with syntax and value bindings"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (letrec-syntaxes+values ~!
        {(~and stx-line [{stx-name:id ...} stx-body:expr]) ...}
        {(~and val-line [{val-name:id ...} val-body:expr]) ...}
      (#%plain-app {~literal values}
        (~seq 'label:label-id name:id) ...))))

(define-syntax-class an-instance-body-with-only-value-bindings
  #:description "an instance body with only value bindings"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (letrec-values ~!
        {(~and val-line [{val-name:id ...} val-body:expr]) ...}
      (#%plain-app {~literal values}
        (~seq 'label:label-id name:id) ...))))

(define-syntax-class an-instance-body-with-no-bindings
  #:description "an instance body with no bindings"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (#%plain-app ~! {~literal values}
      (~seq 'label:label-id name:id) ...)))

(define-syntax-class instance-body
  #:description "an instance body"
  #:attributes {[stx-line 1] [stx-name 2] [stx-body 1]
                [val-line 1] [val-name 2] [val-body 1]
                [label 1] [name 1]}
  #:literal-sets {kernel-literals}

  (pattern
    :an-instance-body-with-syntax-and-value-bindings)

  (pattern
    :an-instance-body-with-only-value-bindings
    #:attr [stx-line 1] '()
    #:attr [stx-name 2] '()
    #:attr [stx-body 1] '())

  (pattern
    :an-instance-body-with-no-bindings
    #:attr [stx-line 1] '()
    #:attr [stx-name 2] '()
    #:attr [stx-body 1] '()
    #:attr [val-line 1] '()
    #:attr [val-name 2] '()
    #:attr [val-body 1] '()))

(define (parse-instance-body stx)
  (syntax-parse stx
    #:literal-sets {kernel-literals}
    [(#%plain-lambda {} :instance-body)
     #:fail-when
     (check-duplicate (@ label) #:key syntax-e)
     "duplicate label"
     #:fail-when
     (check-duplicate-identifier (@ name))
     "instance definition exported with two different labels"
     #:fail-when
     (check-missing-identifier (@ name) (append* (@ val-name)))
     "instance exports externally-defined name"
     #:fail-when
     (check-missing-identifier (append* (@ val-name)) (@ name))
     "instance fails to export definition"
     (define defns
       (append
         (build-defns #'define-syntaxes (@ stx-line) (@ stx-name) (@ stx-body))
         (build-defns #'define-values (@ val-line) (@ val-name) (@ val-body))))
     (values (@ label) (@ name) defns)]))

(define-syntax-class a-quote-expression
  #:description "a quote expression"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern (quote ~! v)))

(define-syntax-class an-if-expression
  #:description "an if expression"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (if ~! test:expr then:expr else:expr)))

(define-syntax-class a-let-expression
  #:description "a let expression"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (let-values ~! {[(x:id) e:expr] ...} e0:expr)))

(define-syntax-class a-function-application
  #:description "a function application"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (#%plain-app ~! f:expr e:expr ...)))

(define-syntax-class an-expression
  #:description "an expression"
  #:attributes {}
  #:literal-sets {kernel-literals}
  (pattern _:a-quote-expression)
  (pattern _:an-if-expression)
  (pattern _:a-let-expression)
  (pattern _:a-function-application)
  (pattern _:id))

(define (parse-expression stx)
  (syntax-parse stx
    #:literal-sets {kernel-literals}

    [:a-quote-expression
     (qu-expr (src stx)
       (to-datum (@ v)))]

    [:an-if-expression
     (if-expr (src stx)
       (parse-expression (@ test))
       (parse-expression (@ then))
       (parse-expression (@ else)))]

    [:a-let-expression
     (let-expr (src stx)
       (map var (@ x))
       (map parse-expression (@ e))
       (parse-expression (@ e0)))]

    [:a-function-application
     (app-expr (src stx)
       (parse-addr (@ f))
       (map parse-expression (@ e)))]

    [x:id
     (var (@ x))]))

(define-syntax-class a-member-dereference
  #:description "a member dereference"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (#%plain-app {~literal #%deref} comp:expr (quote label:label-id))))

(define-syntax-class an-addr
  #:description "an address"
  #:no-delimit-cut
  #:attributes {}
  #:literal-sets {kernel-literals}
  (pattern _:a-member-dereference)
  (pattern _:id))

(define (parse-addr stx)
  (syntax-parse stx
    #:literal-sets {kernel-literals}

    [:a-member-dereference
     (deref (src stx) (parse-addr (@ comp)) (syntax-e (@ label)))]

    [x:id (var (@ x))]))

(define-syntax-class an-annotated-proof-goal
  #:description "an annotated proof goal"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (#%plain-lambda {} '#:goal ~! 's:str h:expr ...)))

(define (parse-goal stx)
  (syntax-parse stx
    #:literal-sets {kernel-literals}

    [:an-annotated-proof-goal
     (goal (src stx)
       (syntax-e (@ s))
       (map parse-hint (@ h)))]))

(define-syntax-class a-:BY-hint
  #:description "a :BY hint"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (#%plain-lambda {} '#:by ~! (~optional l:expr))))

(define-syntax-class a-:USE-hint
  #:description "a :USE hint"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (#%plain-lambda {} '#:use ~! l:expr ...)))

(define-syntax-class a-:IN-THEORY-hint
  #:description "a :IN THEORY hint"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (#%plain-lambda {} '#:in-theory ~! t:expr)))

(define-syntax-class a-:INDUCT-hint
  #:description "a :INDUCT hint"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (#%plain-lambda {} '#:induct ~! e:expr)))

(define (parse-hint stx)
  (syntax-parse stx
    #:literal-sets {kernel-literals}

    [:a-:BY-hint
     (by-hint (src stx)
       (and (@ l) (parse-lemma (@ l))))]

    [:a-:USE-hint
     (use-hint (src stx)
       (map parse-lemma (@ l)))]

    [:a-:IN-THEORY-hint
     (in-theory-hint (src stx)
       (parse-theory (@ t)))]

    [:a-:INDUCT-hint
     (induct-hint (src stx)
       (parse-expression (@ e)))]))

(define-syntax-class an-instantiated-lemma
  #:description "an instantiated lemma"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (#%plain-app ~! r:expr e:expr ...)))

(define (parse-lemma stx)
  (syntax-parse stx
    #:literal-sets {kernel-literals}

    [:an-instantiated-lemma
     (lemma (src stx)
       (parse-addr (@ r))
       (map parse-expression (@ e)))]))

(define-syntax-class an-ENABLE-theory
  #:description "an ENABLE theory"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (#%plain-lambda {} '#:enable ~! r:expr ...)))

(define-syntax-class a-DISABLE-theory
  #:description "a DISABLE theory"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (#%plain-lambda {} '#:disable ~! r:expr ...)))

(define (parse-theory stx)
  (syntax-parse stx
    #:literal-sets {kernel-literals}

    [:an-ENABLE-theory
     (enable (src stx)
       (map parse-addr (@ r)))]

    [:a-DISABLE-theory
     (disable (src stx)
       (map parse-addr (@ r)))]))

(define-syntax-class a-:REWRITE-rule
  #:description "a :REWRITE rule"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (#%plain-lambda {} '#:rewrite ~! cor:expr)))

(define-syntax-class a-:FORWARD-CHAINING-rule
  #:description "a :FORWARD-CHAINING rule"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (#%plain-lambda {} '#:forward-chaining ~! cor:expr)))

(define-syntax-class a-:ELIM-rule
  #:description "a :ELIM rule"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (#%plain-lambda {} '#:elim ~! cor:expr)))

(define-syntax-class a-:TYPE-PRESCRIPTION-rule
  #:description "a :TYPE-PRESCRIPTION rule"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (#%plain-lambda {} '#:type-prescription ~! cor:expr)))

(define-syntax-class a-:LINEAR-rule
  #:description "a :LINEAR rule"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (#%plain-lambda {} '#:linear ~! cor:expr)))

(define-syntax-class a-:DEFINITION-rule
  #:description "a :DEFINITION rule"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (#%plain-lambda {} '#:definition ~! cor:expr cli:expr)))

(define-syntax-class a-:INDUCTION-rule
  #:description "a :INDUCTION rule"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (#%plain-lambda {} '#:induction ~! cor:expr pat:expr con:expr sch:expr)))

(define (parse-rule stx)
  (syntax-parse stx
    #:literal-sets {kernel-literals}

    [:a-:REWRITE-rule
     (rewrite (src stx)
       (parse-corollary (@ cor)))]

    [:a-:FORWARD-CHAINING-rule
     (forward-chaining (src stx)
       (parse-corollary (@ cor)))]

    [:a-:ELIM-rule
     (elim (src stx)
       (parse-corollary (@ cor)))]

    [:a-:TYPE-PRESCRIPTION-rule
     (type-prescription (src stx)
       (parse-corollary (@ cor)))]

    [:a-:LINEAR-rule
     (linear (src stx)
       (parse-corollary (@ cor)))]

    [:a-:DEFINITION-rule
     (definition (src stx)
       (parse-corollary (@ cor))
       (parse-clique (@ cli)))]

    [:a-:INDUCTION-rule
     (induction (src stx)
       (parse-corollary (@ cor))
       (parse-pattern (@ pat))
       (parse-condition (@ con))
       (parse-scheme (@ sch)))]))

(define-syntax-class no-corollary
  #:description "no corollary"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern '#:no-corollary))

(define-syntax-class an-explicit-corollary
  #:description "an explicit corollary"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (#%plain-lambda {} '#:corollary e:expr)))

(define (parse-corollary stx)
  (syntax-parse stx
    #:literal-sets {kernel-literals}
    [:no-corollary #false]
    [:an-explicit-corollary
     (parse-expression (@ e))]))

(define-syntax-class no-induction-condition
  #:description "no induction condition"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern '#:no-condition))

(define-syntax-class an-explicit-induction-condition
  #:description "an explicit induction condition"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (#%plain-lambda {} '#:condition e:expr)))

(define (parse-condition stx)
  (syntax-parse stx
    #:literal-sets {kernel-literals}
    [:no-induction-condition #false]
    [:an-explicit-induction-condition
     (parse-expression (@ e))]))

(define-syntax-class an-induction-pattern
  #:description "an induction pattern"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (#%plain-lambda {} '#:pattern e:expr)))

(define (parse-pattern stx)
  (syntax-parse stx
    #:literal-sets {kernel-literals}
    [:an-induction-pattern
     (parse-expression (@ e))]))

(define-syntax-class an-induction-scheme
  #:description "an induction scheme"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (#%plain-lambda {} '#:scheme e:expr)))

(define (parse-scheme stx)
  (syntax-parse stx
    #:literal-sets {kernel-literals}
    [:an-induction-scheme
     (parse-expression (@ e))]))

(define-syntax-class an-implicit-recursion-clique
  #:description "an implicit recursion clique"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern '#:no-clique))

(define-syntax-class an-explicit-recursion-clique
  #:description "an explicit recursion clique"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (#%plain-lambda {} '#:clique f:expr ...)))

(define-syntax-class an-explicit-recursion-clique-with-controllers
  #:description "an explicit recursion clique with controllers"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (#%plain-lambda {} '#:controllers
      (#%plain-app f:expr 'b:boolean ...)
      ...)))

(define (parse-clique stx)
  (syntax-parse stx
    #:literal-sets {kernel-literals}
    [:an-implicit-recursion-clique
     #false]
    [:an-explicit-recursion-clique
     (clique (src stx)
       (map parse-addr (@ f)))]
    [:an-explicit-recursion-clique-with-controllers
     (controllers (src stx)
       (map parse-addr (@ f))
       (map-map syntax-e (@ b)))]))

(define-syntax-class an-implicit-measure
  #:description "an implicit measure"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern '#:no-measure))

(define-syntax-class an-explicit-measure
  #:description "an explicit measure"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (#%plain-lambda {} '#:measure m:expr)))

(define (parse-measure stx)
  (syntax-parse stx
    #:literal-sets {kernel-literals}
    [:an-implicit-measure #false]
    [:an-explicit-measure
     (parse-expression (@ m))]))

(define-syntax-class no-annotated-goals
  #:description "no annotated goals"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern '#:no-goals))

(define-syntax-class explicit-annotated-goals
  #:description "explicit annotated goals"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (plain-lambda {} '#:goals g:expr ...)))

(define (parse-goals stx)
  (syntax-parse stx
    #:literal-sets {kernel-literals}
    [:no-annotated-goals #false]
    [:explicit-annotated-goals
     (map parse-goal (@ g))]))

(define-syntax-class no-rule-classes
  #:description "no rule classes"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern '#:no-rules))

(define-syntax-class explicit-rule-classes
  #:description "explicit rule classes"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (#%plain-lambda {} '#:rules r:expr ...)))

(define (parse-rules stx)
  (syntax-parse stx
    #:literal-sets {kernel-literals}
    [:no-rule-classes #false]
    [:explicit-rule-classes
     (map parse-rule (@ r))]))

(define-syntax-class a-:DIR-book-option
  #:description "a :DIR book option"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (#%plain-lambda {} '#:dir 'dir:keyword)))

(define-syntax-class a-:SKIP-PROOFS-OKP-book-option
  #:description "a :SKIP-PROOFS-OKP book option"
  #:no-delimit-cut
  #:literal-sets {kernel-literals}
  (pattern 
    (#%plain-lambda {} '#:skip-proofs-ok? 'ok:boolean)))

(define (parse-book-option stx)
  (syntax-parse stx
    #:literal-sets {kernel-literals}
    [:a-:DIR-book-option
     (book-dir (src stx) (syntax-e (@ dir)))]
    [:a-:SKIP-PROOFS-OKP-book-option
     (skip-proofs-ok (src stx) (syntax-e (@ ok)))]))

(define (build-defns define-id lines name-lists bodies)
  (for/list {[line (in-list lines)]
             [names (in-list name-lists)]
             [body (in-list bodies)]}
    (to-syntax #:source line
      (list define-id names body))))

(define (src stx) (to-syntax #:source stx '!))
