#lang dracula/kernel

(provide
  datatype
  ~datatype
  datatype-description datatype-component
  ~datatype-definitions datatype-definitions)

(require
  dracula/prelude/core
  dracula/prelude/base/match
  dracula/prelude/base/struct
  dracula/prelude/base/primitive
  dracula/prelude/base/shorthand
  (for-syntax
    mischief))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Sugar" Syntax Classes

(begin-for-syntax

  (define-splicing-syntax-class named-datatypes-spec
    #:attributes {simple}
    (pattern (~seq names:names-spec types:datatypes-spec)
      #:attr simple #'{names.simple types.simple ...}))

  (define-splicing-syntax-class names-spec
    #:attributes {simple}
    (pattern
      (~seq name:id
        (~or
          (~optional (~seq #:component comp:id)
            #:defaults {[comp (identifier-titlecase (@ name))]})
          (~optional (~seq #:description desc:id)
            #:defaults {[desc (identifier-upcase (@ name))]}))
        ...)
      #:attr simple #'{name comp desc}))

  (define-splicing-syntax-class datatypes-spec
    #:attributes {[simple 1]}
    (pattern (~seq variant:named-variant-spec ...)
      #:fail-when (check-duplicate-identifier (@ variant.name))
      "found a duplicate variant name"
      #:attr [simple 1] (@ variant.simple)))

  (define-syntax-class named-variant-spec
    #:attributes {name simple}
    (pattern [name:id variant:variant-spec]
      #:attr simple #'[name . variant.simple]))

  (define-splicing-syntax-class variant-spec
    #:attributes {simple}
    (pattern (~seq #:list-of ~! element:type-spec)
      #:attr simple #'[#:list-of element.simple])
    (pattern (~seq (name:id [field:id type:type-spec] ...) ...)
      #:attr simple #'[#:structs {(name [field type.simple] ...) ...}]))

  (define-syntax-class type-spec
    #:attributes {simple}
    #:literals {quote}
    (pattern (quote ~! value)
      #:attr simple #'(quote value))
    (pattern value:self-quoting
      #:attr simple #'(quote value))
    (pattern predicate:expr
      #:attr simple (@ predicate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Simple" Syntax Classes

(begin-for-syntax

  (define-syntax-class simple-datatype
    #:attributes {name comp desc [variant 1]}
    (pattern [{name:id comp:id desc:id} variant:expr ...]))

  (define-syntax-class simple-variant
    #:attributes {name}
    (pattern [name:id _:keyword _:expr]))

  (define-syntax-class simple-struct
    #:attributes {name [clause 1]}
    (pattern [name:id #:structs {clause:expr ...}]))

  (define-syntax-class struct-clause
    #:attributes {name [field 1] [type 1]}
    (pattern (name:id [field:id type:expr] ...)))

  (define-syntax-class simple-list
    #:attributes {name element}
    (pattern [name:id #:list-of element:expr])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recursion Lookup

(begin-for-syntax

  (define-syntax-class recursive-id
    #:attributes {data variant}
    (pattern x:id
      #:attr data
      (syntax-parameter-value
        #'current-datatype)
      #:when (@ data)
      #:attr variant
      (syntax-parse (@ data)
        [d:simple-datatype
         (for/or {[v-stx (in-list (@ d.variant))]}
           (define/syntax-parse v:simple-variant v-stx)
           (define p-stx (id+suffix (@ v.name) /variant-predicate))
           (and (free-identifier=? p-stx (@ x)) v-stx))])
      #:when (@ variant))))

(define-syntax-parameter current-datatype #false)

(define-syntax (with-datatype stx)
  (syntax-parse stx
    [(_ data:simple-datatype body:expr)
     #'(syntax-parameterize {[current-datatype #'data]} body)]))

(define-syntax (recursive-dispatch stx)
  (syntax-parse stx
    [(_ attr:id with-data-template:expr without-data-expr:expr)
     (define data-stx
       (syntax-parameter-value
         #'current-datatype))
     (cond
       [(not data-stx) #'without-data-expr]
       [else
        (define/syntax-parse data data-stx)
        (define with-data-stx
          (eval-in-scope
            #'(syntax-parse (quote-syntax data)
                [(~var attr simple-datatype)
                 (syntax with-data-template)])))
        with-data-stx])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building Names

(begin-for-syntax

  (define /recursive-predicate #'?)
  (define /variant-predicate #'?)
  (define /recursion-function #'-recursion)
  (define /recursion-theorem #'-induction)

  (define /recursive-predicate-theorem #'?/variants)
  (define /variant-predicate-theorem #'?/definition)

  (define (id+suffix prefix suffix)
    (format-id prefix #:source prefix "~a~a" prefix suffix)))

(define-syntax (define-with-suffix stx)
  (syntax-parse stx
    [(_ suffix-expr:expr (form:id (name:id . args) . body))
     (define suffix (eval-in-scope (@ suffix-expr)))
     (define/syntax-parse name/suffix:id (id+suffix (@ name) suffix))
     #'(form (name/suffix . args) . body)]))

(define-syntax (call-with-suffix stx)
  (syntax-parse stx
    [(_ suffix-expr:expr (name:id . args))
     (define suffix (eval-in-scope (@ suffix-expr)))
     (define/syntax-parse name/suffix:id (id+suffix (@ name) suffix))
     #'(name/suffix . args)]))

(define-syntax (with-suffix stx)
  (syntax-parse stx
    [(_ suffix-expr:expr name:id)
     (define suffix (eval-in-scope (@ suffix-expr)))
     (define/syntax-parse name/suffix:id (id+suffix (@ name) suffix))
     #'name/suffix]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Definitions

(define-syntax (datatype stx)
  (syntax-parse stx
    [(_ data:named-datatypes-spec) #'(datatype data.simple)]
    [(_ data:simple-datatype)
     #'(begin
         (datatype-description data)
         (datatype-component data)
         (open data.comp))]))

(define-syntax (~datatype stx)
  (syntax-parse stx
    [(_ data:named-datatypes-spec) #'(~datatype data.simple)]
    [(_ data:simple-datatype)
     #'(begin
         (~datatype-description data)
         (~datatype-component data)
         (~open data.comp))]))

(define-syntax (~datatype-description stx)
  (syntax-parse stx
    [(_ data:named-datatypes-spec) #'(~datatype-description data.simple)]
    [(_ data:simple-datatype)
     #'(~description data.desc
         (~datatype-definitions data))]))

(define-syntax (datatype-description stx)
  (syntax-parse stx
    [(_ data:named-datatypes-spec) #'(datatype-description data.simple)]
    [(_ data:simple-datatype)
     #'(description data.desc
         (~datatype-definitions data))]))

(define-syntax (~datatype-component stx)
  (syntax-parse stx
    [(_ data:named-datatypes-spec) #'(~datatype-component data.simple)]
    [(_ data:simple-datatype)
     #'(~component data.comp #:> data.desc)]))

(define-syntax (datatype-component stx)
  (syntax-parse stx
    [(_ data:named-datatypes-spec) #'(datatype-component data.simple)]
    [(_ data:simple-datatype)
     #'(component data.comp #:> data.desc
         (datatype-definitions data))]))

(define-syntax (~datatype-definitions stx)
  (syntax-parse stx
    [(_ data:named-datatypes-spec) #'(~datatype-definitions data.simple)]
    [(_ data:simple-datatype)
     #'(begin
         (~structures data)
         (~predicates data)
         (~recursion data))]))

(define-syntax (datatype-definitions stx)
  (syntax-parse stx
    [(_ data:named-datatypes-spec) #'(datatype-definitions data.simple)]
    [(_ data:simple-datatype)
     #'(begin
         (structures data)
         (predicates data)
         (recursion data))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structs

(define-syntax (~structures stx)
  (syntax-parse stx
    [(_ data:simple-datatype)
     #'(begin
         (~variant-structures data.desc data.comp data.variant)
         ...)]))

(define-syntax (structures stx)
  (syntax-parse stx
    [(_ data:simple-datatype)
     #'(begin
         (variant-structures data.desc data.comp data.variant)
         ...)]))

(define-syntax (~variant-structures stx)
  (syntax-parse stx
    [(_ desc:id comp:id variant:simple-list) #'(begin)]
    [(_ desc:id comp:id variant:simple-struct)
     (define/syntax-parse {clause:struct-clause ...} (@ variant.clause))
     #'(~structs/external (clause.name clause.field ...) ...)]))

(define-syntax (variant-structures stx)
  (syntax-parse stx
    [(_ desc:id comp:id variant:simple-list) #'(begin)]
    [(_ desc:id comp:id variant:simple-struct)
     (define/syntax-parse {clause:struct-clause ...} (@ variant.clause))
     #'(structs/internal (clause.name clause.field ...) ...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates

(define-syntax (~predicates stx)
  (syntax-parse stx
    [(_ data:simple-datatype)
     #'(begin
         (~recursive-predicate data)
         (~variant-predicate data.variant data)
         ...)]))

(define-syntax (predicates stx)
  (syntax-parse stx
    [(_ data:simple-datatype)
     #'(begin
         (recursive-predicate data)
         (variant-predicate data.variant data)
         ...)]))

(define-syntax (~recursive-predicate stx)
  (syntax-parse stx
    [(_ data:simple-datatype)
     #'(begin
         (define-with-suffix /recursive-predicate
           (~define (data.name x ty)))
         (define-with-suffix /recursive-predicate-theorem
           (~theorem (data.name x)
             (and
               (equal? (datatype-check data x (tag data.variant))
                 (variant-check data.variant x))
               ...))))]))

(define-syntax (recursive-predicate stx)
  (syntax-parse stx
    [(_ data:simple-datatype)
     #'(begin
         (define-with-suffix /recursive-predicate
           (define (data.name x ty)
             #:measure (size x)
             (datatype-predicate-body data x ty)))
         (define-with-suffix /recursive-predicate-theorem
           (theorem (data.name x) #:disable
             (and
               (equal? (datatype-check data x (tag data.variant))
                 (variant-check data.variant x))
               ...))))]))

(define-syntax (~variant-predicate stx)
  (syntax-parse stx
    [(_ variant:simple-variant data:simple-datatype)
     #'(begin
         (define-with-suffix /variant-predicate
           (~define (variant.name x)))
         (define-with-suffix /variant-predicate-theorem
           (~theorem (variant.name x)
             (equal? (variant-check variant x)
               (variant-predicate-body variant x))
             #:rule-classes
               {#:definition
                [#:controllers
                 [((variant-predicate-name data.variant) #true) ...]]})))]))

(define-syntax (variant-predicate stx)
  (syntax-parse stx
    [(_ variant:simple-variant data:simple-datatype)
     #'(begin
         (define-with-suffix /variant-predicate
           (define (variant.name x)
             (datatype-check data x (tag variant))))
         (define-with-suffix /variant-predicate-theorem
           (theorem (variant.name x) #:disable
             (equal? (variant-check variant x)
               (variant-predicate-body variant x))
             #:rule-classes
               {#:definition
                [#:controllers
                 [((variant-predicate-name data.variant) #true) ...]]})))]))

(define-syntax (datatype-predicate-body stx)
  (syntax-parse stx
    [(_ data:simple-datatype x:id ty:id)
     #'(with-datatype data
         (cond
           [(equal? ty (tag data.variant))
            (variant-predicate-body data.variant x)]
           ...
           [#:else #false]))]))

(define-syntax (variant-predicate-body stx)
  (syntax-parse stx
    [(_ variant:simple-struct x:id)
     #'(cond
         [(is? variant.clause x)
          (struct-predicate-body variant.clause x)]
         ...
         [#:else #false])]
    [(_ variant:simple-list arg:expr)
     #'(match arg
         ['() #true]
         [(cons head tail)
          (and (check variant.element head)
            (variant-check variant tail))]
         [_ #false])]))

(define-syntax (struct-predicate-body stx)
  (syntax-parse stx
    [(_ clause:struct-clause x:id)
     #'(and
         (let {[clause.field (get clause clause.field x)]}
           (check clause.type clause.field))
         ...)]))

(define-syntax (datatype-check stx)
  (syntax-parse stx
    [(_ data:simple-datatype x:expr ty:expr)
     #'(call-with-suffix /recursive-predicate
         (data.name x ty))]))

(define-syntax (variant-check stx)
  (syntax-parse stx
    [(_ variant:simple-variant x:expr)
     #'(recursive-dispatch data
         (datatype-check data x (tag variant))
         (call-with-suffix /variant-predicate
           (variant.name x)))]))

(define-syntax (variant-predicate-name stx)
  (syntax-parse stx
    [(_ variant:simple-variant)
     #'(with-suffix /variant-predicate variant.name)]))

(define-syntax (check stx)
  (syntax-parse stx
    #:literals {quote}
    [(_ rec:recursive-id ~! x:id)
     #'(datatype-check rec.data x
         (tag rec.variant))]
    [(_ pred:expr x:id) #'(pred x)]
    [(_ (quote value) x:id)
     #'(equal? x (quote value))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recursion

(define-syntax (~recursion stx)
  (syntax-parse stx
    [(_ data:simple-datatype)
     #'(begin
         (define-with-suffix /recursion-function
           (~define (data.name x ty)
             #:measure (size x)
             (datatype-recursion-body data x ty)))
         (define-with-suffix /recursion-theorem
           (~theorem (data.name x ty)
             #true
             #:rule-classes
               {#:induction
                [#:pattern (datatype-check data x ty)
                 #:scheme (datatype-recur data x ty)]})))]))

(define-syntax (recursion stx)
  (syntax-parse stx
    [(_ data:simple-datatype)
     #'(begin
         (define-with-suffix /recursion-function
           (define (data.name x ty)
             #:measure (size x)
             (datatype-recursion-body data x ty)))
         (define-with-suffix /recursion-theorem
           (theorem (data.name x ty) #:disable
             #true
             #:rule-classes
               {#:induction
                [#:pattern (datatype-check data x ty)
                 #:scheme (datatype-recur data x ty)]})))]))

(define-syntax (datatype-recursion-body stx)
  (syntax-parse stx
    [(_ data:simple-datatype x:id ty:id)
     #'(with-datatype data
         (cond
           [(equal? ty (tag data.variant))
            (variant-recursion-body data.variant x)]
           ...
           [#:else x]))]))

(define-syntax (variant-recursion-body stx)
  (syntax-parse stx
    [(_ variant:simple-struct x:id)
     #'(cond
         [(is? variant.clause x)
          (struct-recursion-body variant.clause x)]
         ...
         [#:else x])]
    [(_ variant:simple-list x:id)
     #'(match x
         ['() '()]
         [(cons head tail)
          (cons (recur variant.element head)
            (variant-recur variant tail))]
         [_ x])]))

(define-syntax (struct-recursion-body stx)
  (syntax-parse stx
    [(_ clause:struct-clause x:id)
     #'(make clause
         (let {[clause.field (get clause clause.field x)]}
           (recur clause.type clause.field))
         ...)]))

(define-syntax (datatype-recur stx)
  (syntax-parse stx
    [(_ data:simple-datatype x:expr ty:expr)
     #'(call-with-suffix /recursion-function
         (data.name x ty))]))

(define-syntax (variant-recur stx)
  (syntax-parse stx
    [(_ variant:simple-variant x:expr)
     #'(recursive-dispatch data
         (datatype-recur data x (tag variant))
         x)]))

(define-syntax (recur stx)
  (syntax-parse stx
    #:literals {quote}
    [(_ rec:recursive-id ~! x:id)
     #'(datatype-recur rec.data x
         (tag rec.variant))]
    [(_ pred:expr x:id) #'x]
    [(_ (quote value) x:id)
     #'(quote value)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Helpers

(define-syntax (tag stx)
  (syntax-parse stx
    [(_ variant:simple-variant)
     #'(quote variant.name)]))

(define-syntax (is? stx)
  (syntax-parse stx
    [(_ clause:struct-clause arg:expr)
     #'(is-struct? clause.name arg)]))

(define-syntax (make stx)
  (syntax-parse stx
    [(_ clause:struct-clause arg:expr ...)
     #'(make-struct clause.name arg ...)]))

(define-syntax (get stx)
  (syntax-parse stx
    [(_ clause:struct-clause field:id arg:expr)
     #'(get-struct-field clause.name field arg)]))
