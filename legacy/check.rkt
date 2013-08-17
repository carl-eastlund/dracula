#lang mischief

(provide
  check-program!)

(require
  dracula/legacy/type
  dracula/expansion/grammar
  dracula/legacy/registry
  dracula/expansion/names
  dracula/legacy/ref
  dracula/proof/term
  dracula/legacy/rename
  dracula/expansion/alpha
  macro-debugger/emit)

(define (check-program! stxs)
  (check-defns! stxs))

(define (check-defns! stxs #:register? [register? #true])
  (when register?
    (register-defns! stxs))
  (for-each check-defn! stxs))

(define (register-defns! stxs #:decl-names [decl-names #false])
  (parameterize {[current-declaration-names
                  (and decl-names
                    (make-immutable-free-id-table
                      (for/list {[name-id (in-list decl-names)]}
                        (cons name-id #true))))]}
    (for-each register-defn! stxs)))

(define (register-defn! stx)
  (syntax-parse stx

    [:begin-for-syntax-defn (void)]
    [:require-defn (void)]
    [:provide-defn (void)]
    [:syntax-defn (void)]
    [:not-defn (void)]

    [:in-theory-defn (void)]
    [:book-defn (void)]

    [:primitive-defn
     (dict-set! identifier~>type (@ prim-name)
       (function-type
         (@ prim-name)
         (syntax-e (@ arity))
         (::
           (syntax-e (@ sym-package))
           (syntax-e (@ sym-name)))))]

    [:function-defn
     (dict-set! identifier~>type (@ name)
       (function-type (@ name) (length (@ formal)) #false))]

    [:function-decl
     (check-decl-name! (@ name))
     (dict-set! identifier~>type (@ name)
       (function-type (@ name) (syntax-e (@ arity)) #false))]

    [:theorem-defn
     (dict-set! identifier~>type (@ name)
       (theorem-type (@ name)
         (@ formal)
         (@ body)
         (@ rule-class)))]

    [:theorem-decl
     (check-decl-name! (@ name))
     (dict-set! identifier~>type (@ name)
       (theorem-type (@ name)
         (@ formal)
         (@ body)
         (@ rule-class)))]

    [:description-defn

     (for {[name (in-list (@ defn-name))]
           #:when (dict-has-key? identifier~>type name)}
       (wrong-type name
         #:expected "a field name defined in the description"
         #:actual "a field name defined outside the description"))

     (register-defns! (@ body) #:decl-names (@ decl-name))

     (define fields (@ field-name))
     (define names (append (@ decl-name) (@ defn-name)))

     (define field~>role
       (make-immutable-free-id-table
         (map cons fields
           (append
             (map (const 'decl) (@ decl-name))
             (map (const 'defn) (@ defn-name))))))
     (define field~>type
       (make-immutable-free-id-table
         (for/list {[field (in-list fields)]
                    [name (in-list names)]}
           (cons field
             (dict-ref identifier~>type name
               (lambda ()
                 (wrong-type name
                   #:expected
                   "a field name defined or declared in the description"
                   #:actual
                   "an unrecognized name")))))))
     (define field~>name
       (make-immutable-free-id-table
         (map cons fields names)))
     (define defns (@ body))

     (define ty
       (description-type
         (@ name)
         fields
         field~>role
         field~>type
         field~>name
         defns))

     (dict-set! identifier~>type (@ name) ty)
     (for {[field (in-list fields)]}
       (dict-set! identifier~>type field
         (field-type field field)))]

    [:component-decl
     (check-decl-name! (@ name))
     (define desc-ty (check-desc!/type (@ desc)))
     (define comp-ty (desc-type->comp-type (@ name) desc-ty))
     (dict-set! identifier~>type (@ name) comp-ty)]

    [:component-defn
     (define desc-ty (check-comp!/desc-type (@ name) (@ body)))
     (define comp-ty (desc-type->comp-type (@ name) desc-ty))
     (dict-set! identifier~>type (@ name) comp-ty)]

    [:generic-defn
     (define domain-ty (check-desc!/type (@ domain)))
     (define formal-ty (desc-type->comp-type (@ formal) domain-ty))
     (dict-set! identifier~>type (@ formal) formal-ty)
     (define range-ty (check-comp!/desc-type (@ name) (@ body)))
     (define gen-ty (generic-type (@ name) domain-ty (@ formal) range-ty))
     (dict-set! identifier~>type (@ name) gen-ty)]

    [:generic-decl
     (check-decl-name! (@ name))
     (define domain-ty (check-desc!/type (@ domain)))
     (define formal-ty (desc-type->comp-type (@ formal) domain-ty))
     (dict-set! identifier~>type (@ formal) formal-ty)
     (define range-ty (check-desc!/type (@ range)))
     (define gen-ty (generic-type (@ name) domain-ty (@ formal) range-ty))
     (dict-set! identifier~>type (@ name) gen-ty)]

    [:instance-defn
     (define gen-ty (check-ref!/type (@ gen)))
     (unless (generic-type? gen-ty)
       (wrong-type (@ gen)
         #:expected "a generic"
         #:actual gen-ty))
     (define arg-ty (check-ref!/type (@ arg)))
     (unless (component-type? arg-ty)
       (wrong-type (@ arg)
         #:expected "a component"
         #:actual arg-ty))
     (define domain-ty (generic-type-domain gen-ty))
     (check-component-implements-description!
       #:source (@ arg)
       #:component arg-ty
       #:description domain-ty)
     (define formal (generic-type-formal gen-ty))
     (define renaming:formal->actual
       (make-immutable-free-id-table
         (list (cons formal (@ arg)))))
     (define range-ty
       (rename-type (generic-type-range gen-ty)
         renaming:formal->actual))
     (define inst-ty (desc-type->comp-type (@ name) range-ty))
     (dict-set! identifier~>type (@ name) inst-ty)]))

(define (check-comp!/desc-type name stx)
  (syntax-parse stx
    [:component-expr (check-desc!/type (@ desc))]))

(define (desc-type->comp-type ref desc-ty)
  (define field~>type (external-field~>type ref desc-ty))
  (component-type ref desc-ty field~>type))

(define (external-field~>type comp-ref desc-ty)
  (define field~>type0 (description-type-field~>type desc-ty))
  (define field~>role (description-type-field~>role desc-ty))
  (define field~>name (description-type-field~>name desc-ty))
  (define renaming
    (make-immutable-free-id-table
      (for/list {[(field role) (in-dict field~>role)]
                 #:when (memq? role '(defn decl))}
        (define internal (dict-ref field~>name field impossible))
        (cons internal
          (make-deref-expr comp-ref field)))))
  (define field~>type
    (rename-field~>type field~>type0 renaming))
  field~>type)

(define current-declaration-names
  (make-parameter #false))

(define (check-decl-name! name-id)
  (unless (current-declaration-names)
    (wrong-type name-id
      #:expected "declaration at the top level of a description"
      #:actual "a declaration in some other context"))
  (unless (dict-has-key? (current-declaration-names) name-id)
    (wrong-type name-id
      #:expected "a name imported by the enclosing description"
      #:actual "a name not imported by the enclosing description")))

(define (check-defn! stx)
  (syntax-parse stx

    [:begin-for-syntax-defn (void)]
    [:require-defn (void)]
    [:provide-defn (void)]
    [:syntax-defn (void)]
    [:primitive-defn (void)]
    [:function-decl (void)]
    [:component-decl (void)]
    [:generic-decl (void)]
    [:instance-defn (void)]

    [:in-theory-defn
     (check-theory-expr! (@ theory))]

    [:book-defn
     (check-book-options! (@ option))]

    [:function-defn
     (for {[formal-id (in-list (@ formal))]}
       (dict-set! identifier~>type formal-id (value-type formal-id)))
     (check-expr! (@ body))]

    [:theorem-defn
     (for {[formal-id (in-list (@ formal))]}
       (dict-set! identifier~>type formal-id (value-type formal-id)))
     (check-expr! (@ body))
     (when (@ rule-class)
       (for-each check-rule-class! (@ rule-class)))
     (when (@ goal)
       (for-each check-goal! (@ goal)))]

    [:theorem-decl
     (for {[formal-id (in-list (@ formal))]}
       (dict-set! identifier~>type formal-id (value-type formal-id)))
     (check-expr! (@ body))
     (when (@ rule-class)
       (for-each check-rule-class! (@ rule-class)))]

    [:description-defn
     (check-defns! (@ body) #:register? #false)]

    [:component-defn
     (define desc-ty
       (component-type-description
         (dict-ref identifier~>type (@ name) impossible)))
     (check-comp! desc-ty (@ body))]
    [:generic-defn
     (define range-ty
       (generic-type-range
         (dict-ref identifier~>type (@ name) impossible)))
     (check-comp! range-ty (@ body))]

    [e:not-defn (check-expr! (@ e) bad-defn!)]))

(define (check-book-options! stxs)
  (for-each check-book-option! stxs))

(define (check-book-option! stx)
  (syntax-parse stx
    [:book-dir-expr (void)]))

(define (check-rule-class! stx)
  (syntax-parse stx
    [:rule/rewrite-expr
     (check-optional-expr! (@ corollary))]
    [:rule/forward-chaining-expr
     (check-optional-expr! (@ corollary))]
    [:rule/elim-expr
     (check-optional-expr! (@ corollary))]
    [:rule/type-prescription-expr
     (check-optional-expr! (@ corollary))]
    [:rule/definition-expr
     (check-optional-expr! (@ corollary))
     (check-clique! (@ clique))]
    [:rule/induction-expr
     (check-optional-expr! (@ corollary))
     (check-expr! (@ pattern))
     (check-optional-expr! (@ condition))
     (check-expr! (@ scheme))]))

(define (check-clique! stx)
  (syntax-parse stx
    [:optional/none-expr (void)]
    [:option/clique-expr
     (for-each check-fun! (@ fun))]
    [:option/controllers-expr
     (for {[fun-stx (in-list (@ fun))]
           [control-stxs (in-list (@ control))]}
       (check-fun/arity! fun-stx (length control-stxs)))]))

(define (check-optional-expr! stx)
  (syntax-parse stx
    [:optional/some-expr (check-expr! (@ arg))]
    [:optional/none-expr (void)]))

(define (check-goal! stx)
  (syntax-parse stx
    [:goal-expr
     (for-each check-hint! (@ hint))]))

(define (check-hint! stx)
  (syntax-parse stx
    [:hint/by-expr
     (when (@ inst)
       (check-theorem-instance! (@ inst)))]
    [:hint/use-expr
     (for-each check-theorem-instance! (@ inst))]
    [:hint/in-theory-expr
     (check-theory-expr! (@ theory))]
    [:hint/induct-expr
     (check-expr! (@ scheme))]))

(define (check-theorem-instance! stx)
  (syntax-parse stx
    [:app-expr
     (check-rune/arity! (@ fun) (length (@ arg)))
     (for-each check-expr! (@ arg))]))

(define (check-theory-expr! stx)
  (syntax-parse stx
    [:disable-expr
     (for-each check-rune! (@ rune))]
    [:enable-expr
     (for-each check-rune! (@ rune))]))

(define (check-rune/arity! stx expected-arity)
  (define ty (check-ref!/type stx))
  (define actual-arity
    (cond!
      [(function-type? ty)
       (function-type-arity ty)]
      [(theorem-type? ty)
       (length (theorem-type-formals ty))]
      [else (wrong-type stx
              #:expected "a theorem or function"
              #:actual ty)]))
  (check-arity!
    #:source stx
    #:type-desc "a theorem or function"
    #:actual actual-arity
    #:expected expected-arity))

(define (check-rune! stx)
  (define ty (check-ref!/type stx))
  (unless (or (function-type? ty) (theorem-type? ty))
    (wrong-type stx
      #:expected "a theorem or function"
      #:actual ty)))

(define (check-comp! desc-ty stx)
  (syntax-parse stx
    [:component-expr
     (match! desc-ty
       [(description-type _
          desc:fields
          desc:field~>role
          desc:field~>type
          desc:field~>name
          _)

        (define (from-comp? field)
          (eq? (dict-ref desc:field~>role field impossible) 'decl))
        (define-values {fields-from-comp fields-from-desc}
          (partition from-comp? desc:fields))

        (define expected-from-comp (length fields-from-comp))
        (define expected-from-desc (length fields-from-desc))
        (define actual-from-comp (length (@ from-comp)))
        (define actual-from-desc (length (@ from-desc)))
        (unless (and
                  (= expected-from-comp actual-from-comp)
                  (= expected-from-desc actual-from-desc))
          (define (msg from-comp from-desc)
            (format "a description with ~a declared names and ~a defined names"
              from-comp
              from-desc))
          (wrong-type (@ desc)
            #:expected (msg expected-from-comp expected-from-desc)
            #:actual (msg actual-from-comp actual-from-desc)))

        (define fields (append fields-from-comp fields-from-desc))
        (define names (append (@ from-comp) (@ from-desc)))

        (define renaming:desc~>internal
          (make-immutable-free-id-table
            (for/list {[name (in-list names)]
                       [field (in-list fields)]}
              (define desc:name
                (dict-ref desc:field~>name field impossible))
              (cons desc:name name))))

        (for {[name (in-list (@ from-desc))]
              [field (in-list fields-from-desc)]}
          (define desc:type (dict-ref desc:field~>type field impossible))
          (define type (rename-type desc:type renaming:desc~>internal))
          (dict-set! identifier~>type name type))

        (check-defns! (@ body))

        (for {[name (in-list (@ from-comp))]
              [field (in-list fields-from-comp)]}
          (define type (dict-ref identifier~>type name impossible))
          (check-definition-implements-declaration!
            #:source name
            #:definition type
            #:description desc-ty
            #:field field
            #:renaming renaming:desc~>internal))])]))

(define (ref=? one two)
  (cond!
    [(and (false? one) (false? two)) #true]
    [(and (::? one) (::? two)) (equal? one two)]
    [(and (syntax? one) (syntax? two))
     (ref-syntax=? one two)]
    [else #false]))

(define (ref-syntax=? one two)
  (syntax-parse (list one two)
    [(a:var-expr b:var-expr) (free-identifier=? (@ a.name) (@ b.name))]
    [(a:deref-expr b:deref-expr)
     (and (ref-syntax=? (@ a.comp) (@ b.comp))
       (ref-syntax=? (@ a.field) (@ b.field)))]
    [(_ _) #false]))

(define (check-desc!/type stx)
  (syntax-parse stx
    [:refine-expr

     (define ty0 (check-desc!/type (@ base)))

     (define fields0 (description-type-fields ty0))
     (define field~>role0 (description-type-field~>role ty0))
     (define field~>type0 (description-type-field~>type ty0))
     (define field~>name0 (description-type-field~>name ty0))
     (define defns0 (description-type-defns ty0))

     (for {[field-id (in-list (@ field))]}
       (unless (dict-has-key? field~>type0 field-id)
         (wrong-type (@ base)
           #:expected (format "a description with field ~s" (syntax-e field-id))
           #:actual ty0)))

     (define renaming:desc~>refined
       (make-immutable-free-id-table
         (for/list {[field (in-list (@ field))]
                    [ref (@ defn)]}
           (cons (dict-ref field~>name0 field impossible) ref))))

     (for {[field (in-list (@ field))]
           [ref (in-list (@ defn))]}
       (define role (dict-ref field~>role0 field impossible))
       (unless (eq? role 'decl)
         (wrong-type field
           #:expected (role->phrase 'decl)
           #:actual (role->phrase role)))
       (define type (check-ref!/type ref))
       (check-definition-implements-declaration!
         #:source ref
         #:definition type
         #:description ty0
         #:field field
         #:renaming renaming:desc~>refined))

     (define fields fields0)
     (define field~>role
       (for/fold
           {[field~>role field~>role0]}
           {[field (in-list (@ field))]}
         (dict-set field~>role field 'refine)))
     (define field~>type
       (rename-field~>type field~>type0 renaming:desc~>refined))
     (define field~>name field~>name0)
     (define defns
       (refine-defns defns0 renaming:desc~>refined))

     (description-type
       #false
       fields
       field~>role
       field~>type
       field~>name
       defns)]
    [e:expr
     (define ty (check-ref!/type (@ e)))
     (unless (description-type? ty)
       (wrong-type stx
         #:expected "a description"
         #:actual ty))
     ty]))

(define (refine-defns stxs renaming:refinement)
  (define (refined? stx)
    (for/and {[name (in-list (defn-names stx))]}
      (dict-has-key? renaming:refinement name)))
  (filter-not refined? stxs))

(define (check-component-implements-description!
                #:source source
                #:component comp-ty
                #:description desc-ty)
  (match! desc-ty
    [(description-type _
       desc:fields
       desc:field~>role
       desc:field~>type
       desc:field~>name
       _)

     (define comp-ref (type-ref comp-ty))
     (define comp:field~>type
       (component-type-field~>type comp-ty))

     (for {[field (in-list desc:fields)]
           #:unless (dict-has-key? comp:field~>type field)}
       (wrong-type source
         #:expected (format "a component with field ~s" (syntax-e field))
         #:actual (format "a component without field ~s" (syntax-e field))))

     (define renaming:desc~>comp
       (make-immutable-free-id-table
         (for/list {[field (in-list desc:fields)]}
           (define desc:name (dict-ref desc:field~>name field impossible))
           (define comp:field-ref (make-deref-expr comp-ref field))
           (cons desc:name comp:field-ref))))

     (for {[field (in-list desc:fields)]}
       (match! (dict-ref desc:field~>role field impossible)
         ['decl
          (define type (dict-ref comp:field~>type field impossible))
          (check-definition-implements-declaration!
            #:source source
            #:definition type
            #:description desc-ty
            #:field field
            #:renaming renaming:desc~>comp)]
         [(or 'defn 'refine)
          (define comp:type (dict-ref comp:field~>type field impossible))
          (define desc:type0 (dict-ref desc:field~>type field impossible))
          (define desc:type (rename-type desc:type0 renaming:desc~>comp))
          (check-same-type!
            #:source source
            #:expected desc:type
            #:actual comp:type)]))]))

(define (check-same-type!
                #:source source
                #:expected expected-ty
                #:actual actual-ty)
  (define expected-ref (type-ref expected-ty))
  (define actual-ref (type-ref actual-ty))
  (unless (ref=? expected-ref actual-ref)
    (define (msg ty ref)
      (format "~a ~a" (type->string ty) (ref->phrase ref)))
    (wrong-type source
      #:expected (msg expected-ty expected-ref)
      #:actual (msg actual-ty actual-ref)))
  (check-subtype!
    #:source source
    #:expected expected-ty
    #:actual actual-ty)
  (check-subtype!
    #:source source
    #:expected actual-ty
    #:actual expected-ty))

(define (check-definition-implements-declaration!
                #:source source
                #:definition actual-ty
                #:description desc-ty
                #:field field
                #:renaming renaming:desc~>defn)
  (define desc:field~>type (description-type-field~>type desc-ty))
  (define desc:type (dict-ref desc:field~>type field impossible))
  (define expected-ty (rename-type desc:type renaming:desc~>defn))
  (check-subtype!
    #:source source
    #:actual actual-ty
    #:expected expected-ty))

(define (check-subtype!
                #:source source
                #:expected expected-ty
                #:actual actual-ty)
  (match*! {expected-ty actual-ty}

    [{(value-type _) (value-type _)}
     (impossible "definitions are never values")]

    [{(function-type _ expected-arity _)
      (function-type _ actual-arity _)}
     (check-arity!
       #:source source
       #:type-desc "a function"
       #:expected expected-arity
       #:actual actual-arity)]

    [{(theorem-type _ expected-formals expected-body expected-rule-classes)
      (theorem-type _ actual-formals actual-body actual-rule-classes)}
     (check-arity!
       #:source source
       #:type-desc "a theorem"
       #:expected (length expected-formals)
       #:actual (length actual-formals))
     (define alpha-table
       (make-immutable-free-id-table
         (map cons actual-formals expected-formals)))
     (unless (alpha-expr=? actual-body expected-body alpha-table)
       (wrong-type source
         #:expected "matching theorem bodies"
         #:actual "mismatched theorem bodies"))
     (unless (iff expected-rule-classes actual-rule-classes)
       (define (msg explicit?)
         (format "~a rule classes"
           (if explicit? "explicit" "implicit")))
       (wrong-type source
         #:actual (msg actual-rule-classes)
         #:expected (msg expected-rule-classes)))
     (when (and expected-rule-classes actual-rule-classes)
       (unless (alpha-rule-classes=?
                 actual-rule-classes
                 expected-rule-classes
                 alpha-table)
         (wrong-type "source"
           #:expected "matching rule classes"
           #:actual "mismatched rule classes")))]

    [{(field-type _ expected-field)
      (field-type _ actual-field)}
     (unless (free-identifier=? expected-field actual-field)
       (define (msg desc field)
         (format "an accessor for field ~s" (syntax-e field)))
       (wrong-type source
         #:expected (msg expected-field)
         #:actual (msg actual-field)))]

    [{(description-type _
        expected-fields
        expected-field~>role
        expected-field~>type
        _
        _)
      (description-type _
        actual-fields
        actual-field~>role
        actual-field~>type
        _
        _)}
     ;; Entirely invariant checking:
     (check-field-roles!
       #:source source
       #:expected expected-field~>role
       #:actual actual-field~>role)
     (check-field-roles!
       #:source source
       #:expected actual-field~>role
       #:actual expected-field~>role)
     (check-field-types!
       #:source source
       #:expected expected-field~>type
       #:actual actual-field~>type)
     (check-field-types!
       #:source source
       #:expected actual-field~>type
       #:actual expected-field~>type)]

    [{(component-type _ expected-desc _)
      (component-type _ _ _)}
     (check-component-implements-description!
       #:source source
       #:component actual-ty
       #:description expected-desc)]

    [{(generic-type expected-ref expected-domain expected-formal expected-range)
      (generic-type actual-ref actual-domain actual-formal actual-range)}
     ;; Contravariant domain checking:
     (define input (fresh 'generic-input))
     (define expected-input (desc-type->comp-type input expected-domain))
     (dict-set! identifier~>type input expected-input)
     (check-component-implements-description!
       #:source source
       #:component expected-input
       #:description actual-domain)
     ;; Covariant range checking:
     (define output (fresh 'generic-output))
     (define (make-output-renaming formal)
       (make-immutable-free-id-table
         (list (cons formal input))))
     (define actual-output
       (desc-type->comp-type output
         (rename-type actual-range
           (make-output-renaming actual-formal))))
     (dict-set! identifier~>type output actual-output)
     (define expected-output
       (rename-type expected-range
         (make-output-renaming expected-formal)))
     (check-component-implements-description!
       #:source source
       #:component actual-output
       #:description expected-output)
     ;; Cleanup:
     (dict-remove! identifier~>type input)
     (dict-remove! identifier~>type output)]

    [{expected-ty actual-ty}
     (wrong-type source
       #:expected expected-ty
       #:actual actual-ty)]))

(define (check-arity!
                #:source source
                #:type-desc str
                #:expected expected-arity
                #:actual actual-arity)
  (unless (= expected-arity actual-arity)
    (define (msg arity)
      (format "~a of arity ~a" str arity))
    (wrong-type source
      #:expected (msg expected-arity)
      #:actual (msg actual-arity))))

(define (check-field-roles!
                #:source source
                #:expected expected-field~>role
                #:actual actual-field~>role)
  (for {[(field expected-role) (in-dict expected-field~>role)]}
    (define (field-not-found)
      (wrong-type source
        #:expected (format "a description with field ~s" (syntax-e field))
        #:actual (format "a description without field ~s" (syntax-e field))))
    (define actual-role (dict-ref actual-field~>role field field-not-found))
    (unless (eq? actual-role expected-role)
      (define (msg role)
        (format "~s to be ~a"
          (syntax-e field)
          (role->phrase role)))
      (wrong-type source
        #:expected (msg expected-role)
        #:actual (msg actual-role)))))

(define (check-field-types!
                #:source source
                #:expected expected-field~>type
                #:actual actual-field~>type)
  (for {[(field expected-type) (in-dict expected-field~>type)]}
    (define actual-type (dict-ref actual-field~>type field impossible))
    (check-subtype!
      #:source source
      #:expected expected-type
      #:actual actual-type)))

(define (check-expr! stx [bad-term! bad-expr!])
  (syntax-parse stx
    [:quote-expr (void)]
    [:if-expr
     (check-expr! (@ test))
     (check-expr! (@ then))
     (check-expr! (@ else))]
    [:let-expr
     (for-each check-expr! (@ rhs))
     (for {[lhs-id (in-list (@ lhs))]}
       (dict-set! identifier~>type lhs-id (value-type lhs-id)))
     (check-expr! (@ body))]
    [:app-expr
     (check-fun/arity! (@ fun) (length (@ arg)))
     (for-each check-expr! (@ arg))]
    [:var-expr
     (define ty
       (dict-ref identifier~>type (@ name) #false))
     (unless (value-type? ty)
       (wrong-type stx
         #:actual ty
         #:expected "a value"))]
    [_ (bad-term! stx)]))

(define (check-fun! stx)
  (define ty (check-ref!/type stx))
  (unless (function-type? ty)
    (wrong-type stx
      #:actual ty
      #:expected "a function")))

(define (check-fun/arity! stx arity)
  (define ty (check-ref!/type stx))
  (unless (and (function-type? ty)
            (= (function-type-arity ty) arity))
    (wrong-type stx
      #:actual ty
      #:expected (format "a function of arity ~a" arity))))

(define (check-comp-ref!/type stx)
  (define ty (check-ref!/type stx))
  (unless (component-type? ty)
    (wrong-type stx
      #:actual ty
      #:expected "a component"))
  ty)

(define (check-field-ref!/type stx)
  (define ty (check-ref!/type stx))
  (unless (field-type? ty)
    (wrong-type stx
      #:actual ty
      #:expected "a field accessor"))
  ty)

(define (check-ref!/type stx)
  (syntax-parse stx

    [:var-expr
     (dict-ref identifier~>type (@ name)
       (lambda ()
         (wrong-type (@ name)
           #:expected "a defined or primitive name"
           #:actual "an unrecognized name")))]

    [:deref-expr
     (define comp-ty (check-comp-ref!/type (@ comp)))
     (define field-ty (check-field-ref!/type (@ field)))
     (dict-ref (component-type-field~>type comp-ty) (field-type-field field-ty)
       (lambda ()
         (wrong-type (@ comp)
           #:expected
           (format "a component with field ~s"
             (syntax-e (field-type-field field-ty)))
           #:actual comp-ty)))]

    [_ (wrong-type stx
         #:expected "a variable name or a component member reference"
         #:actual "something else")]))

(define (bad-defn! stx)
  (wrong-type stx
    #:expected "a definition or a value expression"
    #:actual "something else"))

(define (bad-expr! stx)
  (wrong-type stx
    #:expected "a value expression"
    #:actual "something else"))

(define (wrong-type stx
                #:actual actual
                #:expected expected)
  (define-values {stx1 stx2}
    (cond
      [(current-syntax-context) =>
       (lambda (ctx) (values ctx stx))]
      [else (values stx #false)]))
  (define msg
    (format "expected ~a, but found ~a"
      (type/message->string expected)
      (type/message->string actual)))
  (when (identifier? stx)
    (check-id! stx)
    (for {[id (in-dict-keys identifier~>type)]}
      (check-id! id)
      (check-ids! id stx)))
  (emit-remark #:unmark? #false
    "syntax error"
    stx
    msg
    "all bindings"
    (for/list {[(id ty) (in-dict identifier~>type)]}
      (list id (type->string ty))))
  (raise-syntax-error 'check-program! msg stx1 stx2))

(define (check-id! one)
  (define two (syntax-local-introduce one))
  (unless (free-identifier=? one two)
    (emit-remark #:unmark? #false "mark makes not free=" one two)
    (wrong-syntax one "mark makes not free=: ~s" (syntax-e one))))

(define (check-ids! one two)
  (when (bound-identifier=? one two)
    (unless (free-identifier=? one two)
      (emit-remark #:unmark? #false "bound= but not free=" one two)
      (wrong-syntax two
        "bound-identifier=? and free-identifier=? failure: ~s / ~s"
        (syntax-e one)
        (syntax-e two)))))

(define (type/message->string x)
  (cond!
    [(type? x) (type->string x)]
    [(false? x) "an unrecognized name"]
    [(string? x) x]))

(define (role->phrase x)
  (match! x
    ['decl "a declared field"]
    ['defn "a defined field"]
    ['refine "a refined field"]))
