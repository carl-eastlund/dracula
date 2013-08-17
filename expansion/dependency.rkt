#lang mischief

(provide
  sort-defns
  defns-bindings
  defn-bindings
  defns-dependencies
  defn-dependencies
  exprs-dependencies
  expr-dependencies)

(require
  dracula/expansion/grammar)

;; Note: bindings cannot overlap.
(define (defns-bindings defns)
  (append* (defn-bindings defns)))

(define (defn-bindings defn)
  (dict-keys (defn-binding-set defn)))
(define (defns-dependencies defns)
  (dict-keys (defns-dependency-set defns)))
(define (defn-dependencies defn)
  (dict-keys (defn-dependency-set defn)))
(define (exprs-dependencies exprs)
  (dict-keys (exprs-dependency-set exprs)))
(define (expr-dependencies expr)
  (dict-keys (expr-dependency-set expr)))

(define (defn-binding-set defn)
  (syntax-parse defn
    [:not-defn (id-set)]
    [:begin-for-syntax-defn (id-set)]
    [:require-defn (id-set)]
    [:provide-defn (id-set)]
    [:syntax-defn (id-set)]
    [:in-theory-defn (id-set)]
    [:book-defn (id-set)]
    [:description-defn (apply id-set (@ name) (@ field-name))]
    [:component-defn (id-set (@ name))]
    [:component-decl (id-set (@ name))]
    [:generic-defn (id-set (@ name))]
    [:generic-decl (id-set (@ name))]
    [:instance-defn (id-set (@ name))]
    [:primitive-defn (id-set (@ prim-name))]
    [:function-defn (id-set (@ name))]
    [:function-decl (id-set (@ name))]
    [:theorem-defn (id-set (@ name))]
    [:theorem-decl (id-set (@ name))]))

(define (defns-dependency-set defns)
  (apply id-set-union
    (map defn-dependency-set defns)))

(define (defn-dependency-set defn)
  (syntax-parse defn
    [:not-defn (expr-dependency-set defn)]
    [:begin-for-syntax-defn (id-set)]
    [:require-defn (id-set)]
    [:provide-defn (id-set)]
    [:syntax-defn (id-set)]
    [:in-theory-defn (theory-expr-dependency-set (@ theory))]
    [:book-defn (book-options-dependency-set (@ option))]
    [:description-defn
     (id-set-difference
       (defns-dependency-set (@ body))
       (apply id-set (@ decl-name))
       (apply id-set (@ defn-name)))]
    [:component-defn (component-dependency-set (@ body))]
    [:component-decl (description-dependency-set (@ desc))]
    [:generic-defn
     (id-set-union
       (description-dependency-set (@ domain))
       (id-set-difference
         (component-dependency-set (@ body))
         (id-set (@ formal))))]
    [:generic-decl
     (id-set-union
       (description-dependency-set (@ domain))
       (id-set-difference
         (description-dependency-set (@ range))
         (id-set (@ formal))))]
    [:instance-defn
     (id-set-union
       (ref-dependency-set (@ gen))
       (ref-dependency-set (@ arg)))]
    [:primitive-defn (id-set)]
    [:function-defn
     (define body-dependencies
       (id-set-difference
         (expr-dependency-set (@ body))
         (id-set (@ name))))
     (cond!
       [(@ measure)
        (id-set-union body-dependencies
          (expr-dependency-set (@ measure)))]
       [else body-dependencies])]
    [:function-decl (id-set)]
    [:theorem-defn
     (define body-dependencies
       (expr-dependency-set (@ body)))
     (define rule-dependencies
       (cond!
         [(@ rule-class)
          (id-set-union body-dependencies
            (rule-classes-dependency-set (@ rule-class)))]
         [else body-dependencies]))
     (define goal-dependencies
       (cond!
         [(@ goal)
          (id-set-union rule-dependencies
            (goals-dependency-set (@ goal)))]
         [else rule-dependencies]))
     goal-dependencies]
    [:theorem-decl
     (define body-dependencies
       (expr-dependency-set (@ body)))
     (define rule-dependencies
       (cond!
         [(@ rule-class)
          (id-set-union body-dependencies
            (rule-classes-dependency-set (@ rule-class)))]
         [else body-dependencies]))
     rule-dependencies]))

(define (book-options-dependency-set opts)
  (apply id-set-union
    (map book-option-dependency-set opts)))

(define (book-option-dependency-set opt)
  (syntax-parse opt
    [:book-dir-expr (id-set)]))

(define (goals-dependency-set goals)
  (apply id-set-union
    (map goal-dependency-set goals)))

(define (goal-dependency-set goal)
  (syntax-parse goal
    [:goal-expr
     (hints-dependency-set (@ hint))]))

(define (hints-dependency-set hints)
  (apply id-set-union
    (map hint-dependency-set hints)))

(define (hint-dependency-set hint)
  (syntax-parse hint
    [:hint/by-expr
     (cond!
       [(@ inst) (theorem-instance-dependency-set (@ inst))]
       [else (id-set)])]
    [:hint/use-expr
     (theorem-instances-dependency-set (@ inst))]
    [:hint/in-theory-expr
     (theory-expr-dependency-set (@ theory))]
    [:hint/induct-expr
     (expr-dependency-set (@ scheme))]))

(define (theorem-instances-dependency-set stxs)
  (apply id-set-union
    (map theorem-instance-dependency-set stxs)))

(define (theorem-instance-dependency-set stx)
  (syntax-parse stx
    [:app-expr
     (id-set-union (ref-dependency-set (@ fun))
       (exprs-dependency-set (@ arg)))]))

(define (theory-expr-dependency-set stx)
  (syntax-parse stx
    [:disable-expr
     (refs-dependency-set (@ rune))]
    [:enable-expr
     (refs-dependency-set (@ rune))]))

(define (rule-classes-dependency-set rule-classes)
  (apply id-set-union
    (map rule-class-dependency-set rule-classes)))

(define (rule-class-dependency-set rule-class)
  (syntax-parse rule-class
    [:rule/rewrite-expr
     (optional-expr-dependency-set (@ corollary))]
    [:rule/forward-chaining-expr
     (optional-expr-dependency-set (@ corollary))]
    [:rule/elim-expr
     (optional-expr-dependency-set (@ corollary))]
    [:rule/type-prescription-expr
     (optional-expr-dependency-set (@ corollary))]
    [:rule/definition-expr
     (id-set-union
       (optional-expr-dependency-set (@ corollary))
       (clique-dependency-set (@ clique)))]
    [:rule/induction-expr
     (id-set-union
       (optional-expr-dependency-set (@ corollary))
       (expr-dependency-set (@ pattern))
       (optional-expr-dependency-set (@ condition))
       (expr-dependency-set (@ scheme)))]))

(define (clique-dependency-set stx)
  (syntax-parse stx
    [:optional/none-expr (id-set)]
    [:option/clique-expr (refs-dependency-set (@ fun))]
    [:option/controllers-expr (refs-dependency-set (@ fun))]))

(define (optional-expr-dependency-set stx)
  (syntax-parse stx
    [:optional/some-expr (expr-dependency-set (@ arg))]
    [:optional/none-expr (id-set)]))

(define (description-dependency-set desc)
  (syntax-parse desc
    [:refine-expr
     (id-set-union
       (description-dependency-set (@ base))
       (refs-dependency-set (@ field))
       (refs-dependency-set (@ defn)))]
    [ref (ref-dependency-set (@ ref))]))

(define (component-dependency-set comp)
  (syntax-parse comp
    [:component-expr
     (id-set-union
       (description-dependency-set (@ desc))
       (id-set-difference
         (defns-dependency-set (@ body))
         (apply id-set (@ from-desc))
         (apply id-set (@ from-comp))))]))

(define (exprs-dependency-set exprs)
  (apply id-set-union
    (map expr-dependency-set exprs)))

(define (expr-dependency-set expr)
  (syntax-parse expr
    [:var-expr (id-set)]
    [:quote-expr (id-set)]
    [:if-expr
     (id-set-union
       (expr-dependency-set (@ test))
       (expr-dependency-set (@ then))
       (expr-dependency-set (@ else)))]
    [:let-expr
     (id-set-union
       (exprs-dependency-set (@ rhs))
       (expr-dependency-set (@ body)))]
    [:app-expr
     (id-set-union
       (ref-dependency-set (@ fun))
       (exprs-dependency-set (@ arg)))]))

(define (refs-dependency-set refs)
  (apply id-set-union
    (map ref-dependency-set refs)))

(define (ref-dependency-set ref)
  (syntax-parse ref
    [:var-expr (id-set (@ name))]
    [:deref-expr
     (id-set-union
       (ref-dependency-set (@ comp))
       (ref-dependency-set (@ field)))]))

(define id-set
  (case-lambda
    [() empty-id-set]
    [ids
     (make-immutable-free-id-table
       (make-alist ids #true))]))
(define empty-id-set
  (make-immutable-free-id-table))
(define (id-set-union . sets)
  (apply dict-add empty-id-set sets
    #:combine (const #true)))
(define (id-set-difference set . sets)
  (apply dict-subtract set sets))

(define (sort-defns defns)

  (define (defn->index defn)
    (dict-ref defn~>index defn impossible))
  (define defn~>index
    (for/hasheq {[defn (in-list defns)]
                 [index (in-naturals)]}
      (values defn index)))

  (define (defined? name)
    (dict-has-key? name~>defn name))
  (define (name->defn name)
    (dict-ref name~>defn name impossible))
  (define name~>defn
    (make-immutable-free-id-table
      (for*/list {[defn (in-list defns)]
                  [name (in-list (defn-bindings defn))]}
        (cons name defn))))

  (define (defn->predecessors defn)
    (dict-ref defn~>predecessors defn impossible))
  (define defn~>predecessors
    (for/hasheq {[defn (in-list defns)]}
      (define predecessor-set
        (for/seteq {[name (in-list (defn-dependencies defn))]
                    #:when (defined? name)}
          (name->defn name)))
      (define predecessors
        (sort (set->list predecessor-set) < #:key defn->index))
      (values defn predecessors)))

  (define (mutual-recursion cycle)
    (wrong-syntax (first cycle)
      "mutually recursive definitions: ~a"
      (list->phrase #:separator ";"
        (for/list {[defn (in-list cycle)]}
          (format "~a at ~a"
            (list->phrase #:none "(no bindings)"
              (for/list {[name (in-list (defn-bindings defn))]}
                (symbol->string (syntax-e name))))
            (source-location->string defn "(unknown location)"))))))

  (topological-sort defns defn->predecessors
    #:cycle mutual-recursion))
