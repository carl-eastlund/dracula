#lang mischief

(provide
  (contract-out
    [rename-defns (-> (listof syntax?) renaming/c (listof syntax?))]
    [rename-type (-> type? renaming/c type?)]
    [rename-field~>type
     (->
       (dict/c identifier? type?)
       renaming/c
       (dict/c identifier? type?))]))

(require
  dracula/legacy/ref
  dracula/expansion/grammar
  dracula/legacy/type)

(define (rename-type ty renaming)
  (define ref (rename-ref (type-ref ty) renaming))
  (match! ty
    [(value-type _) (value-type ref)]
    [(function-type _ arity prim) (function-type ref arity prim)]
    [(theorem-type _ formals body rule-classes)
     (theorem-type ref formals
       (rename-expr body renaming)
       (rename-optional rename-rule-classes rule-classes renaming))]
    [(field-type _ field) (field-type ref field)]
    [(description-type _
       fields
       field~>role
       field~>type0
       field~>name
       defns)
     (description-type ref
       fields
       field~>role
       (rename-field~>type field~>type0 renaming)
       field~>name
       defns)]
    [(component-type _ desc-ty field~>type0)
     (component-type ref
       (rename-type desc-ty renaming)
       (rename-field~>type field~>type0 renaming))]
    [(generic-type _ domain-ty formal range-ty)
     (generic-type ref
       (rename-type domain-ty renaming)
       formal
       (rename-type range-ty renaming))]))

(define (rename-field~>type field~>type renaming)
  (make-immutable-free-id-table
    (for/list {[(field type) (in-dict field~>type)]}
      (cons field
        (rename-type type renaming)))))

(define (rename-defns stxs table)
  (rename-list rename-defn stxs table))

(define (rename-defn stx table)
  (syntax-parse stx
    [:begin-for-syntax-defn stx]
    [:require-defn stx]
    [:provide-defn stx]
    [:syntax-defn stx]
    [:in-theory-defn
     (make-in-theory-defn #:source stx
       (rename-theory-expr (@ theory) table))]
    [:book-defn
     (make-book-defn #:source stx
       (rename-book-options (@ option) table))]
    [:description-defn
     (make-description-defn #:source stx
       (rename-id (@ name) table)
       (@ decls)
       (@ defns)
       (rename-ids (@ field-name) table)
       (@ decl-name) ;; locally scoped
       (@ defn-name) ;; locally scoped
       (rename-defns (@ body) table))]
    [:generic-decl
     (make-generic-decl #:source stx
       (rename-id (@ name) table)
       (rename-desc (@ domain) table)
       (@ formal) ;; locally scoped
       (rename-desc (@ range) table))]
    [:generic-defn
     (make-generic-defn #:source stx
       (rename-id (@ name) table)
       (rename-desc (@ domain) table)
       (@ formal) ;; locally scoped
       (rename-component (@ body) table))]
    [:component-decl
     (make-component-decl #:source stx
       (rename-id (@ name) table)
       (rename-desc (@ desc) table))]
    [:component-defn
     (make-component-defn #:source stx
       (rename-id (@ name) table)
       (rename-component (@ body) table))]
    [:instance-defn
     (make-instance-defn #:source stx
       (rename-id (@ name) table)
       (rename-ref/syntax (@ gen) table)
       (rename-ref/syntax (@ arg) table))]
    [:primitive-defn
     (make-primitive-defn #:source stx
       (rename-id (@ prim-name) table)
       (@ arity)
       (@ sym-package)
       (@ sym-name))]
    [:function-decl
     (make-function-decl #:source stx
       (rename-id (@ name) table)
       (@ arity))]
    [:function-defn
     (make-function-defn #:source stx
       (rename-id (@ name) table)
       (@ formal)
       (and (@ measure)
         (rename-expr (@ measure) table))
       (rename-expr (@ body) table))]
    [:theorem-decl
     (make-theorem-decl #:source stx
       (rename-id (@ name) table)
       (@ formal)
       (and (@ rule-class)
         (rename-rule-classes (@ rule-class) table))
       (rename-expr (@ body) table))]
    [:theorem-defn
     (make-theorem-defn #:source stx
       (rename-id (@ name) table)
       (@ formal)
       (and (@ rule-class)
         (rename-rule-classes (@ rule-class) table))
       (and (@ goal)
         (rename-goals (@ goal) table))
       (rename-expr (@ body) table))]))

(define (rename-book-options options table)
  (rename-list rename-book-option options table))

(define (rename-book-option stx table)
  (syntax-parse stx
    [:book-dir-expr stx]))

(define (rename-goals goals table)
  (rename-list rename-goal goals table))

(define (rename-goal goal table)
  (syntax-parse goal
    [:goal-expr
     (make-goal-expr #:source goal
       (@ name)
       (rename-hints (@ hint) table))]))

(define (rename-hints hints table)
  (rename-list rename-hint hints table))

(define (rename-hint hint table)
  (syntax-parse hint
    [:hint/by-expr
     (make-hint/by-expr #:source hint
       (and (@ inst)
         (rename-theorem-instance (@ inst) table)))]
    [:hint/use-expr
     (make-hint/use-expr #:source hint
       (rename-theorem-instances (@ inst) table))]
    [:hint/in-theory-expr
     (make-hint/in-theory-expr #:source hint
       (rename-theory-expr (@ theory) table))]
    [:hint/induct-expr
     (make-hint/induct-expr #:source hint
       (rename-expr (@ scheme) table))]))

(define (rename-theorem-instances stxs table)
  (rename-list rename-theorem-instance stxs table))

(define (rename-theorem-instance stx table)
  (syntax-parse stx
    [:app-expr
     (make-app-expr #:source stx
       (rename-ref/syntax (@ fun) table)
       (rename-exprs (@ arg) table))]))

(define (rename-theory-expr stx table)
  (syntax-parse stx
    [:disable-expr
     (make-disable-expr #:source stx
       (rename-refs (@ rune) table))]
    [:enable-expr
     (make-enable-expr #:source stx
       (rename-refs (@ rune) table))]))

(define (rename-rule-classes stxs table)
  (rename-list rename-rule-class stxs table))

(define (rename-rule-class stx table)
  (syntax-parse stx
    [:rule/rewrite-expr
     (make-rule/rewrite-expr #:source stx
       (rename-optional-expr (@ corollary) table))]
    [:rule/forward-chaining-expr
     (make-rule/forward-chaining-expr #:source stx
       (rename-optional-expr (@ corollary) table))]
    [:rule/elim-expr
     (make-rule/elim-expr #:source stx
       (rename-optional-expr (@ corollary) table))]
    [:rule/type-prescription-expr
     (make-rule/type-prescription-expr #:source stx
       (rename-optional-expr (@ corollary) table))]
    [:rule/definition-expr
     (make-rule/definition-expr #:source stx
       (rename-optional-expr (@ corollary) table)
       (rename-clique (@ clique) table))]
    [:rule/induction-expr
     (make-rule/induction-expr #:source stx
       (rename-optional-expr (@ corollary) table)
       (rename-expr (@ pattern) table)
       (rename-optional-expr (@ condition) table)
       (rename-expr (@ scheme) table))]))

(define (rename-clique stx table)
  (syntax-parse stx
    [:optional/none-expr
     (make-optional/none-expr #:source stx)]
    [:option/clique-expr
     (make-option/clique-expr #:source stx
       (rename-refs (@ fun) table))]
    [:option/controllers-expr
     (make-option/controllers-expr #:source stx
       (rename-refs (@ fun) table)
       (@ control))]))

(define (rename-optional-expr stx table)
  (syntax-parse stx
    [:optional/some-expr
     (make-optional/some-expr #:source stx
       (rename-expr (@ arg) table))]
    [:optional/none-expr
     (make-optional/none-expr #:source stx)]))

(define (rename-component stx table)
  (syntax-parse stx
    [:component-expr
     (make-component-expr #:source stx
       (rename-desc (@ desc) table)
       (@ from-desc) ;; locally scoped
       (@ from-comp) ;; locally scoped
       (rename-defns (@ body) table))]))

(define (rename-desc stx table)
  (syntax-parse stx
    [:refine-expr
     (make-refine-expr #:source stx
       (rename-desc (@ base) table)
       (rename-refs (@ field) table)
       (rename-refs (@ defn) table))]
    [ref (rename-ref/syntax (@ ref) table)]))

(define (rename-refs stxs table)
  (rename-list rename-ref/syntax stxs table))

(define (rename-exprs stxs table)
  (rename-list rename-expr stxs table))

(define (rename-expr stx table)
  (syntax-parse stx
    [:var-expr stx]
    [:quote-expr stx]
    [:if-expr
     (make-if-expr #:source stx
       (rename-expr (@ test) table)
       (rename-expr (@ then) table)
       (rename-expr (@ else) table))]
    [:let-expr
     (make-let-expr #:source stx
       (@ lhs) ;; locally scoped
       (rename-exprs (@ rhs) table)
       (rename-expr (@ body) table))]
    [:app-expr
     (make-app-expr #:source stx
       (rename-ref/syntax (@ fun) table)
       (rename-exprs (@ arg) table))]))

(define (rename-ids stx table)
  (rename-list rename-id stx table))

(define (rename-id stx table)
  (dict-ref table stx stx))

(define (rename-ref/syntax stx table)
  (rename-ref stx table))

(define (rename-list f stxs table)
  (for/list {[stx (in-list stxs)]}
    (f stx table)))

(define (rename-optional f opt table)
  (and opt
    (f opt table)))
