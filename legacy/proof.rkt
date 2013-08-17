#lang mischief

(provide
  program-obligations
  program-consequences)

(require
  dracula/legacy/type
  dracula/expansion/grammar
  dracula/legacy/registry
  dracula/legacy/unique
  dracula/legacy/ref
  dracula/legacy/rename
  dracula/expansion/dependency
  dracula/proof/term
  dracula/expansion/paths)

(define (program-obligations stxs)
  (define deps (all-dependencies))
  (assign-unique-names
    (append
      (dependencies-obligations deps)
      (dependencies-consequences deps)
      (defns-obligations stxs))))

(define (program-consequences stxs)
  (defns-consequences stxs))

(define (all-dependencies)
  (remove-duplicates
    (queue->list dependency-queue)))

(define (dependencies-obligations deps)
  (append-map dependency-obligations deps))

(define (dependency-obligations dep)
  (make-encapsulate* (dependency-term dep)))

(define (dependencies-consequences deps)
  (append-map dependency-consequences deps))

(define (dependency-consequences dep)
  (dict-ref module-name~>consequences dep
    (lambda ()
      (error 'program-obligations
        "no proof obligation recorded for module dependency ~a"
        dep))))

(define (defns-obligations stxs)
  (append-map defn-obligations
    (sort-defns stxs)))

(define (defns-consequences stxs)
  (append-map defn-consequences
    (sort-defns stxs)))

(define (defn-obligations stx)
  (syntax-parse stx
    [:not-defn '()]
    [:begin-for-syntax-defn '()]
    [:require-defn '()]
    [:provide-defn '()]
    [:syntax-defn '()]
    [:description-defn '()]
    [:component-decl '()]
    [:generic-decl '()]
    [:function-decl '()]
    [:theorem-decl '()]
    [:in-theory-defn
     (list (in-theory-term (@ theory)))]
    [:book-defn
     (list (include-book-term (@ path) (@ option)))]
    [:primitive-defn
     (list
       (make-assert
         (make-if
           (make-quote #true)
           (make-quote #true)
           (make-app
             (::
               (syntax-e (@ sym-package))
               (syntax-e (@ sym-name)))
             (build-list (syntax-e (@ arity))
               (lambda (i)
                 (make-quote (add1 i))))))))]
    [:function-defn
     (list (defun-term (@ name) (@ formal) (@ measure) (@ body)))]
    [:theorem-defn
     (list (defthm-term (@ name) (@ body) (@ rule-class) (@ goal)))]
    [:component-defn
     (define comp-ty (dict-ref identifier~>type (@ name) impossible))
     (define desc-ty (component-type-description comp-ty))
     (apply make-progn*
       (append
         (apply make-encapsulate*
           (component-obligations desc-ty (@ body)))
         (component-consequences (@ name) desc-ty)))]
    [:generic-defn
     (define gen-ty (dict-ref identifier~>type (@ name) impossible))
     (define domain-ty (generic-type-domain gen-ty))
     (define formal (generic-type-formal gen-ty))
     (define range-ty (generic-type-range gen-ty))
     (apply make-encapsulate*
       (append
         (component-consequences formal domain-ty)
         (component-obligations range-ty (@ body))))]
    [:instance-defn
     (define comp-ty (dict-ref identifier~>type (@ name) impossible))
     (define desc-ty (component-type-description comp-ty))
     (component-consequences (@ name) desc-ty)]))

(define (component-obligations desc-ty stx)
  (match! desc-ty
    [(description-type _ fields field~>role field~>type field~>name defns)
     (syntax-parse stx
       [:component-expr

        (define-values {decl-fields defn-fields}
          (partition
            (lambda (field)
              (eq? (dict-ref field~>role field impossible) 'decl))
            fields))
        (define field~>comp-name
          (make-immutable-free-id-table
            (append
              (map cons decl-fields (@ from-comp))
              (map cons defn-fields (@ from-desc)))))
        (define renaming
          (make-immutable-free-id-table
            (for/list {[field (in-list fields)]}
              (cons
                (dict-ref field~>name field impossible)
                (dict-ref field~>comp-name field impossible)))))

        (define auto-defns
          (rename-defns
            (remove-declarations defns)
            renaming))

        (defns-obligations
          (append (@ body) auto-defns))])]))

(define (remove-declarations stxs)
  (filter-not declaration? stxs))

(define (declaration? stx)
  (syntax-parse stx
    [:generic-decl #true]
    [:component-decl #true]
    [:function-decl #true]
    [:theorem-decl #true]
    [_ #false]))

(define (defn-consequences stx)
  (syntax-parse stx
    [:not-defn '()]
    [:begin-for-syntax-defn '()]
    [:require-defn '()]
    [:provide-defn '()]
    [:syntax-defn '()]
    [:description-defn '()]
    [:primitive-defn '()]
    [:generic-defn '()]
    [:generic-decl '()]
    [:in-theory-defn
     (list (in-theory-term (@ theory)))]
    [:book-defn
     (list (include-book-term (@ path) (@ option)))]
    [:function-defn
     (list
       (make-skip-proofs
         (defun-term (@ name) (@ formal) (@ measure) (@ body))))]
    [:function-decl
     (list
       (defstub-term (@ name) (syntax-e (@ arity))))]
    [:theorem-defn
     (list
       (make-skip-proofs
         (defthm-term (@ name) (@ body) (@ rule-class) (@ goal))))]
    [:theorem-decl
     (list
       (make-skip-proofs
         (defthm-term (@ name) (@ body) (@ rule-class) #false)))]
    [:component-defn
     (define comp-ty (dict-ref identifier~>type (@ name) impossible))
     (define desc-ty (component-type-description comp-ty))
     (component-consequences (@ name) desc-ty)]
    [:component-decl
     (define comp-ty (dict-ref identifier~>type (@ name) impossible))
     (define desc-ty (component-type-description comp-ty))
     (component-consequences (@ name) desc-ty)]
    [:instance-defn
     (define comp-ty (dict-ref identifier~>type (@ name) impossible))
     (define desc-ty (component-type-description comp-ty))
     (component-consequences (@ name) desc-ty)]))

(define (component-consequences comp-id desc-ty)
  (define cseqs
    (defns-consequences
      (description-type-defns desc-ty)))
  (define fields (description-type-fields desc-ty))
  (define field~>role (description-type-field~>role desc-ty))
  (define field~>type (description-type-field~>type desc-ty))
  (define field~>name (description-type-field~>name desc-ty))
  (define renaming
    (make-immutable-free-id-table
      (for/list {[field (in-list fields)]}
        (define name (dict-ref field~>name field impossible))
        (define role (dict-ref field~>role field impossible))
        (define target
          (match! role
            [(or 'defn 'decl)
             (make-deref-expr comp-id field)]
            ['refine
             (define type (dict-ref field~>type field impossible))
             (type-ref type)]))
        (cons name target))))
  (rename-pf cseqs renaming))

(define (rename-pf pf renaming)
  (cond!
    [(syntax? pf)
     (rename-ref pf renaming)]
    [(cons? pf)
     (cons
       (rename-pf (car pf) renaming)
       (rename-pf (cdr pf) renaming))]
    [else pf]))

(define (defun-term name-id formal-ids measure-stx/f body-stx)
  (make-defun
    name-id
    formal-ids
    (and measure-stx/f
      (expr-term measure-stx/f))
    (expr-term body-stx)))

(define (defstub-term name-id arity)
  (make-defstub
    name-id
    arity))

(define (defthm-term name-id body-stx rule-class-stxs/f goal-stxs/f)
  (make-defthm
    name-id
    (expr-term body-stx)
    (and rule-class-stxs/f
      (map rule-class-term rule-class-stxs/f))
    (and goal-stxs/f
      (map goal-term goal-stxs/f))))

(define (in-theory-term theory-stx)
  (make-in-theory
    (theory-expr-term theory-stx)))

(define (include-book-term path-stx option-stxs)
  (make-include-book
    (syntax-e path-stx)
    (append-map book-option-terms option-stxs)))

(define (book-option-terms opt)
  (syntax-parse opt
    [:book-dir-expr
     (list '#:DIR (keyword-upcase (syntax-e (@ dir))))]))

(define (dependency-term name)
  (make-include-book
    (dependency->string name)
    (append
      (dependency-options name)
      '(#:SKIP-PROOFS-OKP #s{:: ACL2 T}))))

(define (dependency->string name)
  (cond!
    [(path? name)
     (path->book-name name)]
    [(symbol? name) (symbol->string name)]))

(define (dependency-options name)
  (cond!
    [(path? name) '()]
    [(symbol? name) '(#:DIR #:DYNAMIC)]))

(define (rule-class-term stx)
  (syntax-parse stx
    [:rule/rewrite-expr
     `{#:REWRITE
       ,@(optional-expr-terms (@ corollary) '#:COROLLARY)}]
    [:rule/forward-chaining-expr
     `{#:FORWARD-CHAINING
       ,@(optional-expr-terms (@ corollary) '#:COROLLARY)}]
    [:rule/elim-expr
     `{#:ELIM
       ,@(optional-expr-terms (@ corollary) '#:COROLLARY)}]
    [:rule/type-prescription-expr
     `{#:TYPE-PRESCRIPTION
       ,@(optional-expr-terms (@ corollary) '#:COROLLARY)}]
    [:rule/definition-expr
     `{#:DEFINITION
       ,@(optional-expr-terms (@ corollary) '#:COROLLARY)
       ,@(clique-terms (@ clique))}]
    [:rule/induction-expr
     `{#:INDUCTION
       ,@(optional-expr-terms (@ corollary) '#:COROLLARY)
       #:PATTERN ,(expr-term (@ pattern))
       ,@(optional-expr-terms (@ condition) '#:CONDITION)
       #:SCHEME ,(expr-term (@ scheme))}]))

(define (clique-terms stx)
  (syntax-parse stx
    [:optional/none-expr '()]
    [:option/clique-expr
     (list '#:CLIQUE (map ref-term (@ fun)))]
    [:option/controllers-expr
     (list
       '#:CLIQUE
       (map ref-term (@ fun))
       '#:CONTROLLER-ALIST
       (for/list {[f (in-list (@ fun))]
                  [xs (in-list (@ control))]}
         (make-app f
           (for/list {[x (in-list xs)]}
             (if (syntax-e x)
               (make-t)
               (make-nil))))))]))

(define (optional-expr-terms stx . prefix)
  (syntax-parse stx
    [:optional/some-expr (append prefix (list (expr-term (@ arg))))]
    [:optional/none-expr '()]))

(define (goal-term stx)
  (syntax-parse stx
    [:goal-expr
     (cons (syntax-e (@ name))
       (append-map hint-terms (@ hint)))]))

(define (hint-terms stx)
  (syntax-parse stx
    [:hint/by-expr
     (list '#:BY
       (cond!
         [(@ inst) (theorem-instance-term (@ inst))]
         [else (make-nil)]))]
    [:hint/use-expr
     (list '#:USE
       (map theorem-instance-term (@ inst)))]
    [:hint/in-theory-expr
     (list '#:IN-THEORY
       (theory-expr-term (@ theory)))]
    [:hint/induct-expr
     (list '#:INDUCT
       (expr-term (@ scheme)))]))

(define (theorem-instance-term stx)
  (syntax-parse stx
    [:app-expr
     (define formals
       (theorem-type-formals
         (ref-type (@ fun))))
     (list*
       '#:INSTANCE
       (ref-term (@ fun))
       (map list
         formals
         (map expr-term (@ arg))))]))

(define (theory-expr-term stx)
  (syntax-parse stx
    [:disable-expr
     (make-disable
       (map ref-term (@ rune)))]
    [:enable-expr
     (make-enable
       (map ref-term (@ rune)))]))

(define (expr-term stx)
  (syntax-parse stx
    [:quote-expr
     (make-quote
       (to-datum (@ value)))]
    [:if-expr
     (make-if
       (expr-term (@ test))
       (expr-term (@ then))
       (expr-term (@ else)))]
    [:let-expr
     (make-let
       (@ lhs)
       (map expr-term (@ rhs))
       (expr-term (@ body)))]
    [:app-expr
     (make-app
       (fun-term (@ fun))
       (map expr-term (@ arg)))]
    [:var-expr
     (@ name)]))

(define (fun-term stx)
  (ref-term stx))

(define (ref-term stx)
  (type-ref (ref-type stx)))
