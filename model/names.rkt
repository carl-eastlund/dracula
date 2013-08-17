#lang mischief

(provide
  program-bindings
  defns-bindings
  defn-bindings
  environment-bindings
  decls-bindings
  decl-bindings
  program-references
  defns-references
  defn-references
  environment-references
  decls-references
  decl-references
  sort-defns
  sort-decls)

(require
  dracula/model/data)

(define (program-bindings x) (set->list (program-binding-set x)))
(define (defns-bindings x) (set->list (multiple defn-binding-set x)))
(define (defn-bindings x) (set->list (defn-binding-set x)))
(define (environment-bindings x) (set->list (environment-binding-set x)))
(define (decls-bindings x) (set->list (multiple decl-binding-set x)))
(define (decl-bindings x) (set->list (decl-binding-set x)))
(define (program-references x) (set->list (program-reference-set x)))
(define (defns-references x) (set->list (multiple defn-reference-set x)))
(define (defn-references x) (set->list (defn-reference-set x)))
(define (environment-references x) (set->list (environment-reference-set x)))
(define (decls-references x) (set->list (multiple decl-reference-set x)))
(define (decl-references x) (set->list (decl-reference-set x)))

(define (program-binding-set p)
  (multiple defn-binding-set (program-defns p)))

(define (defn-binding-set d)
  (match! d
    [(defn _ name _) (set name)]
    [(in-theory _ _) (set)]
    [(include-book _ _ _) (set)]
    [(primitive _ name _ _ _) (set name)]
    [(encapsulate _ _) (set)]
    [(skip-proofs _ defns) (multiple defn-binding-set defns)]
    [(assert-event _ _) (set)]
    [(validate _ _ _ _) (set)]
    [_ (set)]))

(define (environment-binding-set e)
  (multiple decl-binding-set (environment-decls e)))

(define (decl-binding-set d)
  (match! d
    [(decl _ name _) (set name)]))

(define (program-reference-set p)
  (multiple defn-reference-set (program-defns p)))

(define (defn-reference-set d)
  (match! d
    [(defn _ name term) (set-remove (term-reference-set term) name)]
    [(in-theory _ theory) (theory-reference-set theory)]
    [(include-book _ _ _) (set)]
    [(primitive _ _ _ _ _) (set)]
    [(encapsulate _ defns) (multiple defn-reference-set defns)]
    [(skip-proofs _ defns) (multiple defn-reference-set defns)]
    [(assert-event _ e) (expr-reference-set e)]
    [(validate _ f _ _) (addr-reference-set f)]
    [_ (expr-reference-set d)]))

(define (term-reference-set term)
  (match! term
    [(stub-term _ _) (set)]
    [(fun-term _ formals body measure goals)
     (set-subtract
       (set-union
         (expr-reference-set body)
         (optional expr-reference-set measure)
         (optional (arg+ multiple goal-reference-set) goals))
       (list->set formals))]
    [(thm-term _ formals body rules goals)
     (set-subtract
       (set-union
         (expr-reference-set body)
         (optional (arg+ multiple rule-reference-set) rules)
         (optional (arg+ multiple goal-reference-set) goals))
       (list->set formals))]
    [(type-term _ type)
     (type-reference-set type)]
    [(seal-term _ type term)
     (set-union
       (type-reference-set type)
       (term-reference-set term))]
    [(inst-term _ labels names program)
     (set-subtract
       (program-reference-set program)
       (list->set names))]
    [(gen-term _ formals domains term)
     (set-union
       (multiple type-reference-set domains)
       (set-subtract
         (term-reference-set term)
         (list->set formals)))]
    [(app-term _ gen args)
     (set-union (addr-reference-set gen)
       (multiple term-reference-set args))]
    [_ (addr-reference-set term)]))

(define (environment-reference-set e)
  (multiple decl-reference-set (environment-decls e)))

(define (decl-reference-set d)
  (match! d
    [(decl _ name type) (set-remove (type-reference-set type) name)]))

(define (type-reference-set type)
  (match! type
    [(value-type _) (set)]
    [(stub-type _ _) (set)]
    [(fun-type _ formals body measure)
     (set-subtract
       (set-union
         (expr-reference-set body)
         (optional expr-reference-set measure))
       (list->set formals))]
    [(thm-type _ formals body rules)
     (set-subtract
       (set-union
         (expr-reference-set body)
         (optional (arg+ multiple rule-reference-set) rules))
       (list->set formals))]
    [(type-type _ type)
     (type-reference-set type)]
    [(inst-type _ labels names environment)
     (set-subtract
       (environment-reference-set environment)
       (list->set names))]
    [(gen-type _ formals domains type)
     (set-union
       (multiple type-reference-set domains)
       (set-subtract
         (type-reference-set type)
         (list->set formals)))]
    [(refine-type _ base labels addr)
     (set-union
       (type-reference-set base)
       (addr-reference-set addr))]
    [(addr-type _ addr)
     (addr-reference-set addr)]
    [_ (addr-reference-set type)]))

(define (theory-reference-set th)
  (match! th
    [(enable _ runes) (multiple addr-reference-set runes)]
    [(disable _ runes) (multiple addr-reference-set runes)]))

(define (rule-reference-set r)
  (match! r
    [(rewrite _ cor) (optional expr-reference-set cor)]
    [(forward-chaining _ cor) (optional expr-reference-set cor)]
    [(elim _ cor) (optional expr-reference-set cor)]
    [(type-prescription _ cor) (optional expr-reference-set cor)]
    [(linear _ cor) (optional expr-reference-set cor)]
    [(definition _ cor cli)
     (set-union
       (optional expr-reference-set cor)
       (optional clique-reference-set cli))]
    [(induction _ cor pat con sch)
     (set-union
       (optional expr-reference-set cor)
       (expr-reference-set pat)
       (optional expr-reference-set con)
       (expr-reference-set sch))]))

(define (clique-reference-set c)
  (match! c
    [(clique _ funs) (multiple addr-reference-set funs)]
    [(controllers _ funs _) (multiple addr-reference-set funs)]))

(define (goal-reference-set g)
  (match! g
    [(goal _ _ hints) (multiple hint-reference-set hints)]))

(define (hint-reference-set h)
  (match! h
    [(by-hint _ lem) (optional lemma-reference-set lem)]
    [(use-hint _ lems) (multiple lemma-reference-set lems)]
    [(in-theory-hint _ th) (theory-reference-set th)]
    [(induct-hint _ sch) (expr-reference-set sch)]))

(define (lemma-reference-set lem)
  (match! lem
    [(lemma _ rune args)
     (set-union
       (addr-reference-set rune)
       (multiple actual-reference-set args))]))

(define (actual-reference-set arg)
  (match! arg
    [(named-actual _ _ e) (expr-reference-set e)]
    [_ (expr-reference-set arg)]))

(define (expr-reference-set expr)
  (match! expr
    [(qu-expr _ _) (set)]
    [(if-expr _ Q A E)
     (set-union
       (expr-reference-set Q)
       (expr-reference-set A)
       (expr-reference-set E))]
    [(let-expr _ lhs rhs body)
     (set-union
       (multiple expr-reference-set rhs)
       (set-subtract
         (expr-reference-set body)
         (list->set lhs)))]
    [(app-expr _ fun args)
     (set-union
       (addr-reference-set fun)
       (multiple expr-reference-set args))]
    [_ (var-reference-set expr)]))

(define (addr-reference-set addr)
  (match! addr
    [(deref _ base label) (addr-reference-set base)]
    [_ (var-reference-set addr)]))

(define (var-reference-set v)
  (match! v
    [(var _ _) (set v)]
    [(:: _ _) (set v)]))

(define (multiple get-set xs)
  (apply set-union (set) (map get-set xs)))

(define (optional get-set x)
  (if x (get-set x) (set)))

(define (sort-defns ds)
  (sort-by-names ds defn-binding-set defn-reference-set))

(define (sort-decls ds)
  (sort-by-names ds decl-binding-set decl-reference-set))

(define (sort-by-names xs binds refs)

  (define elems (list->vector xs))

  (define n (vector-length elems))

  (define name-sets
    (for/vector #:length n {[elem (in-vector elems)]}
      (binds elem)))

  (define name~>index
    (for*/hash! {[{name-set index} (in-indexed (in-vector name-sets))]
                 [name (in-set name-set)]}
      (values name index)))

  (define preds
    (for/vector #:length n {[{elem index} (in-indexed (in-vector elems))]}
      (for/set {[name (in-set (refs elem))]
                #:when (hash-has-key? name~>index name)}
        (hash-ref name~>index name))))

  (define queue (make-queue))
  (define status (make-vector n 'unseen))
  (define stack '())

  (define (visit i)
    (match! (vector-ref status i)
      ['seen (void)]
      ['unseen
       (vector-set! status i 'visiting)
       (set! stack (cons i stack))
       (for {[pred (in-set (vector-ref preds i))]}
         (visit pred))
       (set! stack (rest stack))
       (vector-set! status i 'seen)
       (enqueue! queue i)]
      ['visiting
       (define cycle
         (for/append {[j (in-list stack)]}
           #:final (= i j)
           (set->list (vector-ref name-sets j))))
       (syntax-error (source-of (vector-ref elems i))
         "cycle found among mutually recursive definitions of ~a"
         (vars->string cycle))]))

  (for {[i (in-range n)]}
    (visit i))

  (for/list {[i (in-queue queue)]}
    (vector-ref elems i)))
