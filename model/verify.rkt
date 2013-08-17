#lang mischief

(provide
  verify-program
  verify-dependency)

(require
  dracula/proof/term
  dracula/expansion/paths
  dracula/model/data
  dracula/model/subst
  (for-syntax mischief))

(define (verify-dependency dep)
  (define-values {base subs} (split-module-name dep))
  (define dep-options
    (if (symbol? base)
      (list (book-dir #false '#:dynamic))
      '()))
  (define options
    (cons (skip-proofs-ok #false #true) dep-options))
  (define book (book-name base subs))
  (list
    (encapsulate #false
      (list (include-book #false book options)))))

(define (split-module-name name)
  (match! name
    [(list* base subs) (values base subs)]
    [base (values base '())]))

(define (book-name base subs)
  (string-join
    (cons (book-base-name base)
      (map symbol->string subs))
    ":"))

(define (book-base-name base)
  (match! base
    [(? symbol?) (symbol->string base)]
    [(? path?) (book-path-without-extension base)]))

(define current-context-stack (make-parameter '()))

(define-shorthand (with-context-frame ctx:expr body:expr ...+)
  (parameterize
      {[current-context-stack
        (cons (lambda {} ctx)
          (current-context-stack))]}
    body ...))

(define (verify-program env shapes prog)
  (match! prog
    [(program defns)
     (verify-defns env shapes defns)]))

(define (verify-environment env env0)
  (match! env0
    [(environment decls)
     (verify-decls env decls)]))

(define (verify-defns env shapes defns)
  (for/fold/append-lists
      {[env env] [shapes shapes]}
      {obs csqs}
      {[d (in-list defns)]}
    (verify-defn env shapes d)))

(define (verify-decls env decls)
  (for/fold
      {[env env]}
      {[d (in-list decls)]}
    (verify-decl env d)))

(define (verify-defn env shapes d)
  (parameterize {[current-syntax-context (source-of d)]}
    (match! d
      [(defn src name term)
       (with-context-frame (format "definition of ~a" (var->string name))
         (match! term
           [(fun-term src2 formals body measure goals)
            (define env+formals (bind-values formals env))
            (define rec-ty (stub-type src2 (length formals)))
            (define env+formals+rec (bind-type name rec-ty env+formals))
            (check-expr! env+formals+rec body)
            (when measure (check-expr! env+formals measure))
            (when goals (check-goals! env+formals goals))
            (define shape (rune-shape src name formals))
            (define shapes+formals (bind-value-shapes formals shapes))
            (define shapes+formals+rec (bind-shape name shape shapes+formals))
            (define fun-pf
              (list
                (defn src name
                  (fun-term src2 formals
                    (expr-pf shapes+formals+rec body)
                    (and measure (expr-pf shapes+formals measure))
                    (and goals (goals-pf shapes+formals goals))))))
            (define fun-ty (fun-type src2 formals body measure))
            (values
              (bind-type name fun-ty env)
              (bind-shape name shape shapes)
              fun-pf
              (list (skip-proofs src fun-pf)))]
           [(thm-term src2 formals body rules goals)
            (define env+formals (bind-values formals env))
            (check-expr! env+formals body)
            (when rules (check-rules! env+formals rules))
            (when goals (check-goals! env+formals goals))
            (define shapes+formals (bind-value-shapes formals shapes))
            (define thm-pf
              (list
                (defn src name
                  (thm-term src2 formals
                    (expr-pf shapes+formals body)
                    (and rules (rules-pf shapes+formals rules))
                    (and goals (goals-pf shapes+formals goals))))))
            (define thm-ty (thm-type src2 formals body rules))
            (define shape (rune-shape src name formals))
            (values
              (bind-type name thm-ty env)
              (bind-shape name shape shapes)
              thm-pf
              (list (skip-proofs src thm-pf)))]
           [_
            (define-values {term-ty term-shape term-obs term-csqs}
              (verify-term env shapes term))
            (define sub
              (prefix-substitution name
                (remove-duplicates
                  (append
                    (defns-domain term-obs)
                    (defns-domain term-csqs)))))
            (values
              (bind-type name term-ty env)
              (bind-shape name (subst-shape sub term-shape) shapes)
              (subst-defns sub term-obs)
              (subst-defns sub term-csqs))]))]
      [(assert-event src e)
       (check-expr! env e)
       (define pf (list (assert-event src (expr-pf shapes e))))
       (values env shapes pf '())]
      [(validate src fun inputs outputs)
       (define-values {fun-ty fun-shape} (verify-addr env shapes fun))
       (check-function-type! fun-ty)
       (define arity (type-arity fun-ty))
       (when (empty? inputs)
         (context-error (source-of src)
           "must validate at least one set of input and output values"))
       (define ilen (length inputs))
       (define olen (length outputs))
       (unless (= ilen olen)
         (context-error (source-of src)
           "got ~a to validate for ~a of input values"
           (count->phrase olen "output value")
           (count->phrase ilen "set")))
       (for {[ins (in-list inputs)]
             [i (in-naturals 0)]}
         (define n (length ins))
         (unless (= n arity)
           (context-error (source-of src)
             "input / output set ~a: got ~a for a function of ~a"
             i
             (count->phrase n "input value")
             (count->phrase arity "argument"))))
       (define fun-pf (rune-shape-name fun-shape))
       (define assertions
         (for/list {[args (in-list inputs)]
                    [result (in-list outputs)]}
           (assert-event src
             (app-expr src (:: 'ACL2 'EQUAL)
               (list
                 (app-expr src fun-pf
                   (for/list {[arg (in-list args)]}
                     (qu-expr src arg)))
                 (qu-expr src result))))))
       (values env shapes assertions '())]
      [(in-theory src theory)
       (check-theory! env theory)
       (define pf (list (in-theory src (theory-pf shapes theory))))
       (values env shapes pf pf)]
      [(include-book src path opts)
       (define pf
         (list (include-book src path (verify-book-options env shapes opts))))
       (values env shapes pf pf)]
      [(primitive src name arity package symbol)
       (define sym (:: package symbol))
       (define ty (stub-type src arity))
       (define ob
         (list
           (assert-event src
             (if-expr src (qu-expr src #true)
               (qu-expr src #true)
               (app-expr src sym
                 (for/list {[i (in-range arity)]}
                   (qu-expr src (add1 i))))))))
       (values
         (bind-type name ty env)
         (bind-shape name (rune-shape src sym #false) shapes)
         ob
         (list))]
      [_
       (check-expr! env d)
       (values env shapes empty empty)])))

(define (verify-decl env d)
  (parameterize {[current-syntax-context (source-of d)]}
    (match! d
      [(decl src name type)
       (with-context-frame (format "declaration of ~a" (var->string name))
         (match! type
           [(stub-type _ _)
            (bind-type name type env)]
           [(fun-type src2 formals body measure)
            (define env+formals (bind-values formals env))
            (define rec-ty (stub-type src2 (length formals)))
            (define env+formals+rec (bind-type name rec-ty env+formals))
            (check-expr! env+formals+rec body)
            (when measure (check-expr! env+formals measure))
            (define fun-ty (fun-type src2 formals body measure))
            (bind-type name fun-ty env)]
           [(thm-type src2 formals body rules)
            (define env+formals (bind-values formals env))
            (check-expr! env+formals body)
            (when rules (check-rules! env+formals rules))
            (define thm-ty (thm-type src2 formals body rules))
            (bind-type name thm-ty env)]
           [_
            (define type-ty (verify-type env type))
            (bind-type name type-ty env)]))])))

(define (verify-term env shapes term)
  (parameterize {[current-syntax-context (source-of term)]}
    (match! term
      [(inst-term src labels names prog)
       (define-values {body-env body-shapes body-obs body-csqs}
         (verify-program env shapes prog))
       (define ty
         (inst-type src labels names
           (environment-subtract body-env env)))
       (define shape
         (inst-shape src labels names
           (shape-map-subtract body-shapes shapes)))
       (values ty shape body-obs body-csqs)]
      [(type-term src type)
       (define ty (verify-type env type))
       (values (type-type src ty) (none-shape src) empty empty)]
      [(seal-term src type term)
       (define-values {term-ty term-shape term-obs term-csqs}
         (verify-term env shapes term))
       (define type-ty (verify-type env type))
       (check-subtype! env term-ty type-ty)
       (define-values {type-shape type-csqs}
         (type-pf shapes type-ty))
       (values
         type-ty
         type-shape
         (list* (encapsulate src term-obs) type-csqs)
         type-csqs)]
      [(gen-term src formals domains body)
       (define formals-decls
         (for/list {[formal (in-list formals)]
                    [domain (in-list domains)]}
           (decl (source-of domain) formal domain)))
       (define formals-env
         (verify-environment env
           (environment formals-decls)))
       (define formals-tys
         (for/list {[formal (in-list formals)]}
           (type-of-var formals-env formal)))
       (for {[ty (in-list formals-tys)]}
         (check-instance-type! ty))
       (define-values {formals-shapes formals-csqs}
         (environment-pf shapes
           (environment-subtract formals-env env)))
       (define-values {body-ty body-shape body-obs body-csqs}
         (verify-term formals-env formals-shapes body))
       (check-instance-type! body-ty)
       (values
         (gen-type src formals formals-tys body-ty)
         (none-shape src)
         (list (encapsulate src (append formals-csqs body-obs)))
         empty)]
      [(app-term src gen args)
       (define-values {gen-ty gen-shape gen-obs gen-csqs}
         (verify-term env shapes gen))
       (check-generic-type! gen-ty)
       (check-type-arity! gen-ty (length args) #:desc "a generic")
       (define/for/lists {arg-ty-list arg-shape-list} {[arg (in-list args)]}
         (verify-addr env shapes arg))
       (define alpha (subst (gen-type-formals gen-ty) args))
       (for {[domain-ty (in-list (gen-type-domains gen-ty))]
             [arg-ty (in-list arg-ty-list)]}
         (check-subtype! env arg-ty
           (subst-type alpha domain-ty)))
       (define inst-ty
         (subst-type alpha
           (gen-type-body gen-ty)))
       (define-values {inst-shape inst-csqs}
         (type-pf shapes inst-ty))
       (values
         inst-ty
         inst-shape
         (list* (encapsulate src gen-obs) inst-csqs)
         inst-csqs)]
      [(or (deref _ _ _) (var _ _))
       (define-values {ty shape} (verify-addr env shapes term))
       (values (refine ty empty term (source-of term)) shape empty empty)])))

(define (verify-type env ty)
  (parameterize {[current-syntax-context (source-of ty)]}
    (match! ty
      [(type-type src type)
       (type-type src
         (verify-type env type))]
      [(inst-type src labels names ienv)
       (define body-env
         (verify-environment env ienv))
       (inst-type src labels names
         (environment-subtract body-env env))]
      [(gen-type src formals domains body)
       (define formals-decls
         (for/list {[formal (in-list formals)]
                    [domain (in-list domains)]}
           (decl (source-of domain) formal domain)))
       (define formals-env
         (verify-environment env
           (environment formals-decls)))
       (define formals-tys
         (for/list {[formal (in-list formals)]}
           (type-of-var formals-env formal)))
       (for {[ty (in-list formals-tys)]}
         (check-instance-type! ty))
       (define body-ty (verify-type formals-env body))
       (check-instance-type! body-ty)
       (gen-type src formals formals-tys body-ty)]
      [(refine-type src base labels addr)
       (define base-ty (verify-type env base))
       (define addr-ty (type-of-addr env addr))
       (check-type-is-refinable! base-ty labels)
       (check-subtype-at! env addr-ty labels base-ty)
       (refine base-ty labels addr src)]
      [(addr-type src addr)
       (define addr-ty (type-of-addr env addr))
       (refine addr-ty '() addr src)]
      [(or (deref _ _ _) (var _ _))
       (define type (type-of-addr env ty))
       (check-type-type! type)
       (type-type-type type)])))

(define (verify-book-options env shapes opts)
  (for/list {[o (in-list opts)]}
    (verify-book-option env shapes o)))

(define (verify-book-option env shapes o)
  (parameterize {[current-syntax-context (source-of o)]}
    (match! o
      [(book-dir src dir) (book-dir src dir)]
      [(skip-proofs-ok src ?) (skip-proofs-ok src ?)])))

(define (environment-pf shapes env0)
  (match! env0
    [(environment decls)
     (decls-pf shapes decls)]))

(define (decls-pf shapes decls)
  (for/fold/append-lists
      {[shapes shapes]}
      {csqs}
      {[d (in-list decls)]}
    (decl-pf shapes d)))

(define (decl-pf shapes d)
  (parameterize {[current-syntax-context (source-of d)]}
    (match! d
      [(decl src name (stub-type src2 arity))
       (define shape (rune-shape src name #false))
       (values
         (bind-shape name shape shapes)
         (list (defn src name (stub-term src2 arity))))]
      [(decl d.src d.name (fun-type f.src formals body measure))
       (define shape (rune-shape d.src d.name formals))
       (define shapes+formals (bind-value-shapes formals shapes))
       (define shapes+formals+rec (bind-shape d.name shape shapes+formals))
       (define fun-pf
         (list
           (defn d.src d.name
             (fun-term f.src formals
               (expr-pf shapes+formals+rec body)
               (and measure (expr-pf shapes+formals measure))
               #false))))
       (values
         (bind-shape d.name shape shapes)
         (list (skip-proofs d.src fun-pf)))]
      [(decl d.src d.name (thm-type t.src formals body rules))
       (define shape (rune-shape d.src d.name formals))
       (define shapes+formals (bind-value-shapes formals shapes))
       (define thm-pf
         (list
           (defn d.src d.name
             (thm-term t.src formals
               (expr-pf shapes+formals body)
               (and rules (rules-pf shapes+formals rules))
               #false))))
       (values
         (bind-shape d.name shape shapes)
         (list (skip-proofs d.src thm-pf)))]
      [(decl src name type)
       (define-values {type-shape type-csqs}
         (type-pf shapes type))
       (define sub
         (prefix-substitution name
           (defns-domain type-csqs)))
       (values
         (bind-shape name (subst-shape sub type-shape) shapes)
         (subst-defns sub type-csqs))])))

(define (prefix-substitution prefix suffixes)
  (subst suffixes
    (for/list {[suffix (in-list suffixes)]}
      (var (var-id prefix)
        (append
          (var-labels prefix)
          (addr-symbols suffix))))))

(define (addr-symbols addr [tail '()])
  (match! addr
    [(var id labels) (cons (syntax-e id) (append labels tail))]
    [(deref _ base label) (addr-symbols base (cons label tail))]))

(define (type-pf shapes ty)
  (parameterize {[current-syntax-context (source-of ty)]}
    (match! ty
      [(type-type src type)
       (values (none-shape src) empty)]
      [(inst-type src labels names ienv)
       (define-values {body-shapes body-csqs}
         (environment-pf shapes ienv))
       (define shape
         (inst-shape src labels names
           (shape-map-subtract body-shapes shapes)))
       (values shape body-csqs)]
      [(gen-type src formals domains body)
       (values (none-shape src) empty)]
      [(addr-type src addr)
       (values (shape-of-addr shapes addr) empty)])))

(define (check-rules! env rules)
  (for {[r (in-list rules)]}
    (check-rule! env r)))

(define (check-rule! env r)
  (parameterize {[current-syntax-context (source-of r)]}
    (match! r
      [(rewrite _ cor)
       (when cor (check-expr! env cor))]
      [(forward-chaining _ cor)
       (when cor (check-expr! env cor))]
      [(elim _ cor)
       (when cor (check-expr! env cor))]
      [(type-prescription _ cor)
       (when cor (check-expr! env cor))]
      [(linear _ cor)
       (when cor (check-expr! env cor))]
      [(definition _ cor clique)
       (when cor (check-expr! env cor))
       (check-clique! env clique)]
      [(induction _ cor pat con sch)
       (when cor (check-expr! env cor))
       (check-expr! env pat)
       (when con (check-expr! env con))
       (check-expr! env sch)])))

(define (check-clique! env c)
  (parameterize {[current-syntax-context (and c (source-of c))]}
    (match! c
      [#false #false]
      [(clique src funs)
       (for {[fun (in-list funs)]}
         (check-function-type!
           (structural-type env
             (type-of-addr env fun))))]
      [(controllers src funs controlss)
       (for {[fun (in-list funs)]
             [controls (in-list controlss)]}
         (define anon-ty
           (structural-type env
             (type-of-addr env fun)))
         (check-function-type! anon-ty)
         (check-type-arity! #:desc "a function" anon-ty (length controls)))])))

(define (check-theory! env theory)
  (parameterize {[current-syntax-context (source-of theory)]}
    (match! theory
      [(enable src runes)
       (check-runes! env runes)]
      [(disable src runes)
       (check-runes! env runes)])))

(define (check-exprs! env exprs)
  (for {[e (in-list exprs)]}
    (check-expr! env e)))

(define (check-expr! env e)
  (parameterize {[current-syntax-context (source-of e)]}
    (match! e
      [(qu-expr _ _) (void)]
      [(if-expr _ Q A E)
       (check-expr! env Q)
       (check-expr! env A)
       (check-expr! env E)]
      [(let-expr _ lhs rhs body)
       (check-exprs! env rhs)
       (check-expr! (bind-values lhs env) body)]
      [(app-expr src fun args)
       (define fun-ty (type-of-addr env fun))
       (define anon-ty (structural-type env fun-ty))
       (check-function-type! anon-ty)
       (check-type-arity! #:desc "a function" anon-ty (length args))
       (check-exprs! env args)]
      [(var _ _)
       (define var-ty (type-of-var env e))
       (check-value-type! var-ty)])))

(define (check-goals! env goals)
  (for {[g (in-list goals)]}
    (check-goal! env g)))

(define (check-goal! env g)
  (parameterize {[current-syntax-context (source-of g)]}
    (match! g
      [(goal stx name hints)
       (check-hints! env hints)])))

(define (check-hints! env hints)
  (for {[h (in-list hints)]}
    (check-hint! env h)))

(define (check-hint! env h)
  (parameterize {[current-syntax-context (source-of h)]}
    (match! h
      [(by-hint stx inst)
       (when inst (check-lemma! env inst))]
      [(use-hint stx insts)
       (check-lemmas! env insts)]
      [(in-theory-hint stx theory)
       (check-theory! env theory)]
      [(induct-hint stx scheme)
       (check-expr! env scheme)])))

(define (check-lemmas! env insts)
  (for {[i (in-list insts)]}
    (check-lemma! env i)))

(define (check-lemma! env i)
  (parameterize {[current-syntax-context (source-of i)]}
    (match! i
      [(lemma stx rune args)
       (define rune-ty (type-of-addr env rune))
       (define anon-ty (structural-type env rune-ty))
       (check-function-or-theorem-type! anon-ty)
       (check-known-formals! anon-ty)
       (check-type-arity! anon-ty (length args))
       (check-exprs! env args)])))

(define (check-runes! env runes)
  (for {[r (in-list runes)]}
    (check-rune! env r)))

(define (check-rune! env rune)
  (parameterize {[current-syntax-context (source-of rune)]}
    (define rune-ty (type-of-addr env rune))
    (check-function-or-theorem-type! (structural-type env rune-ty))))

(define (rules-pf shapes rules)
  (for/list {[r (in-list rules)]}
    (rule-pf shapes r)))

(define (rule-pf shapes r)
  (parameterize {[current-syntax-context (source-of r)]}
    (match! r
      [(rewrite src cor)
       (rewrite src
         (and cor (expr-pf shapes cor)))]
      [(forward-chaining src cor)
       (forward-chaining src
         (and cor (expr-pf shapes cor)))]
      [(elim src cor)
       (elim src
         (and cor (expr-pf shapes cor)))]
      [(type-prescription src cor)
       (type-prescription src
         (and cor (expr-pf shapes cor)))]
      [(linear src cor)
       (linear src
         (and cor (expr-pf shapes cor)))]
      [(definition src cor clique)
       (definition src
         (and cor (expr-pf shapes cor))
         (clique-pf shapes clique))]
      [(induction src cor pat con sch)
       (induction src
         (and cor (expr-pf shapes cor))
         (expr-pf shapes pat)
         (and con (expr-pf shapes con))
         (expr-pf shapes sch))])))

(define (clique-pf shapes c)
  (parameterize {[current-syntax-context (and c (source-of c))]}
    (match! c
      [#false #false]
      [(clique src funs)
       (clique src (runes-pf shapes funs))]
      [(controllers src funs controlss)
       (define fun-vars (runes-pf shapes funs))
       (controllers src fun-vars controlss)])))

(define (theory-pf shapes theory)
  (parameterize {[current-syntax-context (source-of theory)]}
    (match! theory
      [(enable src runes)
       (enable src (runes-pf shapes runes))]
      [(disable src runes)
       (disable src (runes-pf shapes runes))])))

(define (exprs-pf shapes exprs)
  (for/list {[e (in-list exprs)]}
    (expr-pf shapes e)))

(define (expr-pf shapes e)
  (parameterize {[current-syntax-context (source-of e)]}
    (match! e
      [(qu-expr src val) (qu-expr src val)]
      [(if-expr src Q A E)
       (if-expr src
         (expr-pf shapes Q)
         (expr-pf shapes A)
         (expr-pf shapes E))]
      [(let-expr src lhs rhs body)
       (let-expr src lhs
         (exprs-pf shapes rhs)
         (expr-pf (bind-value-shapes lhs shapes) body))]
      [(app-expr src fun args)
       (app-expr src (rune-pf shapes fun)
         (exprs-pf shapes args))]
      [(var _ _)
       (shape-of-var shapes e)])))

(define (goals-pf shapes goals)
  (for/list {[g (in-list goals)]}
    (goal-pf shapes g)))

(define (goal-pf shapes g)
  (parameterize {[current-syntax-context (source-of g)]}
    (match! g
      [(goal stx name hints)
       (goal stx name (hints-pf shapes hints))])))

(define (hints-pf shapes hints)
  (for/list {[h (in-list hints)]}
    (hint-pf shapes h)))

(define (hint-pf shapes h)
  (parameterize {[current-syntax-context (source-of h)]}
    (match! h
      [(by-hint stx inst)
       (by-hint stx
         (and inst (lemma-pf shapes inst)))]
      [(use-hint stx insts)
       (use-hint stx
         (lemmas-pf shapes insts))]
      [(in-theory-hint stx theory)
       (in-theory-hint stx
         (theory-pf shapes theory))]
      [(induct-hint stx scheme)
       (induct-hint stx
         (expr-pf shapes scheme))])))

(define (lemmas-pf shapes insts)
  (for/list {[i (in-list insts)]}
    (lemma-pf shapes i)))

(define (lemma-pf shapes i)
  (parameterize {[current-syntax-context (source-of i)]}
    (match! i
      [(lemma stx rune args)
       (define shape (shape-of-addr shapes rune))
       (define formals (rune-shape-formals shape))
       (lemma stx (rune-shape-name shape)
         (for/list {[formal (in-list formals)]
                    [actual (in-list args)]}
           (named-actual (source-of actual) formal
             (expr-pf shapes actual))))])))

(define (runes-pf shapes runes)
  (for/list {[r (in-list runes)]}
    (rune-pf shapes r)))

(define (rune-pf shapes rune)
  (parameterize {[current-syntax-context (source-of rune)]}
    (rune-shape-name (shape-of-addr shapes rune))))

(define (check-type-is-refinable! ty labels)
  (parameterize {[current-syntax-context (source-of ty)]}
    (match! labels
      ['() (void)]
      [(cons label labels)
       (check-instance-type! ty)
       (check-type-member! ty label)
       (check-type-is-refinable! (raw-type-of-member ty label) labels)])))

(define (check-subtype-at! env sub-ty labels0 addr-ty)
  (parameterize {[current-syntax-context (source-of addr-ty)]}
    (match! labels0
      ['() (check-subtype! env sub-ty addr-ty)]
      [(cons label0 labels0)
       (match! addr-ty
         [(inst-type src labels names ienv)
          (define name0
            (dict-ref (map cons labels names) label0
              (lambda {}
                (context-error src
                  "cannot find member ~a in refined type with members: ~a"
                  (label->string label0)
                  (labels->string labels)))))
          (define ty (type-of-var ienv name0))
          (define env2 (environment-union env ienv))
          (with-context-frame (format "member ~a" (label->string label0))
            (check-subtype-at! env2 sub-ty labels0 ty))])])))

(define (check-addr-type! ty)
  (parameterize {[current-syntax-context (source-of ty)]}
    (unless (addr-type? ty)
      (mismatch-error
        #:context (source-of ty)
        #:expected "a reference"
        #:actual (type->string ty)))))

(define (check-type-type! ty)
  (parameterize {[current-syntax-context (source-of ty)]}
    (unless (type-type? ty)
      (mismatch-error
        #:context (source-of ty)
        #:expected "a type"
        #:actual (type->string ty)))))

(define (check-value-type! ty)
  (parameterize {[current-syntax-context (source-of ty)]}
    (unless (value-type? ty)
      (mismatch-error
        #:context (source-of ty)
        #:expected "a value"
        #:actual (type->string ty)))))

(define (check-function-type! ty)
  (parameterize {[current-syntax-context (source-of ty)]}
    (unless (function-type? ty)
      (mismatch-error
        #:context (source-of ty)
        #:expected "a function"
        #:actual (type->string ty)))))

(define (check-theorem-type! ty)
  (parameterize {[current-syntax-context (source-of ty)]}
    (unless (thm-type? ty)
      (mismatch-error
        #:context (source-of ty)
        #:expected "a theorem"
        #:actual (type->string ty)))))

(define (check-abstract-function-type! ty)
  (parameterize {[current-syntax-context (source-of ty)]}
    (unless (stub-type? ty)
      (mismatch-error
        #:context (source-of ty)
        #:expected "an abstract function"
        #:actual (type->string ty)))))

(define (check-concrete-function-type! ty)
  (parameterize {[current-syntax-context (source-of ty)]}
    (unless (fun-type? ty)
      (mismatch-error
        #:context (source-of ty)
        #:expected "a concrete function"
        #:actual (type->string ty)))))

(define (check-function-or-theorem-type! ty)
  (parameterize {[current-syntax-context (source-of ty)]}
    (unless (function-or-theorem-type? ty)
      (mismatch-error
        #:context (source-of ty)
        #:expected "a function or theorem"
        #:actual (type->string ty)))))

(define (check-instance-type! ty)
  (parameterize {[current-syntax-context (source-of ty)]}
    (unless (inst-type? ty)
      (mismatch-error
        #:context (source-of ty)
        #:expected "an instance"
        #:actual (type->string ty)))))

(define (check-instance-members! ty labels)
  (parameterize {[current-syntax-context (source-of ty)]}
    (unless (type-has-members? ty labels)
      (mismatch-error
        #:context (source-of ty)
        #:expected (format "an instance with members ~a"
                     (labels->string labels))
        #:actual (type->string ty)))))

(define (check-generic-type! ty)
  (parameterize {[current-syntax-context (source-of ty)]}
    (unless (gen-type? ty)
      (mismatch-error
        #:context (source-of ty)
        #:expected "a generic"
        #:actual (type->string ty)))))

(define (check-known-formals! ty
          #:desc [desc "a function or theorem"])
  (parameterize {[current-syntax-context (source-of ty)]}
    (unless (type-formals ty)
      (mismatch-error
        #:context (source-of ty)
        #:expected (format "~a with known argument names" desc)
        #:actual (type->string ty)))))

(define (check-type-arity! ty expected
          #:desc [desc "a function or theorem"])
  (parameterize {[current-syntax-context (source-of ty)]}
    (define actual (type-arity ty))
    (define (full-desc)
      (format "~a of ~a" desc (count->phrase expected "argument")))
    (unless actual
      (mismatch-error
        #:context (source-of ty)
        #:message "cannot determine number of arguments"
        #:expected (full-desc)
        #:actual (type->string ty)))
    (unless (= actual expected)
      (mismatch-error
        #:context (source-of ty)
        #:message "wrong number of arguments"
        #:expected (full-desc)
        #:actual (type->string ty)))))

(define (check-type-member! ty label)
  (parameterize {[current-syntax-context (source-of ty)]}
    (unless (type-has-member? ty label)
      (mismatch-error
        #:context (source-of ty)
        #:expected (format "an instance with member label ~a"
                     (label->string label))
        #:actual (type->string ty)))))

(define (check-subtype! env sub ty)
  (parameterize {[current-syntax-context (source-of sub)]}
    (match! ty
      [(value-type _) (check-value-type! sub)]
      [(stub-type _ arity)
       (define anon-ty (structural-type env sub))
       (check-function-type! anon-ty)
       (check-type-arity! anon-ty arity #:desc "a function")]
      [(fun-type _ _ _ _) (check-same-type! env (structural-type env sub) ty)]
      [(thm-type _ _ _ _) (check-same-type! env (structural-type env sub) ty)]
      [(type-type _ _) (check-same-type! env sub ty)]
      [(addr-type _ _) (check-same-type! env sub ty)]
      [(gen-type _ formals domains body)
       (check-generic-type! sub)
       (check-type-arity! sub (length formals) #:desc "a generic")
       (match! sub
         [(gen-type _ sub-formals sub-domains sub-body)
          (for {[formal (in-list formals)]
                [dom (in-list domains)]
                [sub-dom (in-list sub-domains)]}
            (with-context-frame (format "generic argument ~a"
                                  (var->string formal))
              (check-subtype! env dom sub-dom)))
          (define alpha (subst formals sub-formals))
          (define env2 (bind-types sub-formals sub-domains env))
          (check-subtype! env2 sub-body (subst-type alpha body))])]
      [(inst-type _ labels names ienv)
       (check-instance-type! sub)
       (check-instance-members! sub labels)
       (match! sub
         [(inst-type _ sub-labels sub-names sub-ienv)
          (define sub-label~>name
            (make-immutable-hasheq (map cons sub-labels sub-names)))
          (define images
            (for/list {[label (in-list labels)]
                       [name (in-list names)]}
              (dict-ref sub-label~>name label
                (lambda {}
                  (context-error (source-of name)
                    "cannot find member ~a in instance subtype with members ~a"
                    (label->string label)
                    (labels->string sub-labels))))))
          (define alpha (subst names images))
          (define env2 (environment-union env sub-ienv))
          (match! ienv
            [(environment decls)
             (define name~>label
               (make-immutable-hash (map cons names labels)))
             (for {[d (in-list decls)]}
               (match! d
                 [(decl _ name ity)
                  (define label
                    (dict-ref name~>label name
                      (lambda {}
                        (context-error (source-of name)
                          "cannot find member ~a in instance type defining ~a"
                          (var->string name)
                          (vars->string names)))))
                  (with-context-frame (format "member ~a" (label->string label))
                    (define sub-name (subst-var alpha name))
                    (define sub-ity (type-of-var env2 sub-name))
                    (check-subtype! env2 sub-ity
                      (subst-type alpha ity)))]))])])])))

(define (check-same-type! env actual expected)
  (parameterize {[current-syntax-context (source-of actual)]}
    (match! expected
      [(value-type _) (check-value-type! actual)]
      [(stub-type _ arity)
       (check-function-type! actual)
       (check-type-arity! actual arity #:desc "a function")
       (check-abstract-function-type! actual)]
      [(fun-type _ formals body measure)
       (check-function-type! actual)
       (check-type-arity! actual (length formals) #:desc "a function")
       (check-concrete-function-type! actual)
       (match! actual
         [(fun-type _ formals2 body2 measure2)
          (define alpha (subst formals formals2))
          (define env+formals (bind-values formals2 env))
          (unless (same-expr? env+formals body2
                    (subst-expr alpha body))
            (type-error
              #:expected expected
              #:actual actual
              #:message "function body expression mismatch"))
          (unless (same-optional-expr? env+formals measure2
                    (and measure (subst-expr alpha measure)))
            (type-error
              #:actual actual
              #:expected expected
              #:message "function measure expression mismatch"))])]
      [(thm-type _ formals body rules)
       (check-theorem-type! actual)
       (check-type-arity! actual (length formals) #:desc "a theorem")
       (match! actual
         [(thm-type _ formals2 body2 rules2)
          (define alpha (subst formals formals2))
          (define env+formals (bind-values formals2 env))
          (unless (same-expr? env+formals body2
                    (subst-expr alpha body))
            (type-error
              #:actual actual
              #:expected expected
              #:message "theorem body expression mismatch"))
          (unless (same-optional-rules? env+formals rules2
                    (and rules (subst-rules alpha rules)))
            (type-error
              #:actual actual
              #:expected expected
              #:message "theorem rule classes mismatch"))])]
      [(addr-type _ a)
       (check-addr-type! actual)
       (match! actual
         [(addr-type _ b)
          (unless (same-addr? env a b)
            (type-error #:actual actual #:expected expected))])]
      [(type-type _ a)
       (check-type-type! actual)
       (match! actual
         [(type-type _ b)
          (check-same-type! env b a)])]
      [(gen-type _ formals domains body)
       (check-generic-type! actual)
       (check-type-arity! actual (length formals) #:desc "a generic")
       (match! actual
         [(gen-type _ formals2 domains2 body2)
          (for {[formal (in-list formals)]
                [dom (in-list domains)]
                [dom2 (in-list domains2)]}
            (with-context-frame (format "generic argument ~a"
                                  (var->string formal))
              (check-same-type! env dom dom2)))
          (define alpha (subst formals formals2))
          (define env2 (bind-types formals2 domains2 env))
          (check-same-type! env2 body2 (subst-type alpha body))])]
      [(inst-type _ labels names ienv)
       (check-instance-type! actual)
       (check-instance-members! actual labels)
       (match! actual
         [(inst-type _ labels2 names2 ienv2)
          (check-instance-members! expected labels2)
          (define dict2 (make-immutable-hasheq (map cons labels2 names2)))
          (define images
            (for/list {[label (in-list labels)]
                       [name (in-list names)]}
              (dict-ref dict2 label
                (lambda {}
                  (context-error (source-of name)
                    "cannot find member ~a in instance sibling with members ~a"
                    (label->string label)
                    (labels->string label))))))
          (define alpha (subst names images))
          (define env2 (environment-union env ienv2))
          (match! ienv
            [(environment decls)
             (define name~>label
               (make-immutable-hash (map cons names labels)))
             (for {[d (in-list decls)]}
               (match! d
                 [(decl _ name ity)
                  (define label
                    (dict-ref name~>label name
                      (lambda {}
                        (context-error (source-of name)
                          "cannot find member ~a in instance type defining ~a"
                          (var->string name)
                          (vars->string names)))))
                  (define name2 (subst-var alpha name))
                  (define ity2 (type-of-var env2 name2))
                  (with-context-frame (format "member ~a" (label->string label))
                    (check-same-type! env2
                      (subst-type alpha ity)
                      ity2))]))])])])))

(define (same-optional-rules? env one two)
  (match*! {one two}
    [{#false #false} #true]
    [{#false _} #false]
    [{_ #false} #false]
    [{_ _} (same-rules? env one two)]))

(define (same-rules? env ones twos)
  (and (= (length ones) (length twos))
    (for/and {[one (in-list ones)] [two (in-list twos)]}
      (same-rule? env one two))))

(define (same-rule? env one two)
  (parameterize {[current-syntax-context (source-of one)]}
    (match*! {one two}
      [{(rewrite _ corollary1)
        (rewrite _ corollary2)}
       (same-optional-expr? env corollary1 corollary2)]
      [{(forward-chaining _ corollary1)
        (forward-chaining _ corollary2)}
       (same-optional-expr? env corollary1 corollary2)]
      [{(elim _ corollary1)
        (elim _ corollary2)}
       (same-optional-expr? env corollary1 corollary2)]
      [{(type-prescription _ corollary1)
        (type-prescription _ corollary2)}
       (same-optional-expr? env corollary1 corollary2)]
      [{(linear _ corollary1)
        (linear _ corollary2)}
       (same-optional-expr? env corollary1 corollary2)]
      [{(definition _ corollary1 clique1)
        (definition _ corollary2 clique2)}
       (and
         (same-optional-expr? env corollary1 corollary2)
         (same-optional-clique? env clique1 clique2))]
      [{(induction _ corollary1 pattern1 condition1 scheme1)
        (induction _ corollary2 pattern2 condition2 scheme2)}
       (and
         (same-optional-expr? env corollary1 corollary2)
         (same-expr? env pattern1 pattern2)
         (same-optional-expr? env condition1 condition2)
         (same-expr? env scheme1 scheme2))]
      [{_ _} #false])))

(define (same-optional-clique? env one two)
  (match*! {one two}
    [{#false #false} #true]
    [{(clique _ funs1)
      (clique _ funs2)}
     (same-addrs? env funs1 funs2)]
    [{(controllers _ funs1 flags1)
      (controllers _ funs2 flags2)}
     (and
       (same-addrs? env funs1 funs2)
       (equal? flags1 flags2))]
    [{_ _} #false]))

(define (same-exprs? env ones twos)
  (and (= (length ones) (length twos))
    (for/and {[one (in-list ones)] [two (in-list twos)]}
      (same-expr? env one two))))

(define (same-optional-expr? env one two)
  (match*! {one two}
    [{#false #false} #true]
    [{#false _} #false]
    [{_ #false} #false]
    [{_ _} (same-expr? env one two)]))

(define (same-expr? env one two)
  (parameterize {[current-syntax-context (source-of one)]}
    (match*! {one two}
      [{(qu-expr _ value1)
        (qu-expr _ value2)}
       (equal? value1 value2)]
      [{(if-expr _ test1 then1 else1)
        (if-expr _ test2 then2 else2)}
       (and
         (same-expr? env test1 test2)
         (same-expr? env then1 then2)
         (same-expr? env else1 else2))]
      [{(let-expr _ lhs1 rhs1 body1)
        (let-expr _ lhs2 rhs2 body2)}
       (and (same-exprs? env rhs1 rhs2)
         (same-expr? (bind-values lhs1 env) body1
           (subst-expr (subst lhs2 lhs1) body2)))]
      [{(app-expr _ fun1 args1)
        (app-expr _ fun2 args2)}
       (and (same-addr? env fun1 fun2)
         (same-exprs? env args1 args2))]
      [{(var _ _) (var _ _)}
       (same-addr? env one two)]
      [{_ _} #false])))

(define (same-addrs? env ones twos)
  (and (= (length ones) (length twos))
    (for/and {[one (in-list ones)] [two (in-list twos)]}
      (same-addr? env one two))))

(define (type-error
          #:message [message #false]
          #:actual actual-ty
          #:expected expected-ty
          #:context [context actual-ty])
  (mismatch-error
    #:message message
    #:actual (type->string actual-ty)
    #:expected (type->string expected-ty)
    #:context (and context (source-of context))))

(define (mismatch-error
          #:message [message #false]
          #:actual actual
          #:expected expected
          #:context context)
  (context-error context
    "~aexpected ~a; but got ~a instead"
    (if message (format "~a: " message) "")
    expected
    actual))

(define (context-error context fmt . args)
  (syntax-error context
    "~a~a~a"
    (source-location->prefix context)
    (apply format fmt args)
    (context-stack->string (current-context-stack))))

(define (context-stack->string st)
  (apply string-append
    (map context-frame->string
      (reverse st))))

(define (context-frame->string fr)
  (format "\n in: ~a" (fr)))

(define (type->string ty)
  (match! ty
    [(value-type _) "a value"]
    [(stub-type _ arity)
     (format "an abstract function of ~a"
       (count->phrase arity "argument"))]
    [(fun-type _ formals _ _)
     (format "a concrete function of ~a"
       (count->phrase (length formals) "argument"))]
    [(thm-type _ formals _ _)
     (format "a theorem of ~a"
       (count->phrase (length formals) "argument"))]
    [(inst-type _ labels _ _)
     (format "an instance with labels ~a"
       (labels->string labels))]
    [(gen-type _ formals _ _)
     (format "a generic of ~a"
       (count->phrase (length formals) "argument"))]
    [(addr-type _ addr)
     (format "a reference to ~a"
       (addr->string addr))]
    [(type-type _ type)
     (format "the type of ~a"
       (type->string type))]))

(define (type-has-members? ty members)
  (match! ty
    [(inst-type _ labels _ _)
     (define label-set (list->seteq labels))
     (for/and {[m (in-list members)]}
       (set-member? label-set m))]
    [_ #false]))

(define (type-has-member? ty label)
  (type-has-members? ty (list label)))

(define (function-type? ty)
  (match! ty
    [(stub-type _ _) #true]
    [(fun-type _ _ _ _) #true]
    [_ #false]))

(define (function-or-theorem-type? ty)
  (match! ty
    [(stub-type _ _) #true]
    [(fun-type _ _ _ _) #true]
    [(thm-type _ _ _ _) #true]
    [_ #false]))

(define (type-formals ty)
  (match! ty
    [(fun-type _ formals _ _) formals]
    [(thm-type _ formals _ _) formals]
    [(gen-type _ formals _ _) formals]
    [_ #false]))

(define (type-arity ty)
  (match! ty
    [(stub-type _ arity) arity]
    [(fun-type _ formals _ _) (length formals)]
    [(thm-type _ formals _ _) (length formals)]
    [(gen-type _ formals _ _) (length formals)]
    [_ #false]))

(define (bind-shapes vs shape-list shapes)
  (for/fold {[shapes shapes]} {[v (in-list vs)] [shape (in-list shape-list)]}
    (bind-shape v shape shapes)))

(define (bind-shape v s shapes)
  (shape-map-set shapes v s))

(define (bind-types vs tys env)
  (for/fold {[env env]} {[v (in-list vs)] [ty (in-list tys)]}
    (bind-type v ty env)))

(define (bind-type v ty env)
  (environment-set env v ty))

(define (bind-values vs env)
  (for/fold {[env env]} {[v (in-list vs)]}
    (bind-type v (value-type (source-of v)) env)))

(define (bind-value-shapes vs shapes)
  (for/fold {[shapes shapes]} {[v (in-list vs)]}
    (bind-shape v v shapes)))

(define (refine ty labels0 addr src0)
  (parameterize {[current-syntax-context (source-of ty)]}
    (match! ty
      [(stub-type src _) (match! labels0 ['() (addr-type src addr)])]
      [(fun-type src _ _ _) (match! labels0 ['() (addr-type src addr)])]
      [(thm-type src _ _ _) (match! labels0 ['() (addr-type src addr)])]
      [(inst-type src labels names env)
       (match! labels0
         ['()
          (define name~>label
            (make-immutable-hash (map cons names labels)))
          (inst-type src labels names
            (environment-map env
              (lambda {d}
                (match! d
                  [(decl src name type)
                   (decl src name
                     (refine type '()
                       (deref src0 addr
                         (dict-ref name~>label name
                           (lambda {}
                             (context-error name
                               "cannot find member named ~a among: ~a"
                               (var->string name)
                               (vars->string names)))))
                       src0))]))))]
         [(cons label0 labels0)
          (define name
            (dict-ref (map cons labels names) label0
              (lambda {}
                (context-error))))
          (inst-type src labels names
            (environment-update env name
              (lambda {type}
                (refine type labels0 addr src0))
              (lambda {}
                (context-error src
                  "cannot find member ~a in refined instance type among: ~a"
                  (label->string label0)
                  (labels->string labels)))))])]
      [_ ty])))

(define (verify-addr env shapes addr)
  (parameterize {[current-syntax-context (source-of addr)]}
    (values
      (type-of-addr env addr)
      (shape-of-addr shapes addr))))

(define (type-of-addr env addr)
  (parameterize {[current-syntax-context (source-of addr)]}
    (match! addr
      [(var _ _) (type-of-var env addr)]
      [(deref src comp label)
       (type-of-member comp (type-of-addr env comp) label src)])))

(define (shape-of-addr shapes addr)
  (parameterize {[current-syntax-context (source-of addr)]}
    (match! addr
      [(var _ _) (shape-of-var shapes addr)]
      [(deref _ comp label)
       (shape-of-member (shape-of-addr shapes comp) label)])))

(define (type-of-var env v)
  (parameterize {[current-syntax-context (source-of v)]}
    (environment-ref env v
      (lambda {}
        (context-error (source-of v)
          "cannot find type of ~a among: ~a"
          (var->string v)
          (vars->string (environment-keys env)))))))

(define (shape-of-var shapes v)
  (parameterize {[current-syntax-context (source-of v)]}
    (shape-map-ref shapes v
      (lambda {}
        (context-error (source-of v)
          "cannot find shape of ~a among: ~a"
          (var->string v)
          (vars->string (shape-map-keys shapes)))))))

(define (type-of-member addr ty label src)
  (parameterize {[current-syntax-context (source-of ty)]}
    (match! ty
      [(inst-type _ labels names env)
       (subst-type
         (subst names
           (for/list {[label (in-list labels)]}
             (deref src addr label)))
         (raw-type-of-member ty label))])))

(define (raw-type-of-member ty label)
  (parameterize {[current-syntax-context (source-of ty)]}
    (match! ty
      [(inst-type _ labels names env)
       (define name
         (dict-ref (map cons labels names) label
           (lambda {}
             (context-error (source-of ty)
               "cannot find name of member ~a among: ~a"
               (label->string label)
               (labels->string labels)))))
       (type-of-var env name)])))

(define (shape-of-member shape label)
  (parameterize {[current-syntax-context (source-of shape)]}
    (match! shape
      [(inst-shape _ labels names shapes)
       (define name
         (dict-ref (map cons labels names) label
           (lambda {}
             (context-error (source-of shape)
               "cannot find shape of member ~a among: ~a"
               (label->string label)
               (labels->string labels)))))
       (shape-of-var shapes name)])))

(define (structural-type env ty)
  (parameterize {[current-syntax-context (source-of ty)]}
    (match! ty
      [(addr-type _ addr)
       (structural-type env
         (type-of-addr env addr))]
      [_ ty])))

(define (same-addr? env one two)
  (same-canonical-addr?
    (canonical-addr env one)
    (canonical-addr env two)))

(define (same-canonical-addr? one two)
  (match*! {one two}
    [{(var _ _) (var _ _)} (equal? one two)]
    [{(deref _ comp1 label1)
      (deref _ comp2 label2)}
     (and (eq? label1 label2)
       (same-canonical-addr? comp1 comp2))]
    [{_ _} #false]))

(define (canonical-addr env addr)
  (match! (type-of-addr env addr)
    [(addr-type _ addr2) (canonical-addr env addr2)]
    [_ addr]))

(define (defns-domain defns)
  (for/append {[d (in-list defns)]}
    (defn-domain d)))

(define (defn-domain d)
  (match! d
    [(defn _ name _) (list name)]
    [(in-theory _ _) '()]
    [(include-book _ _ _) '()]
    [(primitive _ _ _ _ _) '()]
    [(encapsulate _ defns) (defns-domain defns)]
    [(skip-proofs _ defns) (defns-domain defns)]
    [(assert-event _ _) '()]))
