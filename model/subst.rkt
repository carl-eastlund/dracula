#lang mischief

(provide
  subst-shape-map
  subst-shape
  subst-program
  subst-environment
  subst-defns
  subst-decls
  subst-defn
  subst-decl
  subst-term
  subst-type
  subst-rules
  subst-rule
  subst-expr
  subst-exprs
  subst-var
  subst)

(require
  dracula/model/data)

(define (subst vars things)
  (for/hash {[v (in-list vars)]
             [t (in-list things)]}
    (values v t)))

(define (subst-program dict prog)
  (match! prog
    [(program defns)
     (program (subst-defns dict defns))]))

(define (subst-environment dict env)
  (match! env
    [(environment decls)
     (environment (subst-decls dict decls))]))

(define (subst-defns dict defns)
  (for/list {[d (in-list defns)]}
    (subst-defn dict d)))

(define (subst-decls dict decls)
  (for/list {[d (in-list decls)]}
    (subst-decl dict d)))

(define (subst-defn dict d)
  (match! d
    [(defn src name term)
     (defn src
       (subst-var dict name)
       (subst-term dict term))]
    [(in-theory src theory)
     (in-theory src
       (subst-theory dict theory))]
    [(include-book src path options)
     (include-book src path options)]
    [(primitive src name arity package symbol)
     (primitive src
       (subst-var dict name)
       arity
       package
       symbol)]
    [(encapsulate src defns)
     (encapsulate src
       (subst-defns dict defns))]
    [(skip-proofs src defns)
     (skip-proofs src
       (subst-defns dict defns))]
    [(assert-event src body)
     (assert-event src
       (subst-expr dict body))]
    [(validate src fun inputs outputs)
     (validate src
       (subst-addr dict fun)
       inputs
       outputs)]))

(define (subst-decl dict d)
  (match! d
    [(decl src name type)
     (decl src
       (subst-var dict name)
       (subst-type dict type))]))

(define (subst-terms dict ts)
  (for/list {[t (in-list ts)]}
    (subst-terms dict ts)))

(define (subst-term dict t)
  (match! t
    [(stub-term src arity)
     (stub-term src arity)]
    [(fun-term src formals body measure goals)
     (fun-term src
       formals
       (subst-expr dict body)
       (and measure (subst-expr dict measure))
       (and goals (subst-goals dict goals)))]
    [(thm-term src formals body rules goals)
     (thm-term src
       formals
       (subst-expr dict body)
       (and rules (subst-rules dict rules))
       (and goals (subst-goals dict goals)))]
    [(inst-term src labels names prog)
     (inst-term src labels
       (subst-vars dict names)
       (subst-program dict prog))]
    [(seal-term src type term)
     (seal-term src
       (subst-type dict type)
       (subst-term dict term))]
    [(gen-term src formals domains body)
     (gen-term src formals
       (subst-types dict domains)
       (subst-term dict body))]
    [(app-term src gen args)
     (app-term src
       (subst-term dict gen)
       (subst-addrs dict args))]))

(define (subst-types dict ts)
  (for/list {[t (in-list ts)]}
    (subst-type dict t)))

(define (subst-type dict t)
  (match! t
    [(stub-type src arity)
     (stub-type src arity)]
    [(fun-type src formals body measure)
     (fun-type src
       formals
       (subst-expr dict body)
       (and measure (subst-expr dict measure)))]
    [(thm-type src formals body rules)
     (thm-type src
       formals
       (subst-expr dict body)
       (and rules (subst-rules dict rules)))]
    [(inst-type src labels names env)
     (inst-type src labels
       (subst-vars dict names)
       (subst-environment dict env))]
    [(gen-type src formals domains body)
     (gen-type src formals
       (subst-types dict domains)
       (subst-type dict body))]
    [(addr-type src addr)
     (addr-type src
       (subst-addr dict addr))]
    [(type-type src type)
     (type-type src
       (subst-type dict type))]))

(define (subst-theory dict theory)
  (match! theory
    [(enable src runes)
     (enable src
       (subst-addrs dict runes))]
    [(disable src runes)
     (disable src
       (subst-addrs dict runes))]))

(define (subst-rules dict rules)
  (for/list {[r (in-list rules)]}
    (subst-rule dict r)))

(define (subst-rule dict r)
  (match! r
    [(rewrite src corollary)
     (rewrite src
       (and corollary (subst-expr dict corollary)))]
    [(forward-chaining src corollary)
     (forward-chaining src
       (and corollary (subst-expr dict corollary)))]
    [(elim src corollary)
     (elim src
       (and corollary (subst-expr dict corollary)))]
    [(type-prescription src corollary)
     (type-prescription src
       (and corollary (subst-expr dict corollary)))]
    [(linear src corollary)
     (linear src
       (and corollary (subst-expr dict corollary)))]
    [(definition src corollary clique)
     (definition src
       (and corollary (subst-expr dict corollary))
       (and clique (subst-clique dict clique)))]
    [(induction src corollary pattern condition scheme)
     (induction src
       (and corollary (subst-expr dict corollary))
       (subst-expr dict pattern)
       (and condition (subst-expr dict condition))
       (subst-expr dict scheme))]))

(define (subst-clique dict c)
  (match! c
    [(clique src funs)
     (clique src
       (subst-addrs dict funs))]
    [(controllers src funs flags)
     (controllers src
       (subst-addrs dict funs)
       flags)]))

(define (subst-goals dict goals)
  (for/list {[g (in-list goals)]}
    (subst-goal dict g)))

(define (subst-goal dict g)
  (match! g
    [(goal src name hints)
     (goal src name
       (subst-hints dict hints))]))

(define (subst-hints dict hints)
  (for/list {[h (in-list hints)]}
    (subst-hint dict h)))

(define (subst-hint dict h)
  (match! h
    [(by-hint src lemma)
     (by-hint src
       (and lemma (subst-lemma dict lemma)))]
    [(use-hint src lemmas)
     (use-hint src
       (subst-lemmas dict lemmas))]
    [(in-theory-hint src theory)
     (in-theory-hint src
       (subst-theory dict theory))]
    [(induct-hint src scheme)
     (induct-hint src
       (subst-expr dict scheme))]))

(define (subst-lemmas dict lemmas)
  (for/list {[l (in-list lemmas)]}
    (subst-lemma dict l)))

(define (subst-lemma dict l)
  (match! l
    [(lemma src rune args)
     (lemma src
       (subst-addr dict rune)
       (subst-args dict args))]))

(define (subst-args dict args)
  (for/list {[arg (in-list args)]}
    (subst-arg dict arg)))

(define (subst-arg dict arg)
  (match! arg
    [(named-actual src name actual)
     (named-actual src name
       (subst-expr dict actual))]
    [_ (subst-expr dict arg)]))

(define (subst-shape-map dict shape)
  (for/dict (empty-shape-map)
      {[{name shape} (in-dict shape)]}
    (values
      (subst-var dict name)
      (subst-shape dict shape))))

(define (subst-shape-list dict shape-list)
  (for/list {[s (in-list shape-list)]}
    (subst-shape dict s)))

(define (subst-shape dict shape)
  (match! shape
    [(inst-shape src labels names shapes)
     (inst-shape src labels
       (subst-vars dict names)
       (subst-shape-map dict shapes))]
    [(rune-shape src name formals)
     (rune-shape src
       (subst-var dict name)
       formals)]
    [(none-shape src) (none-shape src)]
    [(or (var _ _) (:: _ _)) (subst-var dict shape)]))

(define (subst-exprs dict es)
  (for/list {[e (in-list es)]}
    (subst-expr dict e)))

(define (subst-expr dict e)
  (match! e
    [(qu-expr _ _) e]
    [(if-expr src test then else)
     (if-expr src
       (subst-expr dict test)
       (subst-expr dict then)
       (subst-expr dict else))]
    [(let-expr src lhs rhs body)
     (let-expr src lhs
       (subst-exprs dict rhs)
       (subst-expr dict body))]
    [(app-expr src fun args)
     (app-expr src
       (subst-addr dict fun)
       (subst-exprs dict args))]
    [(var _ _) (subst-var dict e)]))

(define (subst-addrs dict addrs)
  (for/list {[addr (in-list addrs)]}
    (subst-addr dict addr)))

(define (subst-addr dict addr)
  (match! addr
    [(:: _ _) addr]
    [(var _ _) (subst-var dict addr)]
    [(deref src comp label)
     (deref src
       (subst-addr dict comp)
       label)]))

(define (subst-vars dict vars)
  (for/list {[v (in-list vars)]}
    (subst-var dict v)))

(define (subst-var dict v)
  (dict-ref dict v v))
